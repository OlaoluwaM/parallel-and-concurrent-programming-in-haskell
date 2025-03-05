{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Concurrent.NetworkServers.ChatServer where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, TChan, TVar, atomically, cloneTChan, modifyTVar, newTChan, newTChanIO, newTVarIO, orElse, readTChan, readTVar, writeTChan)
import Control.Exception (bracket, getMaskingState, onException)
import Control.Lens (over)
import Control.Lens.TH
import Control.Monad (forever, unless, void)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.Either.Extra (fromEither)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.String (IsString (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Encoding qualified as T
import Debug.Trace
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

newtype Username = Username {username :: Text}
    deriving newtype (Show, Eq, Hashable)

data User = User {name :: Username, dmChan :: TChan String, channelChan :: TChan String}

newtype ChannelName = ChannelName {channelName :: Text}
    deriving newtype (Hashable, Eq, Show)

type ChannelMembership = HashMap Username User

data Channel = Channel
    { title :: ChannelName
    , {-
          `TVar (HashMap Text (TVar User))` isn't particularly useful here since structural changes to the HashMap will be common and cause performance to be similar to a TVar (HashMap Text User) representation since all threads that seek to modify `TVar User` would need to read `TVar HashMap` first. So when a new user joins the chat, that will affect the activity of all existing users

          Actually, this doesn't *have* to even be wrapped in a `TVar` or `IORef`
      -}
      members :: ChannelMembership
    , broadcastChan :: TChan String
    }

{-
    Having the type be `TVar Channel` here seems to only be useful when you have multiple channels. This would make it such that, as long as there are no structural changes to the `HashMap` itself (inserts or deletes), threads speaking to one channel won't be affected by changes to another. However, since right now we only have one channel, all threads will be speaking/accessing this channel thus making `TVar (HashMap k (TVar Channel)) the same in terms of performance to `TVar (HashMa k v)` with more indirection
-}
type ChannelRegistry = HashMap ChannelName Channel
newtype ChatServerState = ChatServerState {channels :: TVar ChannelRegistry}

$(makeLensesFor [("title", "titleL"), ("members", "membersL"), ("broadcastChan", "broadcastChanL")] ''Channel)
$(makeLensesFor [("channels", "channelsL")] ''ChatServerState)

defaultChannelName :: ChannelName
defaultChannelName = ChannelName "default-channel"

serviceName :: Text
serviceName = "haskell-irc"

main :: IO ()
main = do
    broadcastChan <- newTChanIO

    let defaultChannelMembers = HashMap.empty
    let defaultChannel = Channel{title = defaultChannelName, members = defaultChannelMembers, broadcastChan}

    channels <- newTVarIO (HashMap.singleton defaultChannelName defaultChannel)

    let serverState = ChatServerState channels

    runTCPServer Nothing "4728" (handleConnection serverState)

broadcastInServer :: TChan String -> IO ()
broadcastInServer broadcastTChan = forever $ atomically (readTChan broadcastTChan) >>= putStr

handleConnection :: ChatServerState -> Socket -> IO ()
handleConnection serverStateRef s = do
    -- bracket (registerUser s serverStateRef) (\u -> putStrLn "aa" >> unRegisterUser serverStateRef u) (handleSession s serverStateRef)
    bracket (registerUser s serverStateRef) (unRegisterUser serverStateRef) (const (handleSession s serverStateRef))

registerUser :: Socket -> ChatServerState -> IO User
registerUser s chatServerStateVar = do
    sendAll s [i|Welcome to the #{serviceName}! Enter what you'd like to be called in the chat: |]
    loop
  where
    loop :: IO User
    loop = do
        rawUserResp <- recv s 1024
        let parsedUserResp = tstrip . decodeUtf8Lenient $ rawUserResp

        (defaultChannel, membersList) <- atomically do
            defaultChannel <- getDefaultChannel chatServerStateVar
            pure (defaultChannel, HashMap.keys defaultChannel.members)

        case isInvalidUsername membersList parsedUserResp of
            (Left errMsg) -> do
                sendAll s (T.encodeUtf8 errMsg)
                loop
            (Right _) -> do
                v <- atomically $ do
                    newDmChan <- newTChan
                    userBroadcastChannelChan <- cloneTChan defaultChannel.broadcastChan

                    let newUserName = Username parsedUserResp
                    let newUser = User{name = newUserName, dmChan = newDmChan, channelChan = userBroadcastChannelChan}

                    modifyTVar chatServerStateVar.channels (updateDefaultChannelMembers (HashMap.insert newUserName newUser))
                    writeTChan defaultChannel.broadcastChan [i|#{newUserName} just joined, welcome!\n|]

                    pure newUser
                putStrLn "Done"
                pure v

isInvalidUsername :: [Username] -> Text -> Either Text ()
isInvalidUsername memberList potentialUserName = do
    toEitherBool "Username cannot be empty. Please provide another: " . T.null $ potentialUserName
    toEitherBool "Username must be at least 3 characters long. Please provide another: " . (< 3) . T.length $ potentialUserName
    toEitherBool [i|The name '#{potentialUserName}' is already in use. Please provide another: |] . ((`elem` memberList) . Username) $ potentialUserName
  where
    toEitherBool :: Text -> Bool -> Either Text ()
    toEitherBool e bool = if bool then Left e else Right ()

unRegisterUser :: ChatServerState -> User -> IO ()
unRegisterUser chatServerStateVar (User username _ _) = do
    putStrLn "Running unregistration"
    atomically $ do
        modifyTVar chatServerStateVar.channels (updateDefaultChannelMembers (HashMap.delete username))
        -- traceM (prettyChan chan)
        defaultChannel <- getDefaultChannel chatServerStateVar
        writeTChan defaultChannel.broadcastChan [i|#{username} quit the channel\n|]

-- Alternatively we could have three threads per client, one for receiving client input, another for receiving client notification requests via the chans, and a third for handling all those inputs and requests
-- handleSession :: Socket -> ChatServerState -> User -> IO ()
handleSession s serverStateRef user = do
    -- putStrLn "handle Session"
    -- One thread for dealing with client input
    -- another for client notification
    -- interThreadChan <- newTChanIO
    void $ race (handleClientNotif s user) ()


handleClientNotif :: Socket -> User -> IO ()
handleClientNotif s user = forever $ do
    v <- atomically $ (Left <$> readTChan user.channelChan) `orElse` (Right <$> readTChan user.dmChan)
    sendAll s (fromString . fromEither $ v)

-- handleSession :: Socket -> ChatServerState -> IO ()
-- handleSession s c = forever $ flip onException (pure ()) $ do
--     msg <- recv s 1024
--     unless (BC.null msg) $ do
--         print "handle input"
--         sendAll s msg

-- getMaskingState >>= print
-- handleSession s c

-- handleInput s

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --
getDefaultChannel :: ChatServerState -> STM Channel
getDefaultChannel chatServerStateVar = do
    chatServerState <- readTVar chatServerStateVar.channels
    pure $ chatServerState HashMap.! defaultChannelName

updateDefaultChannelMembers :: (ChannelMembership -> ChannelMembership) -> ChannelRegistry -> ChannelRegistry
updateDefaultChannelMembers f = HashMap.adjust (over membersL f) defaultChannelName

prettyChan :: Channel -> String
prettyChan (Channel title members _) = let memberList = HashMap.keys members in [i|Channel #{title} has the following members: #{memberList}|]

-- -------------------------------------------------------------------------- --
--                                    Utils                                   --
-- -------------------------------------------------------------------------- --
tstrip :: Text -> Text
tstrip = lStrip . rStrip
  where
    lStrip :: Text -> Text
    lStrip = T.dropWhile isSpace

    rStrip :: Text -> Text
    rStrip = T.dropWhileEnd isSpace
