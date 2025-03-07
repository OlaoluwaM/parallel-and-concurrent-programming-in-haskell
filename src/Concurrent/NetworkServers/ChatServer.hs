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
{-# LANGUAGE NoFieldSelectors #-}

module Concurrent.NetworkServers.ChatServer where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
    STM,
    TChan,
    TVar,
    atomically,
    dupTChan,
    modifyTVar,
    newTChanIO,
    newTVar,
    newTVarIO,
    readTChan,
    readTVar,
    readTVarIO,
    writeTChan,
    writeTVar,
 )
import Control.Exception (bracket)
import Control.Monad (forever, join, void)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as AC
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Network.Run.TCP (runTCPServer)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

{-
    Having the type be `TVar Channel` here seems to only be useful when you have multiple channels. This would make it such that, as long as there are no structural changes to the `HashMap` itself (inserts or deletes), threads speaking to one channel won't be affected by changes to another. However, since right now we only have one channel, all threads will be speaking/accessing to one channel making `TVar (HashMap k (TVar Channel)) the same in terms of performance to `TVar (HashMa k v)` with more indirection
-}
newtype ChatServerState = ChatServerState {channels :: HashMap ChannelName Channel}

data Channel = Channel
    { title :: ChannelName
    , members :: TVar (HashMap Username User)
    , broadcastChan :: ChannelBroadcastChan
    }

newtype ChannelName = ChannelName {channelName :: Text}
    deriving newtype (Hashable, Eq, Show)

type ChannelBroadcastChan = TChan Text

data User = User {name :: Username, socket :: Socket, channelChan :: ChannelBroadcastChan, kickStatus :: TVar KickedStatus}

newtype Username = Username {username :: Text}
    deriving newtype (Show, Eq, Hashable)

data KickedStatus = Kicked KickDetails | NotKicked

data KickDetails = KickDetails {by :: Username, at :: UTCTime}

data ClientCommand = DM Username Text | ChannelBroadcast Text | Quit | Kick Username | Help | HelpInvalid | Stats
    deriving stock (Show, Eq)

defaultChannelName :: ChannelName
defaultChannelName = ChannelName "default-channel"

serviceName :: Text
serviceName = "haskell-irc"

main :: IO ()
main = do
    broadcastChan <- newTChanIO
    defaultChannelMembers <- newTVarIO HashMap.empty

    let defaultChannel = Channel{title = defaultChannelName, members = defaultChannelMembers, broadcastChan}
    -- Channels are immutable since we aren't supporting the ability for users to create new channels or get rid of channels
    let chatServerState = ChatServerState{channels = HashMap.singleton defaultChannelName defaultChannel}

    {-
        We don't exactly need to do these two actions as a race, but because we want the relationship semantics to be akin to siblings
        such that if one should exit the other does too, race seems appropriate. `concurrently` is also an option too
    -}
    void $ race (broadcastInServer broadcastChan) (runTCPServer Nothing "4728" (handleClientConnection chatServerState))
  where
    broadcastInServer :: TChan Text -> IO ()
    broadcastInServer broadcastTChan = forever $ atomically (readTChan broadcastTChan) >>= T.putStrLn

handleClientConnection :: ChatServerState -> Socket -> IO ()
handleClientConnection serverState socket = do
    bracket (registerUser serverState socket) (unRegisterUser serverState) (handleSession serverState)

registerUser :: ChatServerState -> Socket -> IO User
registerUser chatServerState socket = do
    sendAll socket [i|Welcome to the #{serviceName}! Enter what you'd like to be called in the chat: |]
    loop
  where
    loop :: IO User
    loop = do
        rawUserInput <- recv socket 1024
        let parsedUserInput = T.strip . T.decodeUtf8Lenient $ rawUserInput
        let defaultChannel = getDefaultChannel chatServerState

        membersList <- atomically do
            channelMembershipRegistry <- readTVar defaultChannel.members
            pure $ HashMap.keys channelMembershipRegistry

        case isInvalidUsername membersList parsedUserInput of
            (Left errMsg) -> do
                sendAll socket (T.encodeUtf8 errMsg)
                loop
            (Right _) -> do
                let chosenUsername = Username parsedUserInput
                atomically $ do
                    userBroadcastChannelChan <- dupTChan defaultChannel.broadcastChan
                    kickStatus <- newTVar NotKicked

                    let newUser = User{name = chosenUsername, socket = socket, channelChan = userBroadcastChannelChan, kickStatus}

                    modifyTVar defaultChannel.members (HashMap.insert chosenUsername newUser)
                    writeTChan defaultChannel.broadcastChan [i|[Info] #{chosenUsername} just joined, welcome!|]

                    pure newUser

isInvalidUsername :: [Username] -> Text -> Either Text ()
isInvalidUsername memberList potentialUserName = do
    toEitherBool "Username cannot be empty. Please provide another: " . T.null $ potentialUserName
    toEitherBool "Username must be at least 3 characters long. Please provide another: " . (< 3) . T.length $ potentialUserName
    toEitherBool [i|The name '#{potentialUserName}' is already in use. Please provide another: |] . ((`elem` memberList) . Username) $ potentialUserName
  where
    toEitherBool :: Text -> Bool -> Either Text ()
    toEitherBool e bool = if bool then Left e else Right ()

unRegisterUser :: ChatServerState -> User -> IO ()
unRegisterUser chatServerState (User username _ _ _) = do
    let defaultChannel = getDefaultChannel chatServerState
    atomically $ do
        modifyTVar defaultChannel.members (HashMap.delete username)
        writeTChan defaultChannel.broadcastChan [i|[Info] #{username} quit the channel|]

handleSession :: ChatServerState -> User -> IO ()
handleSession chatServerState user = do
    -- One thread for dealing with client input
    -- another for client notification
    let defaultChannel = getDefaultChannel chatServerState
    void $ race (handleClientNotif user) (handleClientInput defaultChannel user)

handleClientNotif :: User -> IO ()
handleClientNotif user = forever $ do
    channelBroadcastMsg <- atomically $ readTChan user.channelChan
    sendAllLn user.socket (T.encodeUtf8 channelBroadcastMsg)

handleClientInput :: Channel -> User -> IO ()
handleClientInput defaultChannel user = do
    let clientSocket = user.socket
    rawCmd <- recv clientSocket 1024
    let parsedCmd = parseCmdFromInput rawCmd
    case parsedCmd of
        Quit -> quitServer user
        DM recipientUsername rawMsg -> validateKickStatus user (dmUser defaultChannel user recipientUsername rawMsg) >> handleClientInput defaultChannel user
        ChannelBroadcast msg -> validateKickStatus user (sendMsgToChannel user msg) >> handleClientInput defaultChannel user
        Kick victimUsername -> validateKickStatus user (kickUser defaultChannel user victimUsername) >> handleClientInput defaultChannel user
        Help -> showHelpMessage clientSocket >> handleClientInput defaultChannel user
        HelpInvalid -> sendAllLn clientSocket "[Info] Invalid command" >> showHelpMessage clientSocket >> handleClientInput defaultChannel user
        Stats -> displayChannelStats clientSocket defaultChannel >> handleClientInput defaultChannel user

parseCmdFromInput :: ByteString -> ClientCommand
parseCmdFromInput clientInput
    | BC.null clientInput = Quit
    | otherwise = fromRight HelpInvalid . AC.parseOnly cmdParser $ clientInput
  where
    cmdParser = AC.choice [dmCmdParser, altDmCmdParser, quitCmdParser, kickCmdParser, helpCmdParser, statsCmdParser, broadcastCmdParser]
    helpCmdParser = (cmdPrefixParser *> AC.string "help") $> Help
    statsCmdParser = (cmdPrefixParser *> AC.string "stats") $> Stats
    broadcastCmdParser = ChannelBroadcast . T.decodeUtf8 <$> A.takeTill AC.isEndOfLine
    quitCmdParser = (cmdPrefixParser *> (AC.string "quit" <|> AC.string "exit") <* AC.endOfLine) $> Quit
    kickCmdParser = Kick . Username . T.decodeUtf8 <$> (cmdPrefixParser *> AC.string "kick" *> AC.skipSpace *> A.takeTill AC.isEndOfLine)
    altDmCmdParser = do
        AC.char '@'
        recipientUsername <- AC.takeTill isSpace
        DM (Username (T.decodeUtf8 recipientUsername)) . T.decodeUtf8 <$> (AC.skipSpace *> A.takeTill AC.isEndOfLine)

    dmCmdParser = do
        cmdPrefixParser
        AC.string "tell" <|> AC.string "dm"
        AC.skipSpace
        recipientUsername <- AC.takeTill isSpace
        DM (Username (T.decodeUtf8 recipientUsername)) . T.decodeUtf8 <$> (AC.skipSpace *> A.takeTill AC.isEndOfLine)
    cmdPrefixParser = AC.char '/'

quitServer :: User -> IO ()
quitServer (User{name, socket}) = sendAllLn socket [i|Thanks for chatting with us #{name}. Bye for now!|]

validateKickStatus :: User -> IO () -> IO ()
validateKickStatus user action = do
    kickStatus <- readTVarIO user.kickStatus
    case kickStatus of
        Kicked (KickDetails{by}) -> sendAllLn user.socket [i|[Info] Sorry, since #{by} kicked you, you're banned from performing this action. Be better|]
        NotKicked -> action

dmUser :: Channel -> User -> Username -> Text -> IO ()
dmUser defaultChannel sender recipientUsername rawMsg = do
    let senderUsername = sender.name
    recipientM <- atomically $ lookupUser recipientUsername defaultChannel
    case recipientM of
        Nothing -> sendAllLn sender.socket [i|Couldn't DM #{recipientUsername} because they aren't a member of this IRC|]
        Just recipient -> let msg = T.encodeUtf8 rawMsg in sendAllLn recipient.socket [i|[#{senderUsername}]: #{msg}|]

sendMsgToChannel :: User -> Text -> IO ()
sendMsgToChannel (User{name, channelChan}) msg = atomically $ writeTChan channelChan [i|[Broadcast] #{name}: #{msg}|]

kickUser :: Channel -> User -> Username -> IO ()
kickUser defaultChannel kicker victimUsername = do
    now <- getCurrentTime
    join $ atomically $ do
        victimM <- lookupUser victimUsername defaultChannel
        case victimM of
            Nothing -> pure (sendAllLn kicker.socket [i|[Info] Cannot kick #{victimUsername}. User does not exist|])
            Just victim -> do
                victimKickStatus <- readTVar victim.kickStatus
                case victimKickStatus of
                    Kicked (KickDetails{by, at}) -> pure (sendAllLn kicker.socket [i|The user #{victimUsername} has already been kicked by #{by} at #{at}|])
                    NotKicked -> do
                        let kickerUsername = kicker.name
                        writeTVar victim.kickStatus (Kicked $ KickDetails{by = kickerUsername, at = now})
                        writeTChan defaultChannel.broadcastChan [i|[Info] #{kickerUsername} has kicked #{victimUsername} from the channel|]
                        pure (pure ())

showHelpMessage :: Socket -> IO ()
showHelpMessage socket =
    sendAllLn
        socket
        [i|
Available commands:
    /help           - Show this help message
    /stats          - Show channel statistics
    /tell <user> <message>  - Send a private message to a user
    /dm <user> <message>    - Same as /tell
    /kick <user>    - Kick a user from the channel
    /quit or /exit  - Leave the chat server

To broadcast a message to everyone, just type your message and press enter.
|]

displayChannelStats :: Socket -> Channel -> IO ()
displayChannelStats socket defaultChannel = join $ atomically do
    membersMap <- readTVar defaultChannel.members
    let totalUsers = HashMap.size membersMap
    let members = HashMap.elems membersMap
    let channelTitle = defaultChannel.title

    memberDetails <- traverse formatMemberStatusForOutput members
    kickedUsers <- length . filter isKicked <$> traverse (readTVar . (.kickStatus)) members
    pure $
        sendAllLn
            socket
            [i|
Channel: #{channelTitle}
Total users: #{totalUsers}
Active users: #{totalUsers - kickedUsers}
Kicked users: #{kickedUsers}
Member Statuses:
#{BC.unlines memberDetails}|]
  where
    formatMemberStatusForOutput :: User -> STM ByteString
    formatMemberStatusForOutput (User{name, kickStatus}) = do
        kickedStatus <- readTVar kickStatus
        let statusText = case kickedStatus of
                NotKicked -> "(active)" :: ByteString
                Kicked (KickDetails{by, at}) -> [i|(kicked by #{by} at #{at})|]

        pure [i|  - #{name} #{statusText}|]

-- -------------------------------------------------------------------------- --
--                                   Helpers                                  --
-- -------------------------------------------------------------------------- --
getDefaultChannel :: ChatServerState -> Channel
getDefaultChannel = (HashMap.! defaultChannelName) . (.channels)

lookupUser :: Username -> Channel -> STM (Maybe User)
lookupUser targetUsername defaultChannel = do
    chanMembers <- readTVar defaultChannel.members
    pure $ chanMembers HashMap.!? targetUsername

isKicked :: KickedStatus -> Bool
isKicked (Kicked _) = True
isKicked NotKicked = False

-- -------------------------------------------------------------------------- --
--                                    Utils                                   --
-- -------------------------------------------------------------------------- --

sendAllLn :: Socket -> ByteString -> IO ()
sendAllLn socket = sendAll socket . (<> "\n")
