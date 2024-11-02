module Concurrent.ThreadsAndMVars.PhoneBook where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

type Name = String
type PhoneNumber = String
type PhoneBook = Map Name PhoneNumber

-- This is considered good practice as it allows us export PhoneBookState as an abstract type some consumers
-- of this module can't depend on it's implementation
newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

newPhoneBookState :: IO PhoneBookState
newPhoneBookState = do
    m <- newMVar Map.empty
    pure (PhoneBookState m)

insertEntryIntoPhoneBook :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insertEntryIntoPhoneBook (PhoneBookState m) name number = do
    phoneBook <- takeMVar m -- acquires the lock on the PhoneBookState resource
    putMVar m (Map.insert name number phoneBook) -- updates & releases the lock on the resource

lookupNameInPhoneBook :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookupNameInPhoneBook (PhoneBookState m) name = do
    phoneBook <- takeMVar m -- acquires the lock on the PhoneBookState resource
    putMVar m phoneBook
    pure $ Map.lookup name phoneBook

main :: IO ()
main = do
    phoneBook <- newPhoneBookState
    traverse_ (\n -> let nStr = show n in insertEntryIntoPhoneBook phoneBook ("person" <> nStr) nStr) [(1 :: Int) .. 10000]
    lookupNameInPhoneBook phoneBook "person999" >>= print
    lookupNameInPhoneBook phoneBook "unknown" >>= print
