module Concurrent.STM.TChan where

import Control.Concurrent.STM (STM, TVar, newTVar, orElse, readTVar, retry, writeTVar)

data TChan a = TChan (ReadEnd a) (WriteEnd a)

type ReadEnd a = TVar (TVarList a)
type WriteEnd a = TVar (TVarList a)

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)
    deriving stock (Eq)

newTChan :: STM (TChan a)
newTChan = do
    hole <- newTVar TNil
    readEnd <- newTVar hole
    writeEnd <- newTVar hole
    pure $ TChan readEnd writeEnd

-- Like removing an element from the top of a queue
readTChan :: TChan a -> STM a
readTChan (TChan readEnd _) = do
    filledHole <- readTVar readEnd
    tList <- readTVar filledHole
    case tList of
        TNil -> retry
        (TCons a rest) -> do
            writeTVar readEnd rest
            pure a

-- Kinda like an append as the old write end will contain a reference to the new write end while then the write end itself points to the new 'end' of the list
writeChan :: TChan a -> a -> STM ()
writeChan (TChan _ writeEnd) a = do
    newWriteEnd <- newTVar TNil
    oldWriteEnd <- readTVar writeEnd
    writeTVar oldWriteEnd (TCons a newWriteEnd)
    writeTVar writeEnd newWriteEnd

-- Akin to prepending onto a queue or stack
unGetChan :: TChan a -> a -> STM ()
unGetChan (TChan readEnd _) val = do
    oldReadEnd <- readTVar readEnd
    newReadEnd <- newTVar (TCons val oldReadEnd)
    writeTVar readEnd newReadEnd

-- Other Chan operations that were originally impossible to get right with MVars

isEmptyTChan :: Eq a => TChan a -> STM Bool
isEmptyTChan (TChan readEnd _) = do
    readHole <- readTVar readEnd >>= readTVar
    pure (readHole == TNil)

-- This cannot be achieved with MVars as they lack a way to pick between two computations
readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan tchanA tchanB = (Left <$> readTChan tchanA) `orElse` (Right <$> readTChan tchanB)
