module Concurrent.ThreadsAndMVars.Chan where

import Control.Concurrent hiding (Chan, newChan, readChan, writeChan)
import Control.Monad (void)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

type ReadEnd a = MVar (Stream a)
type WriteEnd a = MVar (Stream a)

-- Another way to think of a channel is as an entity with a read and write end that stores data in a linked-list-esque fashion where elements are hole (which can also be called a `Stream`) that can either be empty or filled (contains some value)
data Chan a = Chan (ReadEnd a) (WriteEnd a)

-- Initially, both read and write pointers reference the "hole"
newChan :: IO (Chan a)
newChan = do
    hole <- newEmptyMVar
    readEnd <- newMVar hole
    writeEnd <- newMVar hole
    pure (Chan readEnd writeEnd)

-- To write a value into the channel we create a new hole and put our desired value into the old hole then point our write pointer to the new hole
-- Now that a value has been placed into the old hole, the read pointer that still references the old hole can now retrieve the new value (remember that in the `newChan` function the read pointer was made to reference the old hole)
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeEnd) val = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeEnd
    putMVar oldHole (Item val newHole)
    putMVar writeEnd newHole

-- To read a value from a channel, we must follow the`read-end` of the channel. We read (use takeMVar) from the read end to get the stream. We read from the stream to get the first `Item` in the stream alongside a pointer to the rest of the stream.
-- After we've read all this, we set the read pointer to the tail of the stream referenced by the retrieved `Item`, then we return the value that was stored inside the retrieved item
readChan' :: Chan a -> IO a
readChan' (Chan readEnd _) = do
    filledHole <- takeMVar readEnd
    -- putStrLn "Blocking on second takeMVar if chan is empty"
    (Item val tail') <- takeMVar filledHole
    putMVar readEnd tail'
    pure val

-- With this implementation, say we had two read-ends and two readers, reader A and reader B. Now say reader A reads the value stored in the first hole, what this new implementation of `readChan` would do is rather than discarding that hole after it's value has been read, it keeps the hole but carries on with shifting the pointer for reader A's read-end to the tail of the channel (the remaining holes in the channel). This way, reader B will still have access to the value stored within the first hole (since reader B's read-end was created with a reference to that hole).
-- Once reader B reads the value from that first hole, its read-end shall also move it's pointer such that it would be referencing the next hole in the channel. I imagine once none of the read-ends no longer references the first hole, it'll be garbage collected along with it's stored value
readChan :: Chan a -> IO a
readChan (Chan readEnd _) = do
    filled <- takeMVar readEnd
    -- putStrLn "Blocking on second takeMVar if chan is empty"
    (Item val tail') <- readMVar filled
    putMVar readEnd tail'
    pure val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeEnd) = do
    hole <- readMVar writeEnd
    newReadEnd <- newMVar hole
    pure $ Chan newReadEnd writeEnd

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readEnd _) a = do
    newHole <- newEmptyMVar
    potentiallyFilledHole <- takeMVar readEnd
    putMVar newHole (Item a potentiallyFilledHole)
    putMVar readEnd newHole

-- If we try to `readChan` while the channel is empty, the thread will block on the second `takeMVar` (because the second `takeMVar` will find an empty hole, instead of an Item). It will block until a `writeChan` is called on another thread to put some value (an Item) into the hole so it can be read from the read end (a hole cannot be read out since it is an empty MVar)

-- If multiple concurrent threads call `readChan` the first one will work as expected but the other will all block until the first call to `readChan` has been completed and the read end updated (to point to the new tail). Similarly, if multiple threads were to call `writeChan` the first call to `writeChan` will work as expected while all the others will be blocked until the first call to `writeChan` has completed and the write end has been updated to point to the new hole

-- Nevertheless, because the read and write ends are separate MVars, a `readChan` and a `writeChan` operation can occur concurrently (without interference, while a `readChan` operation is occurring, a `writeChan` operation can also progress) assuming the channel isn't empty

program1 :: IO ()
program1 = do
    chan <- newChan
    void $ forkIO $ readChan chan >>= putStrLn
    threadDelay (10 ^ 7)
    writeChan chan "Hello"

main :: IO ()
main = do
    chan <- newChan
    writeChan chan "Hello"
    void $ forkIO $ readChan chan >>= putStrLn
    void $ forkIO $ writeChan chan "Bye"
    readChan chan >>= putStrLn
