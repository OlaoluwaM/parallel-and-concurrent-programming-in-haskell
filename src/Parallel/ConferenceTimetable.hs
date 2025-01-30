{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Parallel.ConferenceTimetable where

import Control.DeepSeq (NFData)

-- import Data.Bifunctor (Bifunctor (bimap))
-- import Data.Map (Map)
-- import Data.Map qualified as Map

newtype Talk = Talk Int
    deriving stock (Eq, Ord)
    deriving newtype (NFData, Show)

data Attendee = Attendee
    { name :: String
    , talks :: [Talk]
    }
    deriving stock (Show)

{-
  Here is how the below type alias represents a timetable. Let's assume that our conference has four tracks each with three slots. With our current representation, such a schedule would be modelled as follows with each inner list representing a set of slots where each slot belongs to some track. The exact track a slot belongs to can be more easily identified by transposing the TimeTable list:

  [[slot, slot, slot, slot1], [slot, slot, slot, slot1], [slot, slot, slot, slot1]]

  Transposing the above gives us a slightly clearer representation

  [[slot, slot, slot], [slot, slot, slot], [slot, slot, slot], [slot, slot, slot]]

  where each inner-list is now representative of a track. I reckon we could also model this using an IntMap
-}
type TimeTable = [[Talk]]

timetable :: [Attendee] -> [Talk] -> Int -> Int -> [TimeTable]
timetable attendees allTalks totalNumOfTracks totalNumOfSlots = undefined

-- generate ::
--     Int -> -- Current slot number
--     Int -> -- Current track number
--     Int -> -- Max Slots
--     [[Talk]] -> -- Slots allocated so far
--     [Talk] -> -- talks in this slot
--     [Talk] -> -- talks that can be allocated in this slot
--     [Talk] -> -- all talks remaining to be allocated
--     [TimeTable] -- all possible solutions
-- generate slotNo trackNo maxSlot slots slot slotTalks talks
--   | slotNo == maxSlot = [slots]

-- clashes :: Map Talk [Talk]
-- clashes = Map.fromListWith union [(t, ts) | s <- people, (t, ts) <- selects (talks s)]

-- Simon's implementation
selects :: [a] -> [(a, [a])]
selects [] = []
selects xs0 = go [] xs0
  where
    go _ [] = []
    go xs (y : ys) = (y, xs ++ ys) : go (y : xs) ys

mySelects :: [a] -> [(a, [a])]
mySelects [] = []
mySelects xs = map (\count -> let (lhs, rhs) = splitAt count xs in (last lhs, init lhs ++ rhs)) [1 .. (length xs)]
