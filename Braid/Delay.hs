-- Copyright (c) Peter Duncan White, 2003
module Delay
    ( Delay           -- Data structure to permit delaying of threads
    , DelayTid        -- Element of the structure
    , mkDelayTid      -- Constructor for DelayTid
    , delay           -- Delay an element (add it to the queue)
    , getExpired      -- Get the expired elements of the delay queue
    , size            -- Get the size of a delay queue
    ) where

-- Haskell imports
import qualified LeftistHeap as LH
-- Utility imports
import Null
-- Braid imports
import qualified ThreadId as TID

----------------------------------------------------------------------
-- Delay queue is implemented as a leftist heap, sorted on the
-- remaining tick count.
----------------------------------------------------------------------

data DelayTid = DelayTid { delayCount :: Int
                         , delayTid   :: TID.ThreadId
                         }

-- Constructor for DelayTid
mkDelayTid :: TID.ThreadId -> Int -> DelayTid
mkDelayTid tid n = DelayTid { delayCount = n, delayTid = tid }

-- For equality or ordering, all we care about is the delay count
instance Eq DelayTid where
    dqe1 == dqe2 = (delayCount dqe1) == (delayCount dqe2)

instance Ord DelayTid where
    compare dqe1 dqe2 = compare (delayCount dqe1) (delayCount dqe2)

instance Show DelayTid where
    show dqe = "(" ++ show (delayTid dqe) ++","++ show (delayCount dqe) ++ ")"

-- A heap ordered by the delay count
data Delay = Delay (LH.Heap DelayTid)

instance Null Delay where
    mkNull = Delay LH.empty

instance Show Delay where
    show (Delay h) = LH.fold (\dqe s -> s ++ show dqe) "[" h ++ "]"

-- Add an element to the delay queue
delay :: DelayTid -> Delay -> Delay
delay dqe (Delay d) = Delay (LH.insert dqe d)

-- Get the expired elements of the delay queue. The expired elements
-- are removed from the queue, and the remaining elements have their
-- tick counts decremented by the incoming number of ticks.
getExpired :: Int -> Delay -> (Delay, [TID.ThreadId])
getExpired n (Delay d) =
  let (expired, waiting) = LH.partition (\dqe -> delayCount dqe <= n) d
  in ( Delay waiting, map delayTid (LH.toSeq expired) )

-- Get the size of the delay queue
size :: Delay -> Int
size (Delay d) = LH.size d

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkDelayTid  #-}
{-# INLINE delay       #-}
{-# INLINE getExpired  #-}
{-# INLINE size        #-}
