-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module OskerPool
    ( OskerPool (..) -- Abstract data type of Osker resource pool
    , poolMkEmpty    -- Constructor for Osker pool
    , poolAtMax      -- Observer for OskerPool
    , poolAvail      -- Observer for OskerPool
    , poolEmpty      -- Observer for OskerPool
    , poolMkN        -- Pool with n initialized elements
    , poolGet        -- Constructor / Observer for OskerPool
    , poolNGet       -- Constructor / Observer for OskerPool
    , poolReturn     -- Constructor for OskerPool
    ) where

----------------------------------------------------------------------
-- The allocation pool definition.
----------------------------------------------------------------------

-- Osker imports
import qualified Resource as R

-- The default type of pool:
data OskerPool r = -- Abbreviation (slp)
    OskerPool
    {
    slpMax       :: Int, -- Max local number of r
    slpCurrent   :: Int, -- Current local number of r
    slpAvailable :: [r]  -- rs that are currently available
    }

-- Determine if a pool is full
poolAtMax :: OskerPool r -> Bool
poolAtMax p = slpMax p == slpCurrent p

-- Determine if a pool is empty
poolEmpty :: OskerPool r -> Bool
poolEmpty (OskerPool _max _current avail) = null avail

-- Determine the number of resources available
poolAvail :: OskerPool r -> Int
poolAvail (OskerPool _max _current avail) = length avail

-- Get one from the pool
poolGet :: OskerPool r -> (Maybe r, OskerPool r)
poolGet p@(OskerPool pmax current avail) =
  if null avail
  then (Nothing, p)
  else (Just (head avail), OskerPool pmax (current - 1) (tail avail))

-- Get N from the pool
poolNGet :: Int -> OskerPool r -> (Maybe [r], OskerPool r)
poolNGet n p@(OskerPool pmax current avail) =
  if n > current
  then (Nothing, p)
  else let (get, leave) = splitAt n avail
       in (Just get, OskerPool pmax (current - n) leave)

-- Return an element to the pool
poolReturn :: (R.Resource r) => OskerPool r -> r -> OskerPool r
poolReturn (OskerPool pmax current avail) r =
  OskerPool pmax (current + 1) ((R.zeroize r):avail)

-- Return elements to the pool
poolReturns :: (R.Resource r) => OskerPool r -> [r] -> OskerPool r
poolReturns (OskerPool pmax current avail) rs =
  OskerPool pmax (current + length rs) (avail ++ (map R.zeroize rs))

instance Show (OskerPool r) where
    show sp = "OskerPool: " ++ show (slpMax sp)

poolMkEmpty :: Int -> OskerPool r
poolMkEmpty n = (OskerPool n 0 [])

-- Put n initialized elements in the pool
poolMkN :: (R.Resource r) => r -> Int -> OskerPool r
poolMkN r n = poolReturns (poolMkEmpty n) (replicate n (R.zeroize r))
