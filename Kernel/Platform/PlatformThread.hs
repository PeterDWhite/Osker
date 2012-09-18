-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module PlatformThread
    ( PlatformThread   -- A process thread in the domain braid
    , projPlat         -- Project out of the process thread
    , inPlat           -- Lift into the process thread
      -- Platform thread versions of channel primitives
    , writeChan        -- Write to a channel
    , readChan         -- Read from a channel
    , isEmptyChan      -- Test if channel is empty
    , LC.Chan          -- Channel abstract type
      -- Platform thread versions of IO primitives
    , putStrLn         -- Write a line to the screen
    , putStr           -- Write a string to the screen
      -- Platform thread versions in support of fork
    , myThreadId       -- Get the local thread Id
    , yield            -- Yield the processor for a while
      -- In support of IO actions
    , liftIO           -- Raise an IO action to the bounded thread monad
    , liftPT           -- Lift IO action to the process thread
    ) where

-- Haskell imports
import Prelude hiding ( putStrLn, putStr )
import qualified Dynamic as DYN
-- Braid imports
import qualified BraidExternal as B
import qualified BraidLocal as L
import qualified LocalChan as LC
import qualified ThreadId as TID
import qualified BoundedMonad as BdM
-- Posix imports
import qualified ProcessName as PN
-- Local imports
import qualified PlatformState as PS

----------------------------------------------------------------------
-- A process thread within the domain braid
----------------------------------------------------------------------

-- A process thread is a local thread within the domain braid
-- When a process thread is lifted, it will have the same type as
-- the domain braid. This is the whole point, that a local thread
-- (in this case a process thread), can be lifted into the braid
-- safely
newtype PlatformThread a = PlatformThread (L.Thread PS.PlatformState a)

-- Project out of a process thread
projPlat :: PlatformThread a -> L.Thread PS.PlatformState a
projPlat (PlatformThread t) = t

-- Inject into a process thread
inPlat :: L.Thread PS.PlatformState a -> PlatformThread a
inPlat t = PlatformThread t

-- A wrapper on a monad is still a monad
instance Monad PlatformThread where
    return = PlatformThread . return
    PlatformThread p >>= k =
      PlatformThread ( do { a <- p
                          ; let PlatformThread k' = k a
                          ; k'
                          }
                     )

----------------------------------------------------------------------
-- Platform thread channel primitives
----------------------------------------------------------------------

-- Platform thread version of writeChan
writeChan :: (DYN.Typeable a) => LC.Chan a -> a -> PlatformThread ()
writeChan chan = inPlat . LC.writeChan chan

-- Platform thread version of readChan
readChan :: (DYN.Typeable a) => LC.Chan a -> PlatformThread a
readChan = inPlat . LC.readChan

-- Test if a channel is empty
isEmptyChan :: (DYN.Typeable a) => LC.Chan a -> PlatformThread Bool
isEmptyChan = inPlat . LC.isEmptyChan

----------------------------------------------------------------------
-- Platform thread screen IO primitives
----------------------------------------------------------------------
-- Write a line to the screen
putStrLn :: String -> PlatformThread ()
putStrLn = inPlat . L.putStrLn

-- Write a line to the screen
putStr :: String -> PlatformThread ()
putStr = inPlat . L.putStr

----------------------------------------------------------------------
-- Platform thread forking primitives
----------------------------------------------------------------------
-- Get the local thread Id, tid specialized to process name
myThreadId :: PlatformThread PN.ProcessName
myThreadId = inPlat L.myThreadId

-- Yield the processor for a while
yield :: PlatformThread ()
yield = inPlat L.yield

----------------------------------------------------------------------
-- Support for IO actions
----------------------------------------------------------------------
-- Lift to the bounded monad around process thread
liftIO :: IO a -> BdM.BdM PlatformThread b a
liftIO io = BdM.BdM ( \b -> BdM.pairM b (PlatformThread (L.liftIO io)) )

-- Lift IO to the process thread
liftPT :: IO a -> PlatformThread a
liftPT = PlatformThread . L.liftIO
