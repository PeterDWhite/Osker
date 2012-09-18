-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ProcessThread
    ( ProcessThread    -- A process thread in the domain braid
    , projPT           -- Project out of the process thread
    , inPT             -- Lift into the process thread
      -- Process thread versions of channel primitives
    , writeChan        -- Write to a channel
    , readChan         -- Read from a channel
    , isEmptyChan      -- Test if channel is empty
    , LC.Chan          -- Channel abstract type
      -- Process thread versions of IO primitives
    , putStrLn         -- Write a line to the screen
    , putStr           -- Write a string to the screen
      -- Process thread versions in support of fork
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
import qualified ProcessState as PS

----------------------------------------------------------------------
-- A process thread within the domain braid
----------------------------------------------------------------------

-- A process thread is a local thread within the domain braid
-- When a process thread is lifted, it will have the same type as
-- the domain braid. This is the whole point, that a local thread
-- (in this case a process thread), can be lifted into the braid
-- safely
newtype ProcessThread a = ProcessThread (L.Thread PS.ProcessState a)

-- Project out of a process thread
projPT :: ProcessThread a -> L.Thread PS.ProcessState a
projPT (ProcessThread t) = t

-- Inject into a process thread
inPT :: L.Thread PS.ProcessState a -> ProcessThread a
inPT t = ProcessThread t

-- A wrapper on a monad is still a monad
instance Monad ProcessThread where
    return = ProcessThread . return
    ProcessThread p >>= k =
      ProcessThread ( do { a <- p
                         ; let ProcessThread k' = k a
                         ; k'
                         }
                    )

----------------------------------------------------------------------
-- Process thread channel primitives
----------------------------------------------------------------------

-- Process thread version of writeChan
writeChan :: (DYN.Typeable a) => LC.Chan a -> a -> ProcessThread ()
writeChan chan = inPT . LC.writeChan chan

-- Process thread version of readChan
readChan :: (DYN.Typeable a) => LC.Chan a -> ProcessThread a
readChan = inPT . LC.readChan

-- Test if a channel is empty
isEmptyChan :: (DYN.Typeable a) => LC.Chan a -> ProcessThread Bool
isEmptyChan = inPT . LC.isEmptyChan

----------------------------------------------------------------------
-- Process thread screen IO primitives
----------------------------------------------------------------------
-- Write a line to the screen
putStrLn :: String -> ProcessThread ()
putStrLn = inPT . L.putStrLn

-- Write a line to the screen
putStr :: String -> ProcessThread ()
putStr = inPT . L.putStr

----------------------------------------------------------------------
-- Process thread forking primitives
----------------------------------------------------------------------
-- Get the local thread Id (which is a process name)
myThreadId :: ProcessThread PN.ProcessName
myThreadId = inPT L.myThreadId

-- Yield the processor for a while
yield :: ProcessThread ()
yield = inPT L.yield

----------------------------------------------------------------------
-- Support for IO actions
----------------------------------------------------------------------
-- Lift to the bounded monad around process thread
liftIO :: IO a -> BdM.BdM ProcessThread b a
liftIO io = BdM.BdM ( \b -> BdM.pairM b (ProcessThread (L.liftIO io)) )

-- Lift IO to the process thread
liftPT :: IO a -> ProcessThread a
liftPT = ProcessThread . L.liftIO
