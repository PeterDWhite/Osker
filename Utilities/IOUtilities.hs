-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module IOUtilities
    ( oskerForkIO         -- Fork with generation of thread info
    , ThreadInfo (..)     -- Information gathered about a thread
    , OS.OneShot          -- A use-once MVar
    , OS.newOneShot       -- Make a new use-once MVar
    , OS.shootManyOneShot -- Shoot the use-once MVar
    , sleepForever        -- Become a zombie
    , M.mapListUnit       -- Generate list of actions and do them
    , getElapsed          -- Get the elapsed time
    ) where

-- Haskell imports
import qualified Concurrent as C
import qualified Exception as E
import PosixProcEnv (getProcessTimes, elapsedTime, ProcessTimes)
import Posix (ClockTick)
-- Utility imports
import qualified OneShot as OS
import qualified MonadUtilities as M

data ThreadInfo m = -- abbreviation ti
    ThreadInfo
    { tiTid   :: C.ThreadId -- Created by the fork
    , tiChan  :: C.Chan m   -- Input channel for the thread
    , tiDone  :: OS.OneShot -- Can wait on this until the thread is done
    , tiStart :: OS.OneShot -- Forked process waits on this to start
    }

instance Show (ThreadInfo m) where
    show ti = show (tiTid ti)

-- Another fork version. This one takes an MVar as argument. The
-- MVar is a "one shot". I.e. it is only written once. The forked
-- thread waits on the one shot before it starts. This permits the
-- creator to sequence the start up of operations of its child
-- threads with a finer granularity.
oskerForkIO :: OS.OneShot -> C.Chan m -> IO () -> IO (ThreadInfo m)
oskerForkIO oneShot chan io =
  let wrapIo = do { OS.waitToBeShot oneShot; io }
  in do { mvar <- C.newEmptyMVar
        ; tid <- C.forkIO (wrapIo `E.finally` C.putMVar mvar OS.shoot)
        ; return (ThreadInfo
                     tid      -- The thread Id of the thread
                     chan     -- The input channel to the thread
                     mvar     -- Wait on this
                     oneShot) -- Stimulate this to start it
        }

-- Sleep forever
sleepForever :: IO ()
sleepForever =
  do { stop  <- C.newMVar True
     ; _flag <- C.takeMVar stop
     ; return ()
     }

-- Get the current elapsed time, in the IO monad
-- The clock ticks are 100 per second
getElapsed :: IO ClockTick
getElapsed = getProcessTimes >>= return . elapsedTime
