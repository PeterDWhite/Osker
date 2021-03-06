-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
module BraidUtilities
    ( B.Braid             -- Braid of threads
    , B.ProgramType (..)  -- Thread lifted, unlifted, or wait?
    , OC.ThreadId         -- Abstract type of thread identifiers
    , myFork              -- Creates MVar that is written when thread dies
    , oskerFork           -- Like myForkIO, but waits on oneshot first
    , ForkInfo (..)       -- Information generated about a thread
    , OS.OneShot          -- An MVar to be written only once
    , OS.newOneShot       -- Create a new one shot
    , OS.shootOneShot     -- Shoot the one shot
    , OS.shootManyOneShot -- Shoot a bunch of one shots
    ) where

-- Haskell imports
import Monad
-- Braid imports
import qualified OskerConcurrent as OC
import qualified BraidExternal as B
-- Utility imports
import qualified BraidOneShot as OS
import Null

data ForkInfo m =
    ForkInfo
    { tiTid   :: B.ThreadId   -- Created by the resumption fork
    , tiChan  :: OC.Chan m    -- Input channel for the thread
    , tiDone  :: OS.OneShot   -- Can wait on this until thread is done
    , tiStart :: OS.OneShot   -- Forked process waits on this to start
    }

instance Show (ForkInfo m) where
    show ti = show (tiTid ti)

-- Modified form of myForkIO from the GHC user manual.
-- This fork creates an MVar, and puts something in it when
-- it dies. This permits the creator to wait for the forked
-- child to die.
myFork :: (Null ls, Show ls, Show gs) => --
    String           -> -- Name of the new thread
    OC.Chan m        -> -- Input channel of the process
    B.Braid gs ls () -> -- Program of the new thread
    B.Braid gs ls (ForkInfo m) -- Resulting braid
myFork name chan program =
  do { mvar <- OC.newEmptyMVar name
     ; tid  <- OC.fork name program
     ; OC.putMVar mvar OS.shoot
     ; return ( ForkInfo
                { tiTid   = tid
                , tiChan  = chan
                , tiDone  = mvar
                , tiStart = error "No one shot for myForkIO"
                })
     }

-- Make an Braid action wait on a oneshot first
wrapIo :: (Show ls, Show gs) => --
    OS.OneShot       -> -- Start up variable
    B.Braid gs ls a  -> -- Program to wrap
    B.Braid gs ls a     -- An Braid action
wrapIo startShot threadProgram =
  do { OS.waitToBeShot startShot
     ; threadProgram
     }

-- Another fork version. This one takes an MVar as argument. The
-- MVar is a "one shot". I.e. it is only written once. The forked
-- thread waits on the one shot before it starts. This permits the
-- creator to sequence the start up of operations of its child
-- threads with a finer granularity.
oskerFork :: ( Null ls, Show ls, Show gs ) => --
    String                      -> -- Name of the process
    OC.Chan m                   -> -- Input channel of the process
    B.Braid gs ls a             -> -- Program of the new thread
    B.Braid gs ls (ForkInfo m) -- Resulting braid
oskerFork name chan program =
   do { -- Make the termination condition variable
        waitShot <- OS.newOneShot (name ++ ".wait")
        -- Make the new start up one shot
      ; startShot <- OS.newOneShot (name ++ ".start")
        -- Create the thread, and its associated data,
        -- and feed the input channel to the program
      ; tid <- OC.fork name
                       ( do { wrapIo startShot program
                            ; OC.putMVar waitShot OS.shoot
                            ; return mkNull
                            }
                       )
        -- Ready to format the KernelThread
      ; return ( ForkInfo
                 { tiTid   = tid
                 , tiChan  = chan
                 , tiDone  = waitShot
                 , tiStart = startShot
                 }
               )
       }
