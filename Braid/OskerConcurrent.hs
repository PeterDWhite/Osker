-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module OskerConcurrent
    ( C.Chan                    -- Abstract type of channels
      -- creator
    , C.newChan                 -- :: Braid (Chan a)
      -- operators
    , C.writeChan               -- :: Chan a -> a -> Braid ()
    , C.readChan	 	-- :: Chan a -> Braid a
    , C.isEmptyChan		-- :: Chan a -> Braid Bool
      -- stream interface
    , C.getChanContents         -- :: Chan a -> Braid [a]
      -- In support of Fork
    , B.ThreadId                -- Abstract type of thread id
    , B.myThreadId              -- Get your own thread Id
    , B.fork                    -- For a new thread in the braid
    , B.threadDelay             -- Delay a thread by specified time interval
    , B.killThread              -- Remove a thread from the braid
    , B.yield                   -- Yield the processor for a while
      -- In support of MVars
    , B.MVar                    -- Abstract
    , B.newEmptyMVar            -- Create new empty MVar
    , B.newMVar                 -- Create new MVar with specified value
    , B.takeMVar                -- Take the value (if any) of an MVar
    , B.putMVar                 -- Put a value into an MVar, when it is empty
    , B.readMVar                -- Get value out of an MVar, no modification
    , B.swapMVar                -- Swap contents of an MVar for a new value
      -- In support of debugging
    , B.putStrLn                -- Print line to the screen
    ) where

----------------------------------------------------------------------
-- This module permits the kernel to be compiled with Haskell
-- concurrent IO, in place of resumption IO
----------------------------------------------------------------------

import qualified OskerChan as C
import qualified BraidExternal as B
