-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--------------------------------------------------------------------
-- Test creation of an MVar in a lifted thread
--------------------------------------------------------------------

--Haskell imports
import Monad
-- Test imports
import TestSupport
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import qualified BraidLocal as L
-- Channel imports
import qualified OskerChan as C

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { B.putStrLn ( "Test a lifted MVar, without a skip to help the timing" )
     ; tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
     ; mvarChan <- C.newChan "receive Chan"
     ; tid2  <- B.forkLift "Ls1" (lsChan loops 0 100 mvarChan)
     ; B.putStrLn ( "=== hubl1: lsChan forked, tid = " ++ show tid2 )
     ; mvar <- C.readChan mvarChan
     ; B.putStrLn ( "=== hubl1: Got the mvar-mvar: " ++ show mvar )
     ; s    <- B.takeMVar mvar
     ; B.putStrLn ( "=== hubl1 got: " ++ s )
     ; B.putStrLn ( "hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
-- First argument is max loop count
-- Second argument is current loop count
-- Third argument is the delay
lsChan ::
    Int                    -> -- Max loop count
    Int                    -> -- Current loop count
    Int                    -> -- Delay count
    C.Chan (B.MVar String) -> -- Channel to send MVar to the hub
    L.Thread Ls ()            -- A lifted thread
lsChan _loops _j _delay mvarChan =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "ls1    threadId " ++ show tid )
     ; mvar <- L.newEmptyMVar "Lifted MVar"
     ; L.putStrLn ( "=== ls1: Created Lifted MVar" )
     ; L.writeChan mvarChan mvar
     ; L.putStrLn ( "=== ls1: put to MVar" )
     ; L.putMVar mvar "High Their"
     ; L.putStrLn ( "=== ls1: put to mvarChan" )
     ; L.putStrLn ( "lsChan done" )
     ; return ()
     }

main :: IO ()
main = startup 16 "hubl1" hubl1
