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
     ; chanChan <- C.newChan "receive Chan"
     ; tid2     <- B.forkLift "Ls1" (lsChan loops 0 100 chanChan)
     ; B.putStrLn ( "=== hubl1: lsChan forked, tid = " ++ show tid2 )
     ; chan <- C.readChan chanChan
     ; B.putStrLn ( "=== hubl1: Got the chan: " ++ show chan )
     ; s <- C.readChan chan
     ; B.putStrLn ( "=== hubl1 got: " ++ s )
     ; B.putStrLn ( "hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
lsChan ::
    Int                    -> -- Max loop count
    Int                    -> -- Current loop count
    Int                    -> -- Delay count
    C.Chan (C.Chan String) -> -- Channel to send MVar to the hub
    L.Thread Ls ()            -- A lifted thread
lsChan _loops _j _delay chanChan =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "ls1    threadId " ++ show tid )
     ; chan <- L.newChan "Lifted Chan"
     ; L.putStrLn ( "=== ls1: Created Lifted Channel" )
     ; L.writeChan chanChan chan
     ; L.putStrLn ( "=== ls1: wrote to chanChan" )
     ; L.writeChan chan "High Their"
     ; L.putStrLn ( "lsChan done" )
     ; return ()
     }

main :: IO ()
main = startup 17 "hubl1" hubl1
