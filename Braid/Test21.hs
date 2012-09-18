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
  do { B.putStrLn ( "Test a two level braid, with channels." )
     ; tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
       -- First create the channels
     ; chan12 <- C.newChan "1->2 Chan"
     ; chan21 <- C.newChan "2->1 Chan"
     ; tidHubl2 <- B.bundle "hubl2" (hubl2 loops 0 100 chan12 chan21)
       -- Create the lower level hub.
     ; B.putStrLn ( "=== Ms1 is bundled, tid of Hubl2 = " ++ show tidHubl2 )
     ; tid3     <- B.forkLift "Ls1" (lsChan loops 0 100 chan21 chan12)
     ; B.putStrLn ( "=== hubl1: lsChan2 forked, tid = " ++ show tid3 )
     ; B.putStrLn ( "=== hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
lsChan ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    C.Chan String   -> -- Input channel
    C.Chan String   -> -- Output channel
    L.Thread Ls ()     -- A lifted thread
lsChan _loops _j _delay inChan outChan =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "=== ls1    threadId " ++ show tid )
     ; L.writeChan outChan "High Their!"
     ; L.putStrLn ( "=== ls1: wrote to outChan" )
     ; s <- L.readChan inChan
     ; L.putStrLn ( "=== lsChan got: " ++ s )
     ; L.putStrLn ( "=== lsChan1 done!" )
     ; return ()
     }

-- Lower level hub.
hubl2 ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    C.Chan String   -> -- Input channel
    C.Chan String   -> -- Output channel
    B.Braid Ls Ms ()
hubl2 loops _j _delay inChan outChan =
  do { tid <- B.myThreadId
     ; loc <- B.getLocal
     ; B.putStrLn ( "=== hubl2 threadId = " ++ show tid ++ " // " ++ show loc )
     ; tid2 <- B.forkLift "Ms1" (msChan loops 0 1000000 inChan outChan)
     ; B.putStrLn ( "=== Ms1 is forkLifted, tid = " ++ show tid2 )
     ; B.putStrLn ( "=== hubl2 done!" )
     }

-- A local thread of the lower level hub
msChan ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    C.Chan String   -> -- Input channel
    C.Chan String   -> -- Output channel
    L.Thread Ms ()
msChan _loops _j _delay inChan outChan =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "=== ms1 threadId = " ++ show tid )
     ; s <- L.readChan inChan
     ; L.putStrLn ( "=== ms1 got: " ++ s )
     ; L.writeChan outChan s
     ; L.putStrLn ( "=== ms1 done!" )
     }

main :: IO ()
main = startup 21 "hubl1" hubl1
