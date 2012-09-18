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

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { B.putStrLn ( "Test two level braid, with MVars, and strategic delay." )
     ; tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
       -- First create the channels
     ; mvar12 <- B.newEmptyMVar "1->2 MVar" 
     ; mvar21 <- B.newEmptyMVar "2->1 MVar"
     ; tidHubl2 <- B.bundle "hubl2" (hubl2 loops 0 100 mvar12 mvar21)
       -- Create the lower level hub.
     ; B.putStrLn ( "=== Ms1 is bundled, tid of Hubl2 = " ++ show tidHubl2 )
     ; tid3     <- B.forkLift "Ls1" (lsMVar loops 0 100 mvar21 mvar12)
     ; B.putStrLn ( "=== hubl1: lsChan2 forked, tid = " ++ show tid3 )
     ; B.putStrLn ( "=== hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
lsMVar ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    L.MVar String   -> -- Input mvar
    L.MVar String   -> -- Output mvar
    L.Thread Ls ()     -- A lifted thread
lsMVar _loops _j _delay inMVar outMVar =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "=== ls1    threadId " ++ show tid )
     ; L.putMVar outMVar "High Their!"
     ; L.putStrLn ( "=== ls1: wrote to outMVar" )
     ; L.threadDelay 1000000
     ; s <- L.takeMVar inMVar
     ; L.putStrLn ( "=== ls1 got: " ++ s )
     ; L.putStrLn ( "=== ls1 done!" )
     ; return ()
     }

-- Lower level hub.
hubl2 ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    B.MVar String   -> -- Input mvar
    B.MVar String   -> -- Output mvar
    B.Braid Ls Ms ()
hubl2 loops _j _delay inMVar outMVar =
  do { tid <- B.myThreadId
     ; loc <- B.getLocal
     ; B.putStrLn ( "=== hubl2 threadId = " ++ show tid ++ " // " ++ show loc )
     ; tid2 <- B.forkLift "Ms1" (msMVar loops 0 1000000 inMVar outMVar)
     ; B.putStrLn ( "=== Ms1 is forkLifted, tid = " ++ show tid2 )
     ; B.putStrLn ( "=== hubl2 done!" )
     }

-- A local thread of the lower level hub
msMVar ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    B.MVar String   -> -- Input MVar
    B.MVar String   -> -- Output MVar
    L.Thread Ms ()
msMVar _loops _j _delay inMVar outMVar =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "=== ms1 threadId = " ++ show tid )
     ; s <- L.readMVar inMVar
     ; L.putStrLn ( "=== ms1 got: " ++ s )
     ; L.putMVar outMVar s
     ; L.putStrLn ( "=== ms1 done!" )
     }

main :: IO ()
main = startup 19 "hubl1" hubl1
