-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where
----------------------------------------------------------------------
-- Test the mvars in the layered braid
----------------------------------------------------------------------

--Haskell imports
import Monad
import System (getArgs, exitWith, ExitCode(..))
-- Test imports
import TestSupport
-- Utility imports
import Null
import qualified IOUtilities as IOU
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import qualified BraidLocal as L

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; mvar1 <- B.newMVar "hubl1.mv1" "hubl1: Gronk?"
     ; mvar2 <- B.newMVar "hubl1.mv2" "hubl1: Zonk?"
--     ; B.showState "Start hubl1"
     ; tid2  <- B.forkLift "Ls1mvar" (ls1mvar loops 0 mvar2)
     ; B.putStrLn ( "**** Ls1mvar is forkLifted, tid2 = " ++ show tid2 )
     ; tid3  <- B.fork "Gs1mvar" (gs1mvar loops 0 mvar1)
     ; B.putStrLn ( "**** Gs1mvar is forked, tid3 = " ++ show tid3 )
     ; hubl1' 0 mvar1
     ; B.putStrLn ( "hubl1' done!" )
     } where hubl1' :: Int -> B.MVar String -> B.Braid Gs Ls ()
             hubl1' n mvar =
               do { tid' <- B.myThreadId
                  ; B.threadDelay 300000
                  ; B.putStrLn ( "=== hubl1': taking" )
                  ; val  <- B.takeMVar mvar
                  ; B.putStrLn ( "=== hubl1': got: " ++ show val )
                  ; B.putMVar mvar "hubl1: Gronk!"
                  ; tc <- B.threadCount
                  ; B.putStrLn ( "=== hubl1' threadId " ++ show tid' ++
                                 "\t\t(" ++ show n ++ " / " ++ show tc ++ ") ")
                  ; if n < loops
                    then hubl1' (n + 1) mvar
                    else return ()
                  }

-- Another non-local braid
gs1mvar :: Int -> Int -> B.MVar String -> B.Braid Gs Ls ()
gs1mvar loops j mvar =
  do { tid <- B.myThreadId
--     ; B.putStrLn ( "=== gs1mvar, taking: " ++ show mvar )
     ; val <- B.takeMVar mvar
     ; B.putStrLn ( "=== gs1mvar, got: " ++ show val )
     ; B.putMVar mvar "gs1mvar: Gronk!"
     ; B.putStrLn "=== gs1mvar: After putMVar"
     ; t   <- B.getElapsed
     ; B.threadDelay 1000000
     ; tc <- B.threadCount
     ; B.putStrLn ( "=== gs1mvar, threadId " ++ show tid ++
                    "\t\t(" ++ show j ++ " / " ++ show tc ++ ") " ++ show t )
     ; if j < loops
       then gs1mvar loops (j+1) mvar
       else do { B.putStrLn ( "gs1mvar done!" )
               ; return ()
               }
     }

-- A local thread for the level 1 braid
-- Takes mvar2
ls1mvar :: Int -> Int -> B.MVar String -> L.Thread Ls ()
ls1mvar loops j mvar =
  do { tid <- L.myThreadId
     ; t   <- L.getElapsed
     ; val <- L.takeMVar mvar
     ; L.putStrLn ( "=== ls1mvar: got: " ++ show val )
     ; L.putMVar mvar "=== ls1mvar: Zonk!"
     ; L.putStrLn ( "=== ls1mvar threadId " ++ show tid ++
                    "\t\t(" ++ show j ++ ") " ++ show t )
     ; L.threadDelay 1000000
     ; if j < loops
       then ls1mvar loops (j + 1) mvar
       else do { L.putStrLn ( "ls1mvar done!" )
               ; return ()
               }
     }

main :: IO ()
main =
  do { putStrLn ("Test8: Testing MVars in the layered braid")
     ; args <- getArgs
     ; elapsed <- IOU.getElapsed
     ; B.mkBraid "hubl1"
                 mkNull
                 (fromEnum elapsed)
                 B.scheduleAndUpdate
                 (BI.mkUnliftedProg (hubl1 (parseArgs args)))
     ; putStrLn ( "Test 8 passes" )
     ; exitWith ExitSuccess
     }
