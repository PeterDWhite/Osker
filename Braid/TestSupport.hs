-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module TestSupport
    ( module TestData    -- Re-export test data
    , gs1                -- Braid with global state
    , ls1                -- Braid with local state
    , ms1                -- Braid with micro state
    , startup            -- Startup program for test case
    , hubloop            -- Loop for hub program
    , skip               -- Skip a time slice
    , parseArgs          -- Parse command line arguments to test program
    ) where

-- Hsskell imports
import System (getArgs, exitWith, ExitCode(..))
-- Utility imports
import Null
import qualified IOUtilities as IOU
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import qualified BraidLocal as L
import TestData

-- Parse the arguments coming in to the test program
parseArgs :: [String] -> Int
parseArgs []     = 8 -- Default loop count
parseArgs [a]    = fst (head (reads a))
parseArgs as     =
  error ( "Too many arguments (" ++ show (length as) ++ "to Test2" )

-- A startup function good for many of the test cases.
startup :: Int -> String -> (Int -> B.Braid Gs Ls ()) -> IO ()
startup testNum hubName hub =
  do { args <- getArgs
--     ; putStrLn ("Args: " ++ show args ++ " ... " ++ show (parseArgs args))
     ; elapsed <- IOU.getElapsed
     ; B.mkBraid hubName
                 mkNull
                 (fromEnum elapsed)
                 B.scheduleAndUpdate
                 (BI.mkUnliftedProg (hub (parseArgs args)))
     ; putStrLn ( "Test " ++ show testNum ++ " passes")
     ; exitWith ExitSuccess
     }

-- A generic unlifted thread
-- Argument is the max loop count.
gs1 :: Int -> B.Braid Gs Ls ()
gs1 loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> gs2, my tid = " ++ show tid )
     ; gs1loop loops 0
     ; B.putStrLn ("... gs2' is done" )
     }

-- The inner loop of unlifted thread 1
gs1loop :: Int -> Int -> B.Braid Gs Ls ()
gs1loop loops n =
  do { tid' <- B.myThreadId
     ; B.putStrLn ( "gs2' threadId " ++ show tid' ++ "\t\t(" ++ show n ++ ")" )
     ; B.threadDelay 1000000
     ; if n < loops
       then gs1loop loops (n + 1)
       else return ()
     }

-- The inner loop for generic hub
hubloop :: Int -> Int -> Int -> B.Braid u l ()
hubloop loops n delay =
  do { tid' <- B.myThreadId
     ; B.putStrLn ( "hubl1' threadId " ++ show tid' ++ "\t\t("++ show n ++")" )
     ; B.threadDelay delay
     ; if n < loops
       then hubloop loops (n + 1) delay
       else return ()
     }

-- A lifted thread for the level 1 braid
-- First argument is max loop count
-- Second argument is current loop count
-- Third argument is the delay
ls1 :: Int -> Int -> Int -> L.Thread Ls ()
ls1 loops j delay =
  do { tid <- L.myThreadId
     ; t   <- L.getElapsed
     ; L.putStrLn ( "ls1    threadId " ++ show tid ++
                    "\t\t(" ++ show j ++ ") " ++ show t )
     ; L.threadDelay delay
     ; if j < loops
       then ls1 loops (j + 1) delay
       else do { L.putStrLn ("ls1 done!")
               ; return ()
               }
     }

-- A micro thread for the braid
ms1 :: Int -> Int -> Int -> L.Thread Ms ()
ms1 loops j delay =
  do { tid <- L.myThreadId
     ; t   <- L.getElapsed
     ; L.putStrLn ( "ms1    threadId " ++ show tid ++
                    "\t(" ++ show j ++ ")" ++ " " ++ show t )
     ; L.threadDelay delay
     ; if j < loops
       then ms1 loops (j + 1) delay
       else do { L.putStrLn ("ms1 done!")
               ; return ()
               }
     }

-- Skip a beat
skip :: B.Braid g l ()
skip = B.threadDelay 10000
