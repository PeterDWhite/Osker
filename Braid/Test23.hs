-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--------------------------------------------------------------------
-- Test a lifted exception throw, with no exception handler.
--------------------------------------------------------------------

--Haskell imports
import Monad
-- Test imports
import TestSupport
-- Braid imports
import qualified BraidExternal as B
import qualified BraidLocal as L

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { B.putStrLn ( "Test a lifted exception" )
     ; tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
     ; tid3     <- B.forkLift "Ls1" lsE
     ; B.putStrLn ( "=== hubl1: lsChan2 forked, tid = " ++ show tid3 )
     ; B.putStrLn ( "=== hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
-- The program should continue execution after the exception,
-- since there is no exception handler.
lsE :: L.Thread Ls ()     -- A lifted thread
lsE =
  do { tid <- L.myThreadId
     ; L.putStrLn ( "=== lsE    threadId " ++ show tid )
     ; L.throw L.NotImplemented
     ; L.putStrLn ( "=== lsE is done!" )
     }

main :: IO ()
main = startup 23 "hubl1" hubl1
