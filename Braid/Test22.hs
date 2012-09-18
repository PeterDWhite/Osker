-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--------------------------------------------------------------------
-- Test a lifted execption throw
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
     ; tid3     <- B.forkLift "LsE" (lsE loops 0 100)
     ; B.putStrLn ( "=== hubl1: lsE forked, tid = " ++ show tid3 )
     ; B.putStrLn ( "=== hubl1 is done!" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
lsE ::
    Int             -> -- Max loop count
    Int             -> -- Current loop count
    Int             -> -- Delay count
    L.Thread Ls ()     -- A lifted thread
lsE _loops _j _delay =
  L.catch ( do { tid <- L.myThreadId
               ; L.putStrLn ( "=== lsE threadId " ++ show tid )
               ; L.throw L.NotImplemented
               ; L.putStrLn ( "Fail: should not get here" )
               ; return ()
               }
          )
          ( \e -> ( do { L.putStrLn ( "lsE: Got exception: " ++ show e )
                       ; L.putStrLn ( "=== lsE done!" )
                       }
                  )
          )

main :: IO ()
main = startup 22 "hubl1" hubl1
