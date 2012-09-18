-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--------------------------------------------------------------------
-- Test an unlifted execption throw, with a catcher
--------------------------------------------------------------------

--Haskell imports
import Monad
-- Test imports
import TestSupport
-- Braid imports
import qualified BraidExternal as B

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { B.putStrLn ( "Test a lifted exception" )
     ; tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
     ; tid2  <- B.fork "Gs1" gsE
     ; B.putStrLn ( "=== hubl1: gsE forked, tid = " ++ show tid2 )
     ; B.putStrLn ( "=== hubl1 is done!" )
     ; return ()
     }

-- Should execute to the end, since there is no exception handler.
gsE :: B.Braid Gs Ls ()
gsE =
  B.catch ( do { tid <- B.myThreadId
               ; B.putStrLn ( ">>> gsE, my tid = " ++ show tid )
               ; B.showState "gsE"
               ; B.throw B.NotImplemented
               ; B.putStrLn ("Fail: should not get here" )
               }
          )
          ( \e -> ( do { B.putStrLn ( "gsE: Got exception: " ++ show e )
                       ; B.putStrLn ( "=== gsE done!" )
                       }
                  )
          )

main :: IO ()
main = startup 25 "hubl1" hubl1
