-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--Haskell imports
import Monad
-- Test imports
import TestSupport
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; tid2 <- B.forkLift "Ls1" (ls1 loops 0 1000000)
     ; B.putStrLn ( "**** Ls1 is forkLifted, tid = " ++ show tid2 )
     ; hubloop loops 0 1000000
     ; B.putStrLn ( "hubl1loop done!" )
     }

main :: IO ()
main = startup 5 "hubl1" hubl1
