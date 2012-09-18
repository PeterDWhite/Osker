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
-- Argument is the maximum number of loops
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { tid  <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; tid2  <- B.fork "Gs1" (gs1 loops)
     ; B.putStrLn ( ">>> Gs1 forked, tid = " ++ show tid2 )
     ; hubloop loops 0 1000000
     ; B.putStrLn ("... hubl1' is done" )
     }

main :: IO ()
main = startup 3 "hubl1" hubl1
