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
hubl1 _loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; B.threadDelay 200000
     ; B.showState "done"
     ; return ()
     }

main :: IO ()
main = startup 10 "hubl1" hubl1
