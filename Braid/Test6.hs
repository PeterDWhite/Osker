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
     ; tidHubl2 <- B.bundle "hubl2" (hubl2 loops)
     ; B.putStrLn ( "**** Tid of Hubl2 = " ++ show tidHubl2 )
     ; _tid2 <- B.forkLift "Ls1" (ls1 loops 0 1000000)
     ; B.putStrLn ( "**** Ls1 is forkLifted" )
     ; B.putStrLn ( "Now looping hubl1'" )
     ; hubloop loops 0 1000000
     ; B.putStrLn ( "hubl1' done!" )
     }

-- Hub for level 2 braid
hubl2 :: Int -> B.Braid Ls Ms ()
hubl2 loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl2 threadId = " ++ show tid )
     ; B.putStrLn ( "Now looping hubl2'" )
     ; hubloop loops 0 1000000
     ; B.putStrLn ( "hubl2' done!" )
     }

main :: IO ()
main = startup 6 "hubl1" hubl1
