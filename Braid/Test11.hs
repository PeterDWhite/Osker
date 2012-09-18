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
-- Channel imports
import qualified OskerChan as C

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid ++
                    ", loops = " ++ show loops
                  )
     ; chan1 <- C.newChan "hubl1-self"
     ; B.putStrLn ( "chan1 = " ++ show chan1 )
     ; C.writeChan chan1 "Message-1"
     ; msg <- C.readChan chan1
     ; B.putStrLn ( "msg = " ++ msg )
     ; return ()
     }

main :: IO ()
main = startup 11 "hubl1" hubl1
