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
     ; hubl1loop loops 0 chan1
     ; return ()
     }

hubl1loop :: Int -> Int -> C.Chan String -> B.Braid Gs Ls ()
hubl1loop loops n chan =
  do { C.writeChan chan ( "Message-" ++ show n )
     ; msg <- C.readChan chan
     ; B.putStrLn ( "msg = " ++ msg )
     ; B.nullPause :: B.Braid Gs Ls ()
     ; if n < loops
       then hubl1loop loops (n + 1) chan
       else return ()
     }

main :: IO ()
main = startup 12 "hubl1" hubl1
