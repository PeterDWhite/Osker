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
     ; tohl1 <- C.newChan "to hubl1"
     ; fromhl1 <- C.newChan "from hubl1"
     ; tid2  <- B.fork "GsChan" (gsChan loops 0 tohl1 fromhl1)
     ; B.putStrLn ("... gsChan forked, tid = " ++ show tid2 )
     ; hubl1loop loops 0 tohl1 fromhl1
     ; B.putStrLn ("... hubl1 is done" )
     ; return ()
     }

hubl1loop ::
    Int           -> -- Max loop count
    Int           -> -- Current loop count
    C.Chan String -> -- Channel to the hub
    C.Chan String -> -- Channel from the hub
    B.Braid Gs Ls ()
hubl1loop loops n tohl1 fromhl1 =
  do { C.writeChan fromhl1 ( "From hubl1-" ++ show n )
     ; msg <- C.readChan tohl1
     ; B.putStrLn ( "hubl1 rcv msg  = " ++ msg )
     ; if n < loops
       then hubl1loop loops (n + 1) tohl1 fromhl1
       else return ()
     }

-- An unlifted thread for testing channels
gsChan ::
    Int           -> -- Max loop count
    Int           -> -- Current loop count
    C.Chan String -> -- Channel to the hub
    C.Chan String -> -- Channel from the hub
    B.Braid Gs Ls ()
gsChan loops n tohl1 fromhl1 =
  do { C.writeChan tohl1 ( "To hubl1-" ++ show n )
     ; msg <- C.readChan fromhl1
     ; B.putStrLn ( "gsChan rcv msg = " ++ msg )
     ; if n < loops
       then gsChan loops (n + 1) tohl1 fromhl1
       else do { B.putStrLn ( "... gs2Chan is done" )
               ; return ()
               }
     }

main :: IO ()
main = startup 13 "hubl1" hubl1
