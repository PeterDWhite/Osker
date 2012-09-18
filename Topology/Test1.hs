-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--Haskell imports
import Monad
import Ix
-- Utility imports
import Null
-- Test imports
import TestData
import TestSupport
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import qualified OskerChan as C
-- Topology imports
import qualified Spoke as S

data ChannelId = NullChannel deriving (Eq, Ord, Ix)
instance Null ChannelId where
    mkNull = NullChannel

type SpokeType a     = S.Spoke Ls ChannelId String a
type SpokeBoundsType = S.SpokeBounds Ls ChannelId String
type ChannelType     = S.Channels ChannelId String

-- The hub of the level 1 braid
-- This is not a lifted thread.
hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 _loops =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; chan <- C.newChan "Spoke null channel"
     ; let channels :: ChannelType     = S.initChannels chan
           sp1 = S.mkSpoke channels spoke1
     ; tid2 <- B.forkLift "Sp1" sp1
     ; B.putStrLn ( "=== Sp1/Ls1 is forkLifted, tid = " ++ show tid2 )
     ; B.putStrLn ( "=== hubl1 is done" )
     ; return ()
     }

-- A lifted thread for the level 1 braid
-- First argument is max loop count
-- Second argument is current loop count
-- Third argument is the delay
spoke1 :: SpokeBoundsType -> SpokeType ()
spoke1 b =
  do { S.putStrLn b ( "=== ls1: Spoke running" )
     ; return ()
     }

main :: IO ()
main = startup 1 "hubl1" hubl1
