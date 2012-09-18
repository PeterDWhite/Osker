-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where

--Haskell imports
import Dynamic ( TyCon, mkTyCon, Typeable, typeOf, mkAppTy )
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
import qualified Hub as H
import qualified Responder as R

data ChannelId = NullChannel | HubChannel deriving (Eq, Ord, Ix)
instance Null ChannelId where
    mkNull = NullChannel

data TestMsg = TestMsg { pay :: String, msgChan :: C.Chan TestMsg }
instance R.Message TestMsg
instance R.ResponderMessage TestMsg where
    responseChannel = msgChan

-- Make an TestMsg a dynamic type
testMsgCon :: TyCon
testMsgCon = mkTyCon "TestMsg"
instance Typeable TestMsg where
    typeOf _ = mkAppTy testMsgCon []

type SpokeType a     = S.Spoke Ls ChannelId TestMsg a
type SpokeBoundsType = S.SpokeBounds Ls ChannelId TestMsg
type PreSpokeType    = SpokeBoundsType -> SpokeType ()
type ChannelType     = S.Channels ChannelId TestMsg

hub :: H.Hub Gs Ls
hub = H.mkHub
        HubChannel 
        []
        [spoke1]
        hubl1Prog

hubl1 :: Int -> B.Braid Gs Ls ()
hubl1 _loops = H.toBraid hub

hubl1Prog :: B.Braid Gs Ls ()
hubl1Prog =
  do { tid <- B.myThreadId
     ; B.putStrLn ( ">>> hubl1, my tid = " ++ show tid )
     ; B.putStrLn ( "=== hubl1 is done" )
     }

-- A lifted thread for the level 1 braid
-- First argument is max loop count
-- Second argument is current loop count
-- Third argument is the delay
spoke1 :: PreSpokeType
spoke1 b =
  do { S.putStrLn b ( "=== ls1: Spoke running" )
     ; return ()
     }

main :: IO ()
main = startup 2 "hubl1" hubl1
