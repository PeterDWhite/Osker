
-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003

-----------------------------------------------------------------------
-- The data required for a channel
-- This file is used for both lifted and unlifted versions of channels
-----------------------------------------------------------------------

module PreChan
    ( Chan (..)   -- Channel abstraction
    , Stream (..) -- In support of the channel abstraction
    , ChItem (..) -- In support of the channel abstraction
    , writeEnd    -- Get the writing end of the channel
    , readEnd     -- Get the reading end of the channel
    ) where

-- Haskell imports
import Dynamic (Typeable, TyCon, mkTyCon, typeOf, mkAppTy)
-- Local imports
import qualified BraidMVar as MV

-- A channel is represented by two @MVar@s keeping track of
-- the two ends of the channel contents,i.e.,  the read
-- and write ends. Empty MVars are used to handle consumers
-- trying to read from an empty channel.
-- Osker: Adds a name to the channel, for debugging.
data Chan a
    = Chan { chanName  :: String              -- Name of the channel
           , chanRead  :: (MV.MVar (Stream a)) -- Read end
           , chanWrite :: (MV.MVar (Stream a)) -- Write end
           }

-- Construct name of MVar write end
writeEnd :: String -> String
writeEnd name = name ++ "/write"

-- Construct name of MVar read end
readEnd :: String -> String
readEnd name = name ++ "/read"

instance Show (Chan a) where
    show chan = "Chan: "  ++ chanName chan ++
                "\n\tR("  ++ show (chanRead chan) ++
                ")\n\tW(" ++ show (chanWrite chan) ++ ")"

-- A stream is a mutable variable, representing the head of the list.
-- The value held there has a pointer, which points to the rest of
-- the list. Thus we have a singly linked list.
data Stream a = Stream { streamVar :: (MV.MVar (ChItem a)) } deriving (Eq)

instance Show (Stream a) where
    show _ = "Stream *"

data ChItem a = ChItem a (Stream a)

instance Show (ChItem a) where
    show _ = "ChItem *"

kStreamCon :: TyCon
kStreamCon = mkTyCon "Stream"
instance Typeable (Stream a) where
    typeOf _ = mkAppTy kStreamCon []

kChItemCon :: TyCon
kChItemCon = mkTyCon "ChItem"
instance Typeable (ChItem a) where
    typeOf _ = mkAppTy kChItemCon []

kChanCon :: TyCon
kChanCon = mkTyCon "Chan"
instance Typeable (Chan a) where
    typeOf _ = mkAppTy kChanCon []
