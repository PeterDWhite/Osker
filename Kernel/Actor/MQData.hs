-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module MQData
    ( MqdMap (..)  -- Data type for message queue descriptor map
    , emptyMqdMap  -- Build an empty MqdMap
    , lookupMq     -- Lookup value in message queue descriptor map
    , deleteMq     -- Delete an entry from the message queue desc map
    , filterMq     -- FIlter down a message queue descriptor map
    , nullMq       -- Determine if Mqmap is empty
    ) where

----------------------------------------------------------------------
-- Data in support of Posix message queue calls.                    --
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Posix imports
import qualified MessageQueueData as MQD
import qualified MQ as MQ

data MqdMap =
    MqdMap
    { mqdNext :: MQD.MQDesc -- Next message queue descriptor to use
    , mqdMap  :: FM.FiniteMap MQD.MQDesc MQ.MessageQueueDescriptor
    }

-- Build an empty message queue descriptor map
emptyMqdMap :: MqdMap
emptyMqdMap = MqdMap { mqdNext = 0
                     , mqdMap = FM.emptyFM
                     }

-- Determine if message queue map is empty
nullMq :: MqdMap -> Bool
nullMq mqdmap = mqdNext mqdmap == 0

-- Lookup a value in the message queue map
lookupMq :: MqdMap -> MQD.MQDesc -> Maybe MQ.MessageQueueDescriptor
lookupMq mqdmap desc =
  let fm = mqdMap mqdmap
  in FM.lookupFM fm desc

-- Delete from a message queue
deleteMq :: MqdMap -> MQD.MQDesc -> MqdMap
deleteMq mqdmap desc =
  let fm = mqdMap mqdmap
      fm' = FM.delFromFM fm desc
  in mqdmap { mqdMap = fm' }

instance Show MqdMap where
    show mqdmap = "("  ++ show (mqdNext mqdmap) ++ ", " ++
                  show (FM.fmToList (mqdMap mqdmap)) ++ ")"

-- Filter a message queue
filterMq ::
    MqdMap ->
    (MQD.MQDesc -> MQ.MessageQueueDescriptor -> Bool) ->
    MqdMap
filterMq mqdmap finder =
  let fm = mqdMap mqdmap
      fm' = FM.filterFM finder fm
  in mqdmap { mqdMap = fm' }
