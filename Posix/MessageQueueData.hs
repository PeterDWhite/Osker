-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module MessageQueueData
    ( MQName (..)     -- Data type for message queue name
    , projLocal       -- Get the underlying local components
    , projLocalString -- Get the underlying local showable name
    , getLocal        -- Get the local part of a name
    , isGlobal        -- Determine if a name is global
    , isLocal         -- Determine if a name is local
    , makeGlobal      -- Make a name global
    , MQDesc          -- Type for message queue descriptor
    , errMQDesc       -- Error return for open
    , MQAttr (..)     -- Data type for message queue attributes
    , initMQAttrs     -- Initialize message queue attributs
    , Message (..)    -- Type used for contents of a message
    , outMessage      -- Show the message without decoration
    , liftFilter      -- Lift a Krenz filter to a message
      -- Supporting re-exports
    , N.Name (..)
    ) where

----------------------------------------------------------------------
-- Data structures in support of the POSIX.4 message queue interfaces
----------------------------------------------------------------------

-- Posix imports
import qualified Name as N
import qualified Krenz as K

-- A message queue name can be local "no prepended process name"
-- or global (has a prepended process name)
data MQName
    = MQLocal N.Name
    | MQGlobal
      { globalPart :: N.Name
      , localPart  :: N.Name
      }
    deriving (Eq, Ord, Show)

-- Make a name global
makeGlobal :: N.Name -> MQName -> MQName
makeGlobal gname mqname =
  if isGlobal mqname
  then mqname
  else MQGlobal
       { globalPart = gname
       , localPart = projLocalName mqname
       }

-- Determine if a name is global
isGlobal :: MQName -> Bool
isGlobal (MQGlobal _ _) = True
isGlobal (MQLocal _)    = False

-- Determine if a name is local
isLocal :: MQName -> Bool
isLocal (MQGlobal _ _) = False
isLocal (MQLocal _)    = True

-- Get the underlying local name
projLocalName :: MQName -> N.Name
projLocalName (MQLocal name) = name
projLocalName (MQGlobal _glob loc) = loc

-- Get the underlying local components
projLocal :: MQName -> [String]
projLocal (MQLocal name) = N.projName name
projLocal (MQGlobal _glob loc) = N.projName loc

-- Get the underlying local name string
projLocalString :: MQName -> String
projLocalString (MQLocal name) = N.projNameString name
projLocalString (MQGlobal _glob loc) = N.projNameString loc

-- Get the local part of a name
getLocal :: MQName -> MQName
getLocal local@(MQLocal _name) = local
getLocal (MQGlobal _glob loc) = MQLocal loc

-- A message queue name is local when the first part of the name
-- matches a local process name.

-- POSIX standard required that a message descriptor be convertible
-- to Int, thus the Enum instance is a requirement
type MQDesc = Int

-- Per Posix, return -1 on open for descriptor when there is error
errMQDesc :: MQDesc
errMQDesc = -1

-- Message queue attributes, returned by a create / open call
-- to the message queue.
data MQAttr
    = MQAttr { mqMaxmsg  :: Int -- Maximum number of messages in the queue
             , mqMsgsize :: Int -- Maximum size of a single message
             , mqFlags   :: Int -- Modifies behaviour of the message queue
             , mqCurmsgs :: Int -- Number of messages currently in the queue
             }
    | NullAttr deriving (Show)

initMQAttrs :: MQAttr
initMQAttrs = MQAttr { mqMaxmsg  = 0
                     , mqMsgsize = 0
                     , mqFlags   = 0
                     , mqCurmsgs = 0
                     }

-- The type of messages that can be sent. For now, just use a
-- string, which is a good approximation to an array of characters
newtype Message = Message String

instance Show Message where
    show (Message s) = "Message:" ++ show s

-- Show the message without decoration
outMessage :: Message -> String
outMessage (Message s) = s

-- Lift a krenz filter to a message
liftFilter :: K.Filter -> (Message -> Message)
liftFilter (K.Filter f) = \(Message s) -> Message (f s)
