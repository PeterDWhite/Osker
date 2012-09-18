-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module MQ
    ( MessageQueueDescriptor (..) -- Data type for message queue desc
    , MessageQueue (..)           -- Data type for message queue
    , MessageQueueEntry (..)      -- Entry in the message queue
    , addEntry                    -- Add an entry to a message queue
    , MessageQueueMap             -- Map global name to message queue
    , outMessageQueueMap          -- Format msg q map for printing
    , emptyMQ                     -- Empty message queue
    , emptyMQM                    -- Empty message queue map
    , emptyMQD                    -- Empty message queue descriptor
    , checkWriteAccess            -- See if write access can be granted
    , checkReadAccess             -- See if read access can be granted
    ) where

----------------------------------------------------------------------
-- The trap message defines the requests and response of the trap
-- handler thread
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
import Prelude hiding (IO)
-- Graph imports
import qualified Deep as D
-- Domain imports
import qualified DomainGraph as DOMG
-- Posix imports
import qualified OpenFlags as OF
import qualified MessageQueueData as MQD
import qualified Mode as M
import qualified Resource as R
import qualified ProcessName as PN

----------------------------------------------------------------------
-- Control blocks in support of the message queue interfaces
----------------------------------------------------------------------

-- A given process can only open a message queue once (just like a file)
data MessageQueueDescriptor = --
    MessageQueueDescriptor
    { mqdName  :: MQD.MQName   -- Global name of the message queue
    , mqdFlags :: OF.OpenFlags -- Read, write, etc.
    , mqdAttrs :: MQD.MQAttr   -- Attributes of the message queue
    } deriving (Show)

instance R.Resource MessageQueueDescriptor where
    zeroize mqd = mqd { mqdName  = MQD.MQLocal (MQD.Name [""])
                      , mqdFlags = []
                      , mqdAttrs = MQD.initMQAttrs
                      }

-- An empty message queue descriptor
emptyMQD :: MessageQueueDescriptor
emptyMQD =  MessageQueueDescriptor
            { mqdName  = MQD.MQLocal (MQD.Name [""])
            , mqdFlags = []
            , mqdAttrs = MQD.initMQAttrs
            }

-- An entry in the message queue
data MessageQueueEntry =
    MessageQueueEntry
    { msgSource   :: PN.ProcessName
    , msgContents :: MQD.Message
    }

instance Show MessageQueueEntry where
    show mqe =
      PN.outProcessName (msgSource mqe) ++ "-" ++ "->" ++
      MQD.outMessage (msgContents mqe)

-- A message queue currently uses buffers of length 128
data MessageQueue =
    MessageQueue
    { mqName      :: MQD.MQName   -- Globalname of the message queue
    , mqSenders   :: [PN.ProcessName] -- Permitted senders
    , mqReceivers :: [PN.ProcessName] -- Permitted receivers
    , mqMessages  :: [MessageQueueEntry] -- Contents of the queue
    , mqMode      :: M.Mode        -- Create mode (r, w, ...)
    , mqFlags     :: OF.OpenFlags  -- Read, write, etc.
    , mqAttrs     :: MQD.MQAttr    -- Attributes of the message queue
    } deriving (Show)

-- Add an entry to a message queue
addEntry ::
    MessageQueue -> PN.ProcessName -> MQD.Message -> MessageQueue
addEntry mq name msg =
  let newmsg = MessageQueueEntry
               { msgSource = name
               , msgContents = msg
               }
  in mq { mqMessages = (mqMessages mq) ++ [newmsg] }

-- Construct an empty message queue
emptyMQ ::
    MQD.MQAttr -> OF.OpenFlags -> M.Mode -> MQD.MQName -> MessageQueue
emptyMQ attr flags mode name =
  MessageQueue
  { mqName      = name
  , mqSenders   = []
  , mqReceivers = []
  , mqMessages  = []
  , mqMode      = mode
  , mqFlags     = flags
  , mqAttrs     = attr
  }

-- The creating process maintains a map of message queues
type MessageQueueMap = FM.FiniteMap MQD.MQName MessageQueue

emptyMQM :: MessageQueueMap
emptyMQM = FM.emptyFM

outMessageQueueMap :: MessageQueueMap -> String
outMessageQueueMap mqm = show (FM.fmToList mqm)

-- Check if an opener should get write access
-- This means check that the sender has permission to send
-- to all receivers
checkWriteAccess ::
    MessageQueue ->     -- The message queue to check
    DOMG.DomainGraph -> -- The process / domain model
    PN.ProcessName ->   -- The name of the potential sender
    Bool
checkWriteAccess mq graph pname =
  let receivers = mqReceivers mq
      -- Set up for call to isEdges
      edges = map (\r -> (pname, r)) receivers
  in D.isEdges graph edges

-- Check if an opener should get read access
-- This means check that the receiver has permission to receive
-- from all senders
checkReadAccess ::
    MessageQueue ->     -- The message queue to check
    DOMG.DomainGraph -> -- The process / domain model
    PN.ProcessName ->   -- The name of the potential sender
    Bool
checkReadAccess mq graph pname =
  let senders = mqSenders mq
      -- Set up for call to isEdges
      edges = map (\s -> (s, pname)) senders
  in D.isEdges graph edges
