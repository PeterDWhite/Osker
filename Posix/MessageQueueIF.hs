-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module MessageQueueIF
    ( mq_open      -- Posix interface
    , mq_close     -- Posix interface
    , mq_send      -- Posix interface
    , mq_receive   -- Posix interface
    , mq_notify    -- Posix interface
    , mq_getattr   -- Posix interface
    , mq_setattr   -- Posix interface
    , mq_unlink    -- Posix interface
    , M.Mode (..)
      -- Re-export message queue data stuff
    , MQD.MQName (..)
    , MQD.Name (..)
    , MQD.MQDesc
    , MQD.errMQDesc
    , MQD.MQAttr (..)
    , MQD.Message (..)
    , SZ.Size (..)
      -- Re-export open flags stuff
    , OF.OpenFlag (..)
    , OF.OpenFlags
    ) where

----------------------------------------------------------------------
-- The POSIX.4 message queue interfaces
----------------------------------------------------------------------

-- POSIX imports
import qualified SystemCall as SC
import qualified MessageQueueData as MQD
import qualified Mode as M
import qualified Size as SZ
import qualified OpenFlags as OF

----------------------------------------------------------------------
-- mq_open creates a new or opens an existing message queue.
--
-- The message queue name is constructed per the filename
-- specification. The POSIX.4 specification permits the message
-- queue to appear in the file name space. In particular, under
-- Osker, the message queues will appear in the ls command. The
-- message queues will be listed in the current working directory
-- of the process.
--
-- The open flags can specify write (send), read (receive) or both.
-- In addition, the open flags can specify O_NONBLOCK, which means
-- that Osker will not block when sending to a full message queue,
-- or receiving from an empty message queue. O_CREAT specifies
-- that the message queue is to be created. If O_EXCL is set, then
-- the create will fail if the message queue already exists. If
-- O_EXCL is not set, then the create call will just attach to an
-- existing queue of the same name, if it already exists.
--
-- The geometry (size of messages, depth of queue) is set by the
-- message queue attributes parameter.
----------------------------------------------------------------------
mq_open ::
    MQD.MQName    ->       -- Name of the message queue to open.
    OF.OpenFlags  ->       -- Create, etc.
    M.Mode        ->       -- Permissions to set on the message queue
    MQD.MQAttr    ->       -- Geometry of the message queue
    SC.U SC.SystemResponse -- Sometimes returns the existing sigaction
mq_open filename flags mode attr =
  SC.osker (SC.MqOpenReq filename flags mode attr)

----------------------------------------------------------------------
-- mq_close() severs the connection to a message queue created with
-- mq_open().
-- The message queue will persist even if all connections to it
-- are closed. The only interface that terminates a message queue
-- is mq_unlink().
----------------------------------------------------------------------
mq_close ::
    MQD.MQDesc    ->       -- Message queue to close
    SC.U SC.SystemResponse -- Error indication
mq_close desc = SC.osker (SC.MqCloseReq desc)

-- mq_send sends a message on a message queue
mq_send ::
    MQD.MQDesc    ->      -- Message queue on which to send
    MQD.Message   ->      -- Message to send
    SZ.Size       ->      -- Size of the message
    Int           ->       -- Priority of the message
    SC.U SC.SystemResponse -- Error indication returned
mq_send desc msg size pri = SC.osker (SC.MqSendReq desc msg size pri)

-- mq_receive receives a message from a message queue
-- The returned message will be a null string when an
-- error is returned.
mq_receive ::
    MQD.MQDesc    ->       -- Message queue from which to receive
    SC.U SC.SystemResponse -- Error indication, and message
mq_receive desc = SC.osker (SC.MqReceiveReq desc)

-- mq_notify gives an asynchronous notificaiton when a message
-- is received.
mq_notify ::
    MQD.MQDesc    ->       -- Message queue from which to notify
    SC.U SC.SystemResponse -- Error indication, and message
mq_notify desc = SC.osker (SC.MqNotifyReq desc)

-- mq_getattr returns the attributes of the message queue
mq_getattr ::
    MQD.MQDesc    ->       -- Message queue from which to getattr
    SC.U SC.SystemResponse -- Attributes, error indication
mq_getattr desc = SC.osker (SC.MqGetAttrReq desc)

-- mq_setattr returns the attributes of the message queue
mq_setattr ::
    MQD.MQDesc    ->       -- Message queue from which to getattr
    MQD.MQAttr    ->       -- New attributes to set
    SC.U SC.SystemResponse -- Old Attributes, error indication
mq_setattr desc attr = SC.osker (SC.MqSetAttrReq desc attr)

-- mq_unlink destroys a message queue
mq_unlink ::
    MQD.MQName    -> -- Message queue to unlink
    SC.U SC.SystemResponse
mq_unlink name = SC.osker (SC.MqUnlinkReq name)
