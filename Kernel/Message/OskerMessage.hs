-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module OskerMessage
    ( OskerPay (..)               -- Payload carried by Osker messages
    , OskerMsg                    -- Messages passed around in kernel
      -- Specialize to OskerChannel
    , OskerChannel                -- Channel of Osker messages
      -- Executive message stuff.
    , M.dest                      -- Get the destination of a message
    , M.pay                       -- Get message payload
    -- Some useful utilities
    , getSysReq                   -- Get system request from user half inputs
    , getKernelCoreReq            -- Get kernel core request out of input msg
    , getKernelCoreRsp            -- Get kernel core response from input msg
    , getKernelCoreCommand        -- Get kernel core command from Osker payload
    , getKernelCoreComply         -- Get kernel core comply for responses sys/2
    , getToProcessControlReq      -- Get process control req out of a payload
    , getFromProcessControlReq    -- Get pcr out of a payload
      -- Kernel core requests
    , KernelCoreRequest (..)      -- Data type for requests to the kernel core
    , KernelCoreRequestCode (..)  -- Code for kernel core requests
    , KernelCoreResponse (..)     -- Responses from the kernel core
    , KernelCoreResponseCode (..) -- Code for responses from the kernel core
    , KernelCoreCommand (..)      -- Kernel core command to system half
    , KernelCoreCommandCode (..)  -- Code for kernel core command
    , KernelCoreComply (..)       -- Response to kernel core command
    , KernelCoreComplyCode (..)   -- Code for response to kernel core command
    , kernelCoreRequest2Int       -- Convert kernel core req code to integer
    , kernelCoreCommand2Int       -- Convert kernel core cmd code to integer
    , kernelCoreComply2Int        -- Convert kernel core comply code to int
    , ResourceSpecial (..)
    , DataBlock (..)
      -- Process control requests
    , ProcessControlRequest (..)
    , ProcessControlRequestCode (..)
    , processControlRequest2Int
      -- Timer control block requests
    , GRReqTimer (..)
    , GRRspTimer (..)
      -- Timer device messages
    , IOReqTimer (..)
    , IORspTimer (..)
      -- Standard IO device messages
    , IOReqSIO (..)
    , IORspSIO (..)
      -- File system IO device messages
    , IOReqFS (..)
    , IORspFS (..)
      -- File system core messages
    , FSCoreReq (..)     -- Request to the file system core
    , FSCoreRsp (..)     -- Response from the file system core
      -- Demo exports
    , SC.OskerCommand (..)
      -- Message combinators
    , nullOpenMQRsp      -- Make open MQ response with just an error msg
    ) where

----------------------------------------------------------------------
-- The Osker message is the message type passed around by the kernel,
-- amongst the various kernel threads.
--
-- The OskerPay is a big union of all the types of payload that
-- can be carried by the message. An important structuring concept
-- is that the executive message is polymorphic in the payload type,
-- thus the executive, which oversees the actor, is ignorant
-- of the message payload. Therefore the complexity of this message
--
----------------------------------------------------------------------}

-- Haskell imports
import qualified Dynamic as DYN
-- Domain imports
import qualified DomainGraph as DOMG
import qualified GlobalResource as GR
-- Posix imports
import qualified SystemCall as SC
import qualified Message as M
import qualified ProcessId as PID
import qualified ProcessName as PN
import qualified Errno as ER
import qualified MessageQueueData as MQD
import qualified Mode as M
import qualified OpenFlags as OF
import qualified Errno as ER
import qualified FileName as FN
import qualified DirectoryData as DD
-- Kernel globals
import qualified ExceptionData as ED
-- Osker imports
import qualified ProcessTags as PT
import qualified TimerControlBlock as TCB
import qualified Message as M

-- Osker message, with payload as a big union of types
type OskerMsg = M.Message OskerPay

-- The big union Osker message type. The executive is ignorant of
-- this type, being polymorphic in the payload of the messages, thus
-- the proofs of separation of the executive need not depend on all
-- the details of this message type.
data OskerPay
    -- Kernel core traffic, in response to the system half
    = ToKernelCore        KernelCoreRequest
    | FromKernelCore      KernelCoreResponse
    -- Timer traffic, in response to the system half
    | ToTimerDevice       IOReqTimer
    | FromTimerDevice     IORspTimer
    | TimerTick
    -- Standard IO device traffic
    | ToStandardIO        IOReqSIO
    | FromStandardIO      IORspSIO
    -- File system traffic, including directory commands
    | ToFileSystemDD      IOReqFS
    | FromFileSystemDD    IORspFS
    -- Process control traffic
    | ToProcessControl    ProcessControlRequest
    | FromProcessControl  ProcessControlRequest
    -- System half traffic, in response to the kernel core
    | ToSystemHalf        KernelCoreCommand
    | FromSystemHalf      KernelCoreComply
    -- File system core traffic
    | ToFSCore            FSCoreReq
    | FromFSCore          FSCoreRsp
    -- Exceptions to the user half
    | ToUserException     ED.ExceptionData
    -- System half traffic to null device, for wait token
    | ToWait
    -- System half traffic, in response to the user half
    | ToUserProcess       SC.SystemResponse
    | FromTrapHandler     SC.SystemRequest

osPayCon :: DYN.TyCon
osPayCon = DYN.mkTyCon "Osker Payload"
instance DYN.Typeable OskerPay where
    DYN.typeOf _ = DYN.mkAppTy osPayCon []

-- Helpers for formatting output of internal messages
tostr :: String
tostr = "->"
fromstr :: String
fromstr = "<-"

instance Show OskerPay where
    show (ToKernelCore kcr)        = tostr   ++ "KC "  ++ show kcr
    show (FromKernelCore kcr)      = fromstr ++ "KC "  ++ show kcr
    show (ToUserProcess sc)        = tostr   ++ "UP "  ++ show sc
    show (FromTrapHandler sc)      = fromstr ++ "TH "  ++ show sc
    show (ToTimerDevice ioreq)     = tostr   ++ "TD "  ++ show ioreq
    show (FromTimerDevice iorsp)   = fromstr ++ "TD "  ++ show iorsp
    show TimerTick                 = fromstr ++ "TT"
    show (ToStandardIO ioreq)      = tostr   ++ "SIO " ++ show ioreq
    show (FromStandardIO iorsp)    = fromstr ++ "SIO " ++ show iorsp
    show (ToFileSystemDD ioreq)    = tostr   ++ "FS "  ++ show ioreq
    show (FromFileSystemDD iorsp)  = fromstr ++ "FS "  ++ show iorsp
    show (ToFSCore corereq)        = tostr   ++ "FSC " ++ show corereq
    show (FromFSCore corersp)      = fromstr ++ "FSC " ++ show corersp
    show (ToProcessControl pcr)    = tostr   ++ "PC "  ++ show pcr
    show (FromProcessControl pcr)  = fromstr ++ "PC "  ++ show pcr
    show (ToSystemHalf kcc)        = tostr   ++ "SH "  ++ show kcc
    show (FromSystemHalf kcc)      = fromstr ++ "SH "  ++ show kcc
    show (ToUserException _uhe)    = tostr   ++ "UHE"
    show (ToWait)                  = tostr   ++ "WAIT"

-- Get the system request from inputs originating from the user half.
getSysReq ::
    OskerPay -> -- From whence to get the sysreq
    SC.SystemRequest
getSysReq (FromTrapHandler sysreq) = sysreq
getSysReq sysreq = error ("Not a system request: " ++ show sysreq ++ ", ")

-- Get the kernel core request for inputs from the kernel core
getKernelCoreReq :: OskerPay -> KernelCoreRequest
getKernelCoreReq (ToKernelCore kcr) = kcr
getKernelCoreReq x = error ("Not a kernel core request: " ++ show x)

-- Get the kernel core response
getKernelCoreRsp :: OskerPay -> KernelCoreResponse
getKernelCoreRsp (FromKernelCore kcr) = kcr
getKernelCoreRsp x = error ("Not a kernel core response: " ++ show x)

-- Get the kernel core command from a payload
getKernelCoreCommand :: OskerPay -> KernelCoreCommand
getKernelCoreCommand pay =
  case pay of
    ToSystemHalf kcc -> kcc
    _otherwise -> error ("Not a kernel core command: " ++ show pay)

-- Get the kernel core comply for responses from the system half
getKernelCoreComply :: OskerPay -> KernelCoreComply
getKernelCoreComply (FromSystemHalf kcc) = kcc
getKernelCoreComply _ = error "Not a kernel core comply"

-- Fish the process control request out a payload
getToProcessControlReq :: OskerPay -> ProcessControlRequest
getToProcessControlReq (ToProcessControl pcr) = pcr
getToProcessControlReq x =
  error ("Not a to process control request: " ++ show x)

-- Fish the process control request out a message
getFromProcessControlReq :: OskerPay -> ProcessControlRequest
getFromProcessControlReq (FromProcessControl pcr) = pcr
getFromProcessControlReq x =
  error ("Not a from process control request: " ++ show x)

----------------------------------------------------------------------
-- The message payloads for the kernel core
----------------------------------------------------------------------

data ResourceSpecial
    = NothingSpecial

data KernelCoreRequest
    --------------------------------------------
    -- In support of resource allocation      --
    --------------------------------------------
    = ResourceRequest
      { rrProcessName :: PN.ProcessName     -- For debugging
      , rrResource    :: GR.GlobalResource  -- Type of resource to get
      , rrNumberToGet :: Int                -- # to request
      , rrSpecial     :: ResourceSpecial    -- Flags pertinent to request type
      }
    --------------------------------------------
    -- In support of message queues           --
    --------------------------------------------
    | CreateMQ
      { cmqProcessName :: PN.ProcessName
      , cmqLocalName   :: MQD.MQName
      , cmqFlags       :: OF.OpenFlags -- Flags, e.g. RW, R, W, ...
      , cmqAttr        :: MQD.MQAttr
      , cmqMode        :: M.Mode
      }
    | OpenMQ
      { omqProcessName :: PN.ProcessName -- Name of requesting process
      , omqMQName      :: MQD.MQName   -- The local name of the msg q
      , omqFlags       :: OF.OpenFlags -- Flags, e.g. RW, R, W, ...
      , omqAttr        :: MQD.MQAttr   -- Requested attributes
      , omqMode        :: M.Mode       -- Requested mode
      }
    | CloseMQ
      { clmqProcessName :: PN.ProcessName -- Name of requesting process
      , clmqMQName      :: MQD.MQName     -- The local name of the msg q
      }
    | UnlinkMQ
      { umqMQName      :: MQD.MQName      -- Global name of msg q
      , umqProcessName :: PN.ProcessName  -- Process name of unlinker
      }
    | SendMQ
      { smqProcessName :: PN.ProcessName -- Name of requesting process
      , smqMQName      :: MQD.MQName     -- Global name of the message q
      , smqMessage     :: MQD.Message    -- The message to send
      }
    | ReceiveMQ
      { rmqProcessName :: PN.ProcessName -- Name of the receiving process
      , rmqMQName      :: MQD.MQName     -- Global name of the message q
      }
    --------------------------------------------
    -- In support of process calls            --
    --------------------------------------------
    -- Fork a copy of a process
    | ForkProcess { fpTags :: PT.ProcessTags
                  , fpNode :: DOMG.ProcessNodeData
                  }
    -- Exit a process
    | ExitProcess { epName :: PN.ProcessName }
    -- Update IO Process info
    | UpdateProcess { updateProcess :: () }
    -- Request to send a signal to a process
    | SignalProcess { spPid :: PID.ProcessId
                    , spSig :: SC.Signal
                    }
    -- Delete process from tables
    | DelProcess PN.ProcessName
    -- Add a new process group
    | AddProcessGroup { apgPid  :: PID.ProcessId
                      , apgPgid :: PID.ProcessId
                      , apgTags :: PT.ProcessTags
                      }
    | AddSession { asTags  :: PT.ProcessTags }

instance Show KernelCoreRequest where
    show (ResourceRequest pn resource n _special)    =
      "ResourceRequest: " ++ PN.outProcessName pn ++ ", " ++
      ", " ++ show resource ++ ", " ++ show n
    show (CreateMQ pname mqname flags attr mode) =
      "CreateMQ: " ++ PN.outProcessName pname ++ ", " ++ show attr ++
      ", " ++ show mqname ++ ", " ++ show flags ++ ", " ++ show mode
    show (OpenMQ pname mqname flags attr mode) =
      "OpenMQ: " ++ PN.outProcessName pname ++ ", " ++ show mqname ++
      ", " ++ show flags ++ ", " ++ show attr ++ ", " ++ show mode
    show (CloseMQ pname mqname) =
      "CloseMQ: " ++ PN.outProcessName pname ++ ", " ++ show mqname
    show (UnlinkMQ mqname pn) =
      "UnlinkMQ: " ++ show mqname ++ ", " ++ PN.outProcessName pn
    show (SendMQ pname mqname msg) =
      "SendMQ: " ++ PN.outProcessName pname ++ ", " ++ show mqname ++
      ", " ++ show msg
    show (ReceiveMQ pname mqname) =
      "ReceiveMQ: " ++ PN.outProcessName pname ++ ", " ++ show mqname
    show (ForkProcess tags node) =
      "ForkProcess: " ++ show tags ++ ", " ++ show node
    show (ExitProcess pname)  = "ExitProcess: " ++ PN.outProcessName pname
    show (UpdateProcess proc) = "UpdateProcess: " ++ show proc
    show (SignalProcess pid signal) =
      "SignalProcess: " ++ show pid ++ ", " ++ show signal
    show (DelProcess pname)   = "DelProcess: " ++ PN.outProcessName pname
    show (AddProcessGroup pid pgid tags)   =
      "AddProcessGroup: " ++ show pid ++ ", " ++ show pgid ++
      ", " ++ show tags
    show (AddSession tags) = "AddSession: " ++ show tags

data KernelCoreRequestCode
    = ResourceRequestCode
    | CreateMQCode
    | OpenMQCode
    | CloseMQCode
    | UnlinkMQCode
    | SendMQCode
    | ReceiveMQCode
    | ForkProcessCode
    | ExitProcessCode
    | UpdateProcessCode
    | SignalProcessCode
    | DelProcessCode
    | AddProcessGroupCode
    | AddSessionCode
    deriving (Eq, Ord, Enum, Show)

-- For equality comparison, only care about the codes
instance Eq KernelCoreRequest where
    sr1 == sr2 = kernelCoreRequest2Code sr1 ==
                 kernelCoreRequest2Code sr2

-- Ghc blows up unless I define my own Ord instance.
instance Ord KernelCoreRequest where
    compare sr1 sr2 =
      compare (kernelCoreRequest2Code sr1) (kernelCoreRequest2Code sr2)

kernelCoreRequest2Code :: KernelCoreRequest -> KernelCoreRequestCode
kernelCoreRequest2Code (ResourceRequest _ _ _ _) = ResourceRequestCode
kernelCoreRequest2Code (CreateMQ _ _ _ _ _)    = CreateMQCode
kernelCoreRequest2Code (OpenMQ _ _ _ _ _)      = OpenMQCode
kernelCoreRequest2Code (CloseMQ _ _ )          = CloseMQCode
kernelCoreRequest2Code (UnlinkMQ _ _)          = UnlinkMQCode
kernelCoreRequest2Code (SendMQ _ _ _)          = SendMQCode
kernelCoreRequest2Code (ReceiveMQ _ _)         = ReceiveMQCode
kernelCoreRequest2Code (ForkProcess _ _)       = ForkProcessCode
kernelCoreRequest2Code (ExitProcess _)         = ExitProcessCode
kernelCoreRequest2Code (UpdateProcess _)       = UpdateProcessCode
kernelCoreRequest2Code (SignalProcess _ _)     = SignalProcessCode
kernelCoreRequest2Code (DelProcess _)          = DelProcessCode
kernelCoreRequest2Code (AddProcessGroup _ _ _) = AddProcessGroupCode
kernelCoreRequest2Code (AddSession _)          = AddSessionCode

kernelCoreRequest2Int :: KernelCoreRequest -> Int
kernelCoreRequest2Int = fromEnum . kernelCoreRequest2Code

kcReqCon :: DYN.TyCon
kcReqCon = DYN.mkTyCon "Kernel Core Request"
instance DYN.Typeable KernelCoreRequest where
    DYN.typeOf _ = DYN.mkAppTy kcReqCon []

----------------------------------------------------------------------
-- The new processing items for the kernel core
----------------------------------------------------------------------
data DataBlock -- Data block that can be resources
    = TCBResponse TCB.TimerControlBlock
    deriving (Show)

-- Make an open MQ Response with just an error message
nullOpenMQRsp :: ER.Errno -> KernelCoreResponse
nullOpenMQRsp er = OpenMQResponse { omqrErr   = Just er
                                  , omqrName  = Nothing
                                  , omqrAttr  = MQD.initMQAttrs
                                  , omqrFlags = OF.nullFlags
                                  }

data KernelCoreResponse
    -- Resource response, Nothing is failure
    = ResourceResponse (Maybe DataBlock)
    -- Create MQ Response, Nothing is failure
    | CreateMQResponse { cmqrName  :: Maybe MQD.MQName
                       , cmqrAttr  :: MQD.MQAttr
                       , cmqrFlags :: OF.OpenFlags -- Flags, e.g. RW, R, W
                       }
    -- Open MQ Response, Nothing is failure
    | OpenMQResponse { omqrErr  :: Maybe ER.Errno
                     , omqrName :: Maybe MQD.MQName
                     , omqrAttr  :: MQD.MQAttr
                     , omqrFlags :: OF.OpenFlags -- Flags, e.g. RW, R, W
                     }
    -- Close MQ Response
    | CloseMQResponse Bool
    -- Unlink response
    | UnlinkMQResponse Bool
    -- Send response
    | SendMQResponse Bool
    -- Receive response
    | ReceiveMQResponse
      { rmqrErr :: (Maybe ER.Errno)
      , rmqrMsg :: (Maybe MQD.Message)
      }
    -- Fork response, Nothing is fork fails, PID if fork succeeds
    | ForkResponse (Maybe PID.ProcessId)
    -- Signal response, success / failure indication
    | SignalProcessResponse Bool
    -- Add process group response: returning success / failure code
    | AddProcessGroupResponse ER.Errno
    -- Add a session, returning error code
    | AddSessionResponse { asrEr :: ER.Errno }
    deriving (Show)

data KernelCoreResponseCode
    = ResourceResponseCode
    | CreateMQResponseCode
    | OpenMQResponseCode
    | CloseMQResponseCode
    | UnlinkMQResponseCode
    | SendMQResponseCode
    | ReceiveMQResponseCode
    | ForkResponseCode
    | SignalProcessResponseCode
    | AddProcessGroupResponseCode
    | AddSessionResponseCode
    deriving (Eq, Ord, Show, Enum)

-- For equality comparison, only care about the codes
instance Eq KernelCoreResponse where
    sr1 == sr2 = kernelCoreResponse2Code sr1 ==
                 kernelCoreResponse2Code sr2

-- Ghc blows up unless I define my own Ord instance.
instance Ord KernelCoreResponse where
    compare sr1 sr2 =
      compare (kernelCoreResponse2Code sr1) (kernelCoreResponse2Code sr2)

kernelCoreResponse2Code :: KernelCoreResponse -> KernelCoreResponseCode
kernelCoreResponse2Code (ResourceResponse _)      = ResourceResponseCode
kernelCoreResponse2Code (CreateMQResponse _ _ _)  = CreateMQResponseCode
kernelCoreResponse2Code (OpenMQResponse _ _ _ _)  = OpenMQResponseCode
kernelCoreResponse2Code (CloseMQResponse _)       = CloseMQResponseCode
kernelCoreResponse2Code (UnlinkMQResponse _)      = UnlinkMQResponseCode
kernelCoreResponse2Code (SendMQResponse _)        = SendMQResponseCode
kernelCoreResponse2Code (ReceiveMQResponse _ _)   = ReceiveMQResponseCode
kernelCoreResponse2Code (ForkResponse _)          = ForkResponseCode
kernelCoreResponse2Code (SignalProcessResponse _) =
  SignalProcessResponseCode
kernelCoreResponse2Code (AddProcessGroupResponse _) =
  AddProcessGroupResponseCode
kernelCoreResponse2Code (AddSessionResponse _) = AddSessionResponseCode

kcRspCon :: DYN.TyCon
kcRspCon = DYN.mkTyCon "Kernel Core Response"
instance DYN.Typeable KernelCoreResponse where
    DYN.typeOf _ = DYN.mkAppTy kcRspCon []

----------------------------------------------------------------------
-- Kernel core commands to the system half, and their responses
-- called "complies"
----------------------------------------------------------------------
data KernelCoreCommand
    -- Request some TCBs
    = ReceiveSignal PID.ProcessId SC.Signal
    -- The last one, as a place holder.
    | LastKernelCoreCommand

instance Show KernelCoreCommand where
    show = show . kernelCoreCommand2Code

data KernelCoreCommandCode
    = ReceiveSignalCode
    | LastKernelCoreCommandCode
    deriving (Eq, Ord, Enum, Show)

-- For equality comparison, only care about the codes
instance Eq KernelCoreCommand where
    sr1 == sr2 = kernelCoreCommand2Code sr1 ==
                 kernelCoreCommand2Code sr2

-- Again for Ord, care only about the corresponding codes
instance Ord KernelCoreCommand where
    compare sr1 sr2 =
      compare (kernelCoreCommand2Code sr1) (kernelCoreCommand2Code sr2)

kernelCoreCommand2Code :: KernelCoreCommand -> KernelCoreCommandCode
kernelCoreCommand2Code (ReceiveSignal _ _)     = ReceiveSignalCode
kernelCoreCommand2Code (LastKernelCoreCommand) = LastKernelCoreCommandCode

kernelCoreCommand2Int :: KernelCoreCommand -> Int
kernelCoreCommand2Int = fromEnum . kernelCoreCommand2Code

kcComCon :: DYN.TyCon
kcComCon = DYN.mkTyCon "Kernel Core Command"
instance DYN.Typeable KernelCoreCommand where
    DYN.typeOf _ = DYN.mkAppTy kcComCon []

data KernelCoreComply
    -- Comply with the receive signal
    = ReceiveSignalComply Bool
    deriving (Show)

data KernelCoreComplyCode
    = ReceiveSignalComplyCode
    deriving (Eq, Ord, Show, Enum)

-- For equality comparison, only care about the codes
instance Eq KernelCoreComply where
    sr1 == sr2 = kernelCoreComply2Code sr1 ==
                 kernelCoreComply2Code sr2

-- Again, for Ord care only about the corresponding codes
instance Ord KernelCoreComply where
    compare sr1 sr2 =
      compare (kernelCoreComply2Code sr1) (kernelCoreComply2Code sr2)

kernelCoreComply2Code :: KernelCoreComply -> KernelCoreComplyCode
kernelCoreComply2Code (ReceiveSignalComply _)  = ReceiveSignalComplyCode

kernelCoreComply2Int :: KernelCoreComply -> Int
kernelCoreComply2Int = fromEnum . kernelCoreComply2Code

kcComplyCon :: DYN.TyCon
kcComplyCon = DYN.mkTyCon "Kernel Core Comply"
instance DYN.Typeable KernelCoreComply where
    DYN.typeOf _ = DYN.mkAppTy kcComplyCon []

----------------------------------------------------------------------
-- Process control messages, from the actor of the kernel
-- core to process control, or from process control back to the
-- actor of the kernel core.
----------------------------------------------------------------------

data ProcessControlRequest
    = AddProcessPCR { -- Information about the process to add
                      apProcessTags       :: PT.ProcessTags
                    , apProcessNode       :: DOMG.ProcessNodeData
                      -- Information about the forking process
                    , forkingProcess      :: PT.ProcessTags
                    }
    | UpdateProcessPCR { forkedProcess    :: ()
                       , forkingProcess   :: PT.ProcessTags
                       }
    | DeleteProcessPCR { dpProcessName    :: PN.ProcessName
                       , dpSystemHalfTid  :: PN.ProcessName
                       }
    | StartProcessPCR { spForked          :: ()
                      , spForking         :: PT.ProcessTags
                      }
      -- To continue after some deferred work
    | FinishProcessPCR { fpForked         :: PT.ProcessTags
                       , fpForking        :: PT.ProcessTags
                       }

instance Show ProcessControlRequest where
    show (AddProcessPCR tags node forking) =
      "AddProcessPCR " ++ show tags  ++
      ", " ++ show node ++ "," ++ show forking
    show (DeleteProcessPCR name _) = "DeleteProcessPCR " ++ show name
    show (UpdateProcessPCR pd forking)     =
      "UpdateProcessPCR " ++ show pd ++ ", " ++ show forking
    show (StartProcessPCR proc forking) =
      "StartProcessPCR " ++ show proc ++ ", " ++ show forking
    show (FinishProcessPCR forked forking) =
      "FinishProcessPCR " ++ show forked ++ ", " ++ show forking

data ProcessControlRequestCode
    = AddProcessCodePCR
    | UpdateProcessCodePCR
    | DeleteProcessCodePCR
    | StartProcessCodePCR
    | FinishProcessCodePCR
    deriving (Eq, Ord, Show, Enum)

processControlRequest2Code ::
    ProcessControlRequest -> ProcessControlRequestCode
processControlRequest2Code (AddProcessPCR _ _ _)  = AddProcessCodePCR
processControlRequest2Code (UpdateProcessPCR _ _) = UpdateProcessCodePCR
processControlRequest2Code (DeleteProcessPCR _ _) = DeleteProcessCodePCR
processControlRequest2Code (StartProcessPCR _ _)  = StartProcessCodePCR
processControlRequest2Code (FinishProcessPCR _ _) = FinishProcessCodePCR

processControlRequest2Int :: ProcessControlRequest -> Int
processControlRequest2Int = fromEnum . processControlRequest2Code

----------------------------------------------------------------------
-- The timer system resource blocks
----------------------------------------------------------------------

-- Request Messages relating to timer resource allocation
data GRReqTimer =
    GRReqTCB Int -- Request tcbs
    deriving (Show)

kGRReqTimerCon :: DYN.TyCon
kGRReqTimerCon = DYN.mkTyCon "Timer Resource Requests"
instance DYN.Typeable GRReqTimer where
    DYN.typeOf _ = DYN.mkAppTy kGRReqTimerCon []

-- Response Messages relating to timer resource allocation
data GRRspTimer
    = GRNoTimers
    | GRRspTimer TCB.TimerControlBlock -- Response to get timer
    deriving (Show)

kGRRspTimerCon :: DYN.TyCon
kGRRspTimerCon = DYN.mkTyCon "Timer Resource Responses"
instance DYN.Typeable GRRspTimer where
    DYN.typeOf _ = DYN.mkAppTy kGRRspTimerCon []

-- Request Messages relating to the file system "Pid" thread
data IOReqFS = IOReqFS { reqFS :: SC.SystemRequest } deriving (Show)
data IORspFS = IORspFS { rspFS :: SC.SystemResponse } deriving (Show)

-- Request Messages relating to the file system core thread
data FSCoreReq
    = OpenDirReq { odPName :: PN.ProcessName
                 , odDir   :: FN.FileName
                 }
    | ReadDirReq { rdPName :: PN.ProcessName
                 , rdDir   :: DD.DIR
                 }

instance Show FSCoreReq where
    show fsreq =
      case fsreq of
        OpenDirReq { odPName = pn, odDir = fn } ->
          "OpenDirReq: " ++ PN.outProcessName pn ++
          ", " ++ FN.outFileName fn ++ " "
        ReadDirReq { rdPName = pn, rdDir = dir } ->
          "ReadDirReq: " ++ PN.outProcessName pn ++
          ", " ++ show dir

data FSCoreRsp
    = OpenDirRsp { odRspDir :: DD.DIR
                 , odRsp    :: Bool
                 }
    | ReadDirRsp { rdRspDir :: DD.DIR
                 , rdName   :: String
                 , rdIsDir  :: Bool
                 }
    deriving (Show)

-- Request Messages relating to the standard IO device
data IOReqSIO
    = ReadSIO  { rsioProcessName :: PN.ProcessName }
    | WriteSIO { wsioProcessName :: PN.ProcessName
               , wsioData        :: SC.SystemResponse
               }

instance Show IOReqSIO where
    show rsio =
      case rsio of
        ReadSIO { rsioProcessName = pn } ->
          "ReadSIO: " ++ PN.outProcessName pn
        WriteSIO { wsioProcessName = pn
                 , wsioData        = rsp
                 } ->
          "WriteSIO: " ++ PN.outProcessName pn ++ "," ++ show rsp

-- Response messages relating to the standard IO device
data IORspSIO
    = ReadSIORsp  { rsiorSysReq :: SC.OskerCommand }
    | WriteSIORsp { wsioRsp :: Bool }
    deriving (Show)

-- Request Messages relating to the timer device
-- The request includes the response channel for the driver
data IOReqTimer
    = StartTimerRequest  { strTCB         :: TCB.TimerControlBlock }
    | CancelTimerRequest { ctrTCB         :: TCB.TimerControlBlock
                         , ctrProcessName :: PN.ProcessName
                         }

instance Show IOReqTimer where
    show (StartTimerRequest { strTCB = str }) =
         "StartTimerRequest: " ++ show str
    show (CancelTimerRequest { ctrTCB = ctr, ctrProcessName = pn } ) =
      "CancelTimerRequest: " ++ show ctr ++ ", " ++ PN.outProcessName pn

kIOReqTimerCon :: DYN.TyCon
kIOReqTimerCon = DYN.mkTyCon "Timer IO Requests"
instance DYN.Typeable IOReqTimer where
    DYN.typeOf _ = DYN.mkAppTy kIOReqTimerCon []

-- Response Messages relating to the timer device
data IORspTimer
    = StartTimerResponse (Maybe Int)  -- Response to start timer
    | CancelTimerResponse (Maybe Int) -- Response to cancel timer
    deriving (Show)

kIORspTimerCon :: DYN.TyCon
kIORspTimerCon = DYN.mkTyCon "Timer IO Responses"
instance DYN.Typeable IORspTimer where
    DYN.typeOf _ = DYN.mkAppTy kIORspTimerCon []

----------------------------------------------------------------------
-- This specialisation of the response message permits a responder
-- to access the response channel field
----------------------------------------------------------------------
type OskerChannel = M.Channel OskerMsg
