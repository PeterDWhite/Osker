-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SystemCall
    ( SystemRequest (..)           -- Type for system requests to Osker
    , SystemResponse (..)          -- System response, including Posix Errno
    , SpecificSystemResponse (..)  -- System response to specific call
    , ER.Errno                     -- Part of system response
    , noError                      -- Indicator for no error
    , nullSystemResponse           -- When system response not needed
    , SystemRequestCode (..)       -- Codes for system requests
    , outSystemReqCode             -- Nice print out of system request code
    , systemRequest2String         -- Convert request to string
    , systemResponse2String        -- Convert response to string
    , systemRequest2Code           -- Convert request to request code
    , systemRequest2Int            -- Convert request to integer
    , allSystemRequests            -- Generate list of all system requests
    , ErrorInd                     -- Posix error indicator type
    , passFlag                     -- The call passed
    , failFlag                     -- The call failed
      -- For demo only
    , OskerCommand (..)            -- Commands to the demo program
      -- Signal data structures
    , Signal (..)           -- Posix signals
    , SignalMask            -- Set of masked signals
    , SigAction (..)        -- Implementing action to take when signal arrives
    , SignalSet             -- A set of signals
    , SignalMap (..)        -- Map from signal to signal action
    , outSignalMap          -- Print out a signal map
    , SignalAction (..)     -- Action to take when a signal arrives
    , hasHandler            -- Determine if a signal action has a handler
    , defaultSignalActions  -- Defualt action to take
    -- Library signal interfaces
    , sigemptyset           -- An empty signal set
    , sigfillset            -- A set with all signals in it
    , sigaddset             -- Add an element to a signal set
    , sigdelset             -- Delete an element from a signal set
    , sigismember           -- Determine if a signal is in a set
    , siglisttoset          -- Convert list of signals to signal set
    , sigunion              -- Union of two signal sets
    , sigintersect          -- Intersection of two signal sets
    , sigcomplement         -- Complement of a signal set wrt sigfillset
    -- Osker signal interfaces
    , SigProcMaskParam (..) -- How to change the signal mask
    , SignalSetParam (..)   -- The set of signals to update
    , getSigAction          -- Lookup in the signal map
    , updateSignalMap       -- Update the signal map
      --------------------------------------------------
      -- User monad stuff                             --
      --------------------------------------------------
    , U                     -- User monad
    , UserProgram           -- Monad action returning ()
    , UserException (..)    -- Exceptions to the user program
    , projSig               -- Project out the signal
    , ExceptionHandler      -- Handler for user program
      --------------------------------------------------
      -- Entries used by the controller (system half) --
      --------------------------------------------------
    , newSlice        -- Grant another time slice
    , UserTrap (..)   -- Result of running the program
    , UserState (..)  -- Internal state of user program
    , isComplete      -- Determine if user program has completed
      -----------------------------------
      -- Entries used by the user half --
      -----------------------------------
      -- System call methods
    , osker           -- Make system call and get response
      -- Debugging calls
    , putStr          -- Debugging print out
    , putStrLn        -- Print with preceding "\n"
    , startUserHalf   -- Start a user half program
    , fail            -- Fail a user program
    ) where

----------------------------------------------------------------------
-- The system calls and the corresponding responses are defined here.
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding ( fail, putStr, putStrLn )
import List
import Dynamic ( TyCon, Typeable, typeOf, mkTyCon, mkAppTy )
import qualified Exception as E
import qualified Ix as IX
import qualified FiniteMap as FM
-- Utility imports
-- import Unsafe
-- Resumption imports
import qualified RSEIO as R
-- Posix imports
import qualified OpenFlags as OF
import qualified ProcessId as PID
import qualified MessageQueueData as MQD
import qualified Mode as M
import qualified Size as SZ
import qualified File as F
import qualified FileName as FN
import qualified Errno as ER
import qualified RealTimeSignal as RTS
import qualified Whence as W
import qualified BufferMode as BM
import qualified Uid as U
import qualified Gid as G
import qualified DirectoryData as D

-- Commands from the demo program
data OskerCommand
    = OskerSysReq SystemRequest
    | OskerScript String
    deriving (Show)

data SystemRequest
    = NullSystemRequest -- For error cases
    ----------------------------------------------
    ---- PROCESS CONTROL INTERFACES (POSIX.1) ----
    ----------------------------------------------
    -- fork: Fork a copy of a process
    -- Until we have continuation passing style in the user half,
    -- we will pass in the user half code, thus fork is combined
    -- with exec right now
    | ForkReq
    -- exit: Exit a user process
    | ExitReq Int
    -- getPid: Return the PID of the running process
    | GetPidReq
    -- getPpid: Get parent process ID
    | GetPPidReq
    -- getPgrp: Get process group id
    | GetPGidReq PID.ProcessId
    -- setpgid: Set the process group id
    | SetPGidReq PID.ProcessId PID.ProcessId
    -- setsid: Change the session id
    | SetSidReq
    -------------------------------------
    ---- SIGNAL INTERFACES (POSIX.1) ----
    -------------------------------------
    -- alarm: Block for t seconds
    | AlarmReq Int
    -- pause:  Wait for a signal
    | PauseReq
    -- sigaction: Change signal behaviour
    | SigActionReq Signal SigAction
    -- sigprocmask: Change the signal mask
    | SigProcMaskReq SigProcMaskParam SignalSetParam
    -- sigpending: Find out what signals are blocked
    | SigPendingReq
    -- kill: Send a signal to a process
    | KillReq PID.ProcessId Signal
    -- sigsuspend halts until signal with a new mask
    | SigSuspendReq SignalSet
    -- sleep: Like a pause, but will end after specified time
    | SleepReq { seconds :: Int }
    --------------------------------------------
    ---- MESSAGE QUEUE INTERFACES (POSIX.4) ----
    --------------------------------------------
    -- mq_open: Open or create a message queue
    | MqOpenReq { mqorName  :: MQD.MQName
                , mqorFlags :: OF.OpenFlags
                , mqorMode  :: M.Mode
                , mqorAttr  :: MQD.MQAttr
                }
    -- mq_close: Release access to a message queue
    | MqCloseReq { mqcrDesc :: MQD.MQDesc }
    -- mq_send: Send a message on a message queue
    | MqSendReq { mqsrDesc     :: MQD.MQDesc
                , mqsrMessage  :: MQD.Message
                , mqsrSize     :: SZ.Size
                , mqsrPriority :: Int
                }
    -- mq_receive: Recevie a message on a message queue
    | MqReceiveReq { mqrrDesc :: MQD.MQDesc }
    -- mq_notify: Receive aynchronous notification of message in a queue
    | MqNotifyReq  { mnrDesc  :: MQD.MQDesc }
    -- mq_gatattr: Get POSIX.4 message queue attributes
    | MqGetAttrReq MQD.MQDesc
    -- mq_setattr: Set POSIX.4 attributes of a message queue
    | MqSetAttrReq MQD.MQDesc MQD.MQAttr
    -- mq_unlink: Destroy a message queue
    | MqUnlinkReq  { mqurLocalName :: MQD.MQName }
    --------------------------------------
    ---- FILE IO INTERFACES (POSIX.1) ----
    --------------------------------------
    -- Put specified string to file
    | FPutsReq { fputsString :: String
               , fputcStream :: F.File
               }
    -- Put character to file
    | FPutcReq { fputcChar   :: Char
               , fputcStream :: F.File
               }
    -- Get a string up to specified length from file
    | FGetsReq { fgetsLength :: Int
               , fgetsStream :: F.File
               }
    -- Get a character from a file
    | FGetcReq { fgetcStream :: F.File }
    -- Get the current file position
    | FTellReq { ftrStream :: F.File }
    -- Set the current file position
    | FSeekReq { fsrStream :: F.File
               , fsrOffset :: Int
               , fsrBase   :: W.Whence
               }
    -- Open a file for read, write, or append
    | FOpenReq { forPath :: String
               , forMode :: M.Mode
               }
    -- Close an open file
    | FCloseReq { fcrStream :: F.File }
    -- Low level read from a stream
    | FReadReq  { frrSize   :: Int
                , frrNumber :: Int
                , frrStream :: F.File
                }
    -- Low level write to a file
    | FWriteReq { fwrString :: String  -- For now, write a string
                , fwrSize   :: Int     -- Block size
                , fwrNumber :: Int     -- Number of blocks
                , fwrStream :: F.File  -- File to write
                }
    -- Set file position to beginning
    | RewindReq { rewindStream :: F.File }
    -- Flush buffers to output
    | FFlushReq { ffrStream :: F.File }
    -- Set the buffering characteristics of a file
    | SetVBufReq { setvStream :: F.File
                 , setvBuf    :: ()     -- We do not allow user spec buf YET
                 , setvMode   :: BM.BufferMode
                 , setvSize   :: Int
                 }
    ------------------------------------------
    ---- FILE SYSTEM INTERFACES (POSIX.1) ----
    ------------------------------------------
    | MkdirReq { mkdPath :: String
               , mkdMode :: M.Mode
               }
    | RmdirReq { rmdPath :: String }
    | LinkReq  { existingPath :: String
               , newPath      :: String
               }
    | UnlinkReq { unlinkPath :: String }
    | ChmodReq  { chmodPath  :: String
                , chmodMode  :: M.Mode
                }
    | ChownReq  { chownPath  :: String
                , chownUid   :: U.Uid
                , chownGid   :: G.Gid
                }
    ----------------------------------------
    ---- DIRECTORY INTERFACES (POSIX.1) ----
    ----------------------------------------
    | CloseDirReq  { closeDir  :: D.DIR }
    | OpenDirReq   { openPath  :: String }
    | ReadDirReq   { readDir   :: D.DIR }
    | RewindDirReq { rewindDir :: D.DIR }
    ------------------------------------------
    ---- STANDARD IO INTERFACES (POSIX.1) ----
    ------------------------------------------
    -- Get a string from standard input
    | GetsReq
    -- Get a character from standard input
    | GetCharReq
    -- Put a string on standard output
    | PutsReq    { putsRsp :: SystemResponse }
    -- Put a character to standard output
    | PutCharReq { putcChar :: Char }
    --------------------------
    ---- Osker interfaces   --
    --------------------------
    -- A time slice is done
    | SliceReq
    -- logaudit: Log an audit event
    | LogAuditReq
    -- Useful for range operations on sys req
    | LastSystemRequest

-- For equality comparison, only care about the request codes
-- corresponding to the request.
instance Eq SystemRequest where
    sr1 == sr2 = systemRequest2Code sr1 == systemRequest2Code sr2

-- Ghc blows up unless I define my own Ord instance. The system
-- request is used to index the activity table, and this is where
-- the trouble occurs.
instance Ord SystemRequest where
    compare sr1 sr2 =
      compare (systemRequest2Code sr1) (systemRequest2Code sr2)

data SystemRequestCode
                               = NullSystemReqCode     -- For error cases
                               ----------------------------------------------
                               ---- PROCESS CONTROL INTERFACES (POSIX.1) ----
                               ----------------------------------------------
    | ForkReqCode           -- For a copy of a process
    | ExitReqCode           -- Exit a user process
    | GetPidReqCode         -- Return PID of running process
    | GetPPidReqCode        -- Return PID of parent process
    | GetPGidReqCode        -- Return process group id
    | SetPGidReqCode        -- Set process group id
    | SetSidReqCode         -- Set session id
    -------------------------------------
    ---- SIGNAL INTERFACES (POSIX.1) ----
    -------------------------------------
    | AlarmReqCode          -- Block for t seconds
    | SigActionReqCode      -- Change signal behaviour
    | SigProcMaskReqCode    -- Change signal mask
    | SigPendingReqCode     -- Find out what signals are pending
    | KillReqCode           -- Send a signal
    | PauseReqCode          -- Wait for a signal
    | SigSuspendReqCode     -- Wait for a signal with a new mask
    | SleepReqCode          -- Pause with timeout
    --------------------------------------------
    ---- MESSAGE QUEUE INTERFACES (POSIX.4) ----
    --------------------------------------------
    | MqOpenReqCode         -- Create / open a message queue
    | MqCloseReqCode        -- Close a message queue
    | MqSendReqCode         -- Send a message on a queue
    | MqReceiveReqCode      -- Receive a message from a queue
    | MqNotifyReqCode       -- Receive asynch notification of msg
    | MqGetAttrReqCode      -- Get message queue attributes
    | MqSetAttrReqCode      -- Set message queue attributes
    | MqUnlinkReqCode       -- Destroy a message queue
    --------------------------------------
    ---- FILE IO INTERFACES (POSIX.1) ----
    --------------------------------------
    | FPutsReqCode
    | FPutcReqCode
    | FGetsReqCode
    | FGetcReqCode
    | FTellReqCode
    | FSeekReqCode
    | FOpenReqCode
    | FCloseReqCode
    | FReadReqCode
    | FWriteReqCode
    | RewindReqCode
    | FFlushReqCode
    | SetVBufReqCode
    ------------------------------------------
    ---- FILE SYSTEM INTERFACES (POSIX.1) ----
    ------------------------------------------
    | MkdirReqCode
    | RmdirReqCode
    | LinkReqCode
    | UnlinkReqCode
    | ChmodReqCode
    | ChownReqCode
    ----------------------------------------
    ---- DIRECTORY INTERFACES (POSIX.1) ----
    ----------------------------------------
    | CloseDirReqCode
    | OpenDirReqCode
    | ReadDirReqCode
    | RewindDirReqCode
    ------------------------------------------
    ---- STANDARD IO INTERFACES (POSIX.1) ----
    ------------------------------------------
    | GetsReqCode
    | GetCharReqCode
    | PutsReqCode
    | PutCharReqCode
    --------------------------
    ---- OSKER INTERFACES ----
    --------------------------
    | SliceReqCode            -- A time slice is done
    | LogAuditReqCode         -- Log an audit event
    | LastSystemReqCode       -- Useful for range operations on sys req
    deriving (Eq, Ord, Enum, IX.Ix, Show)

-- Generate a list of all system requests
allSystemRequests :: [SystemRequestCode]
allSystemRequests = IX.range (NullSystemReqCode, LastSystemReqCode)

-- A function to print out a system request code without the "ReqCode"
outSystemReqCode :: SystemRequestCode -> String
outSystemReqCode s = take (length (show s) - length "ReqCode") (show s)

----------------------------------------------------------------------
-- The system response has a specific portion, specific to the
-- system request. There is also a portion common to all system
-- responses, which is the error number. For the Osker Haskell
-- interface, the errno is part of the system response. When we
-- implement the C interface, we will set an errno in the memory
-- space of the C user process.
----------------------------------------------------------------------
data SystemResponse =
    SystemResponse { srErrno    :: Maybe ER.Errno
                   , srSpecific :: SpecificSystemResponse
                   } deriving (Show)

-- Used for returning no error condigion
noError :: Maybe ER.Errno
noError = Nothing

-- A null system response
nullSystemResponse :: SystemResponse
nullSystemResponse = SystemResponse { srErrno    = noError
                                    , srSpecific = NullResponse
                                    }

-- When -1 means error...
type ErrorInd = Int
failFlag :: ErrorInd
failFlag = -1
passFlag :: ErrorInd
passFlag = 0

data SpecificSystemResponse
    = NullResponse                     -- When no response needed
    -----------------------------
    ---- REJECTION RESPONSES ----
    -----------------------------
    | BogusSystemRequest SystemRequest -- Kernel cannot figure out request
    | BogusException E.Exception       -- When exception cannot be fromDyn'ed
    | GlobalResourceFailure            -- When global resources not available
    | GlobalDeltaFailure               -- When global IO fails
    | Reject ER.Errno                  -- Call rejected
    ----------------------------------------------
    ---- PROCESS CONTROL INTERFACES (POSIX.1) ----
    ----------------------------------------------
    | ForkRsp Int                      -- Returns child / parent flag
    | ExitRsp                          -- For exit system call
    | GetPidRsp PID.ProcessId          -- Get pid of running process
    | GetPPidRsp PID.ProcessId         -- Get pid of parent
    | GetPGidRsp PID.ProcessId         -- Get pid of process group
    | SetPGidRsp ErrorInd              -- Returns error indication
    | SetSidRsp PID.ProcessId          -- Returns process group id.
    -------------------------------------
    ---- SIGNAL INTERFACES (POSIX.1) ----
    -------------------------------------
    | AlarmRsp (Maybe Int)             -- # sec left, when success
    | PauseRsp                         -- Response to pause system call
    | SigActionRsp
      { sarAct :: SignalAction }       -- Response to sigaction
    | SigProcMaskRsp SignalSet         -- Response to sigprocmask
    | SigPendingRsp
      { sprPending :: SignalSet }      -- What signals are pending
    | KillRsp ErrorInd                 -- Returns error indication
    | SigSuspendRsp ErrorInd           -- Returns error indication
    | SleepRsp ErrorInd                -- Returns error indication
    -------------------------------------------
    ---- MESSAGE QUEUE RESPONSES (POSIX.4) ----
    -------------------------------------------
    | MqOpenRsp { mqorspDesc :: MQD.MQDesc
                , mqorspAttr :: MQD.MQAttr
                }  -- Open / create message queue
    | MqCloseRsp
      { mqcrspFlag :: ErrorInd }       -- Returns error indication
    | MqSendRsp
      { mqsrspFlag ::  ErrorInd }      -- Returns error indication
    | MqReceiveRsp
      { mqrrspMessage :: Maybe MQD.Message -- Possible message
      , mqrrsp        :: ErrorInd          -- Error indication
      }
    | MqNotifyRsp RTS.SigEvent         -- Notification structure
    | MqGetAttrRsp MQD.MQAttr ErrorInd -- Returns attributes, error ind
    | MqSetAttrRsp MQD.MQAttr ErrorInd -- Returns old attrs, error ind
    | MqUnlinkRsp
      { mqurspFlag :: ErrorInd }       -- Returns error indication
    --------------------------------------
    ---- FILE IO INTERFACES (POSIX.1) ----
    --------------------------------------
    | FPutsRsp ErrorInd                -- Returns error indication
    | FPutcRsp (Maybe Char)            -- Returns char written or EOF (Nothing)
    | FGetsRsp (Maybe String)          -- Nothing if EOF, otherwise string
    | FGetcRsp (Maybe Char)            -- Nothing if EOF, otherwise char
    | FTellRsp Int                     -- File position, or -1 for error
    | FSeekRsp ErrorInd                -- Returns error indication
    | FOpenRsp
      { forStream  :: Maybe F.File }   -- File (when opened)
    | FCloseRsp 
      { fcrInd     :: ErrorInd }       -- Error indication
    | FReadRsp
      { frrmString :: Maybe String }   -- String returned, Nothing if EOF
    | FWriteRsp
      { fwrLength  :: Int }            -- Number of elements written
    | RewindRsp                        -- No response value
    | FFlushRsp
      { ffrInd     :: ErrorInd }       -- Error indication
    | SetVBufRsp
      { setvInd    :: ErrorInd }       -- Error indication
    ------------------------------------------
    ---- FILE SYSTEM INTERFACES (POSIX.1) ----
    ------------------------------------------
    | MkdirRsp
      { mkdInd     :: ErrorInd }       -- Error indication
    | RmdirRsp
      { rmdInd     :: ErrorInd }       -- Returns error indication
    | LinkRsp
      { lInd       :: ErrorInd }       -- Returns error indication
    | UnlinkRsp
      { uInd       :: ErrorInd }       -- Returns error indication
    | ChmodRsp
      { chmodInd   :: ErrorInd }       -- Returns error indication
    | ChownRsp
      { chownInd   :: ErrorInd }       -- Returns error indication
    ----------------------------------------
    ---- DIRECTORY INTERFACES (POSIX.1) ----
    ----------------------------------------
    | CloseDirRsp
      { closeDirInd :: ErrorInd }      -- Returns error indication
    | OpenDirRsp
      { openDirRsp  :: Maybe D.DIR }   -- Opened directory struct
    | ReadDirRsp
      { readDirRsp   :: Maybe D.DIR    -- Updated directory struct
      , readDirName  :: String         -- File name returned
      , readDirIsDir :: Bool           -- Is it a directory?
      }
    | RewindDirRsp                     -- No return value
    ------------------------------------------
    ---- STANDARD IO INTERFACES (POSIX.1) ----
    ------------------------------------------
    | GetsRsp (Maybe OskerCommand)     -- Returned Osker command, if any
    | GetCharRsp (Maybe Char)          -- Char, or Nothing if EOF
    | PutsRsp ErrorInd                 -- Returns error indication
    | PutCharRsp (Maybe Char)          -- Character written, or EOF on error
    -------------------------
    ---- OSKER RESPONSES ----
    -------------------------
    | LogAuditRsp                      -- For log audit system call
    -------------------------
    ---- DEMO RESPONSES -----
    -------------------------
    | FindRsp { foundFiles :: [FN.FileName] } -- Demo find script response
  
----------------------------------------------------------------------
-- Derive an integer code from a system request
----------------------------------------------------------------------
systemRequest2Code ::
  SystemRequest ->  -- System request to translate to a code
  SystemRequestCode -- The resulting code
systemRequest2Code NullSystemRequest    = NullSystemReqCode
----------------------------------------------
---- PROCESS CONTROL INTERFACES (POSIX.1) ----
----------------------------------------------
systemRequest2Code (ExitReq _)          = ExitReqCode
systemRequest2Code ForkReq              = ForkReqCode
systemRequest2Code GetPidReq            = GetPidReqCode
systemRequest2Code GetPPidReq           = GetPPidReqCode
systemRequest2Code (GetPGidReq _)       = GetPGidReqCode
systemRequest2Code (SetPGidReq _ _)     = SetPGidReqCode
systemRequest2Code SetSidReq            = SetSidReqCode
-------------------------------------
---- SIGNAL INTERFACES (POSIX.1) ----
-------------------------------------
systemRequest2Code (AlarmReq _)         = AlarmReqCode
systemRequest2Code PauseReq             = PauseReqCode
systemRequest2Code (SigActionReq _ _)   = SigActionReqCode
systemRequest2Code (SigProcMaskReq _ _) = SigProcMaskReqCode
systemRequest2Code SigPendingReq        = SigPendingReqCode
systemRequest2Code (KillReq _ _)        = KillReqCode
systemRequest2Code (SigSuspendReq _)    = SigSuspendReqCode
systemRequest2Code (SleepReq _)         = SleepReqCode
--------------------------------------------
---- MESSAGE QUEUE INTERFACES (POSIX.4) ----
--------------------------------------------
systemRequest2Code (MqOpenReq _ _ _ _)  = MqOpenReqCode
systemRequest2Code (MqCloseReq _)       = MqCloseReqCode
systemRequest2Code (MqSendReq _ _ _ _)  = MqSendReqCode
systemRequest2Code (MqReceiveReq _)     = MqReceiveReqCode
systemRequest2Code (MqNotifyReq _)      = MqNotifyReqCode
systemRequest2Code (MqGetAttrReq _)     = MqGetAttrReqCode
systemRequest2Code (MqSetAttrReq _ _)   = MqSetAttrReqCode
systemRequest2Code (MqUnlinkReq _ )     = MqUnlinkReqCode
-----------------------------------
---- FILE INTERFACES (POSIX.1) ----
-----------------------------------
systemRequest2Code (FPutsReq _ _)       = FPutsReqCode
systemRequest2Code (FPutcReq _ _)       = FPutcReqCode
systemRequest2Code (FGetsReq _ _)       = FGetsReqCode
systemRequest2Code (FGetcReq _)         = FGetcReqCode
systemRequest2Code (FTellReq _)         = FTellReqCode
systemRequest2Code (FSeekReq _ _ _)     = FSeekReqCode
systemRequest2Code (FOpenReq _ _)       = FOpenReqCode
systemRequest2Code (FCloseReq _)        = FCloseReqCode
systemRequest2Code (FReadReq _ _ _)     = FReadReqCode
systemRequest2Code (FWriteReq _ _ _ _)  = FWriteReqCode
systemRequest2Code (RewindReq _)        = RewindReqCode
systemRequest2Code (FFlushReq _)        = FFlushReqCode
systemRequest2Code (SetVBufReq _ _ _ _) = SetVBufReqCode
------------------------------------------
---- FILE SYSTEM INTERFACES (POSIX.1) ----
------------------------------------------
systemRequest2Code (MkdirReq _ _)       = MkdirReqCode
systemRequest2Code (RmdirReq _)         = RmdirReqCode
systemRequest2Code (LinkReq _ _)        = LinkReqCode
systemRequest2Code (UnlinkReq _)        = UnlinkReqCode
systemRequest2Code (ChmodReq _ _)       = ChmodReqCode
systemRequest2Code (ChownReq _ _ _)     = ChownReqCode
----------------------------------------
---- DIRECTORY INTERFACES (POSIX.1) ----
----------------------------------------
systemRequest2Code (CloseDirReq _)      = CloseDirReqCode
systemRequest2Code (OpenDirReq _)       = OpenDirReqCode
systemRequest2Code (ReadDirReq _)       = ReadDirReqCode
systemRequest2Code (RewindDirReq _)     = RewindDirReqCode
---------------------------------------
---- STANDARD INTERFACES (POSIX.1) ----
---------------------------------------
systemRequest2Code GetsReq              = GetsReqCode
systemRequest2Code GetCharReq           = GetCharReqCode
systemRequest2Code (PutsReq _)          = PutsReqCode
systemRequest2Code (PutCharReq _)       = PutCharReqCode
--------------------------
---- OSKER INTERFACES ----
--------------------------
systemRequest2Code SliceReq             = SliceReqCode
systemRequest2Code LogAuditReq          = LogAuditReqCode
systemRequest2Code LastSystemRequest    = LastSystemReqCode

systemRequest2Int :: SystemRequest -> Int
systemRequest2Int = fromEnum . systemRequest2Code

----------------------------------------------------------------------
-- Convert a system request to a string.
----------------------------------------------------------------------
systemRequest2String ::
    SystemRequest -> -- System request to translate to a string
    String           -- The resulting string
systemRequest2String NullSystemRequest = "Null Req"
----------------------------------------------
---- PROCESS CONTROL INTERFACES (POSIX.1) ----
----------------------------------------------
systemRequest2String (ExitReq _)       = "ExitReq"
systemRequest2String ForkReq           = "ForkReq"
systemRequest2String GetPidReq         = "GetPidReq"
systemRequest2String GetPPidReq        = "GetPPidReq"
systemRequest2String (GetPGidReq _)    = "GetPGidReq"
systemRequest2String (SetPGidReq _ _)  = "SetPGidReq"
systemRequest2String SetSidReq         = "SetSidReq"
-------------------------------------
---- SIGNAL INTERFACES (POSIX.1) ----
-------------------------------------
systemRequest2String (AlarmReq t)      = "AlarmReq " ++ show t
systemRequest2String (SigActionReq sig _) =
  "SigAction Req: " ++ show sig
systemRequest2String (SigProcMaskReq how sigset) =
  "SigProcMask Req: " ++ show how ++ ", " ++ show sigset
systemRequest2String (KillReq _ _)     = "KillReq"
systemRequest2String PauseReq          = "PauseReq"
systemRequest2String (SigSuspendReq sigset) =
  "SigSuspend " ++ show sigset
systemRequest2String SigPendingReq     = "SigPendingReq"
systemRequest2String (SleepReq t)      = "SleepReq " ++ show t
--------------------------------------------
---- MESSAGE QUEUE INTERFACES (POSIX.4) ----
--------------------------------------------
systemRequest2String (MqOpenReq fn flags mode attr) =
  "MqOpenReq " ++ show fn ++ ", " ++ show mode ++ ", " ++
  show flags ++ ", " ++ show attr
systemRequest2String (MqCloseReq desc) = "MqCloseReq " ++ show desc
systemRequest2String (MqSendReq q msg size pri) =
  "MqSendReq " ++ show q ++ ", " ++ show msg ++ ", " ++
  show size ++ ", " ++ show pri
systemRequest2String (MqReceiveReq q) = "MqReceiveReq " ++ show q
systemRequest2String (MqNotifyReq q)  = "MqNotifyReq " ++ show q
systemRequest2String (MqGetAttrReq q) = "MqGetAttrReq " ++ show q
systemRequest2String (MqSetAttrReq q attr) =
  "MqSetAttrReq " ++ show q ++ ", " ++ show attr
systemRequest2String (MqUnlinkReq name) =
  "MqUnlinkReq " ++ show name
-----------------------------------
---- FILE INTERFACES (POSIX.1) ----
-----------------------------------
systemRequest2String (FPutsReq s f) = "FPutsReq " ++ s ++ "/" ++ show f
systemRequest2String (FPutcReq c f) = "FPutcReq " ++ show c ++ "/" ++ show f
systemRequest2String (FGetsReq l f) = "FGetsReq " ++ show l ++ "/" ++ show f
systemRequest2String (FGetcReq f)   = "FGetcReq " ++ show f
systemRequest2String (FTellReq f)   = "FTellReq " ++ show f
systemRequest2String (FSeekReq f b w) =
  "FSeekReq " ++ show f ++ " / " ++ show b ++ " / " ++ show w
systemRequest2String (FOpenReq p m) = "FOpenReq " ++ show p ++ "/" ++ show m
systemRequest2String (FCloseReq f)  = "FCloseReq " ++ show f
systemRequest2String (FReadReq s n f) =
  "FReadReq " ++ show s ++ "/" ++ show n ++ "/" ++ show f
systemRequest2String (FWriteReq str s n f) =
  "FWriteReq " ++ str ++ "\n\t" ++ show s ++ "/" ++ show n ++ "/" ++ show f
systemRequest2String (RewindReq f)  = "RewindReq " ++ show f
systemRequest2String (FFlushReq f)  = "FFlushReq " ++ show f
systemRequest2String (SetVBufReq f _xsb m s) =
  "SetVBufReq " ++ show f ++ "/" ++ show m ++ "/" ++ show s
------------------------------------------
---- FILE SYSTEM INTERFACES (POSIX.1) ----
------------------------------------------
systemRequest2String (MkdirReq p m) = "MkdirReq " ++ p ++ "/" ++ show m
systemRequest2String (RmdirReq p)   = "RmdirReq " ++ p
systemRequest2String (LinkReq e n)  = "LinkReq " ++ e ++ " -> " ++ n
systemRequest2String (UnlinkReq p)  = "UnlinkReq " ++ p
systemRequest2String (ChmodReq p m) = "ChmodReq " ++ p ++ " ... " ++ show m
systemRequest2String (ChownReq p u g) =
  "ChownReq " ++ p ++ " ... " ++ show u ++ "/" ++ show g
----------------------------------------
---- DIRECTORY INTERFACES (POSIX.1) ----
----------------------------------------
systemRequest2String (CloseDirReq d)  = "CloseDirReq: " ++ show d
systemRequest2String (OpenDirReq p)   = "OpenDirReq: " ++ p
systemRequest2String (ReadDirReq d)   = "ReadDirReq: " ++ show d
systemRequest2String (RewindDirReq d) = "RewindDir: " ++ show d
------------------------------------------
---- STANDARD IO INTERFACES (POSIX.1) ----
------------------------------------------
systemRequest2String GetsReq          = "GetsReq"
systemRequest2String GetCharReq       = "GetCharReq"
systemRequest2String (PutsReq r)      = "PutsReq " ++ show r
systemRequest2String (PutCharReq c)   = "PutCharReq " ++ show c
--------------------------
---- OSKER INTERFACES ----
--------------------------
systemRequest2String SliceReq          = "SliceReq"
systemRequest2String LogAuditReq       = "LogAuditReq"
systemRequest2String LastSystemRequest = "LastSystemReqCode"

instance Show SystemRequest where
    show sysreq = systemRequest2String sysreq

instance Show SpecificSystemResponse where
    show sysrsp = systemResponse2String sysrsp

----------------------------------------------------------------------
-- Convert a system response to a string
----------------------------------------------------------------------
systemResponse2String ::
  SpecificSystemResponse -> -- System response to translate to string
  String                    -- The resulting string
systemResponse2String NullResponse  = "Null Rsp"
-----------------------------
---- REJECTION RESPONSES ----
-----------------------------
systemResponse2String (BogusSystemRequest sysreq) =
  "Bogus System Request: " ++ show sysreq
systemResponse2String (BogusException e) =
  "Bogus exception " ++ show e
systemResponse2String GlobalResourceFailure =
  "Global Resource Failure"
systemResponse2String (Reject errno)     = "Reject..." ++ show errno
systemResponse2String GlobalDeltaFailure = "Global Delta Failure"
----------------------------------------------
---- PROCESS CONTROL INTERFACES (POSIX.1) ----
----------------------------------------------
systemResponse2String (ForkRsp n)        = "Fork Rsp: " ++ show n
systemResponse2String ExitRsp            = "Exit Rsp"
systemResponse2String (GetPidRsp pid)    = "GetPid Rsp: " ++ show pid
systemResponse2String (GetPPidRsp pid)   = "GetPPid Rsp: " ++ show pid
systemResponse2String (GetPGidRsp pid)   = "GetPGid Rsp: " ++ show pid
systemResponse2String (SetPGidRsp ret)   = "SetPGid Rsp: " ++ show ret
systemResponse2String (SetSidRsp pid)    = "SetSid Rsp: " ++ show pid
-------------------------------------
---- SIGNAL INTERFACES (POSIX.1) ----
-------------------------------------
systemResponse2String (AlarmRsp f)       = "Alarm Rsp: " ++ show f
systemResponse2String (SigActionRsp r)   = "SigAction Rsp: " ++ show r
systemResponse2String (SigProcMaskRsp sigset) =
  "SigProcMask Rsp: " ++ show sigset
systemResponse2String (SigPendingRsp sigset) =
  "SigPending Rsp: " ++ show sigset
systemResponse2String (KillRsp ret)      = "Kill " ++ show ret
systemResponse2String PauseRsp           = "Pause Rsp"
systemResponse2String (SigSuspendRsp ret) =
  "SigSuspend Rsp: " ++ show ret
systemResponse2String (SleepRsp ret)     = "Sleep Rsp: " ++ show ret
--------------------------------------------
---- MESSAGE QUEUE INTERFACES (POSIX.4) ----
--------------------------------------------
systemResponse2String (MqOpenRsp desc attr) =
  "MqOpen Rsp: desc = " ++ show desc ++ ", " ++ show attr
systemResponse2String (MqCloseRsp ret) = "MqClose Rsp: " ++ show ret
systemResponse2String (MqSendRsp ret)  = "MqSend Rsp: " ++ show ret
systemResponse2String (MqReceiveRsp msg ret)  =
  "MqReceive Rsp: " ++ show ret ++ ", " ++ show msg
systemResponse2String (MqNotifyRsp event) = "MqNotifyRsp: " ++ show event
systemResponse2String (MqGetAttrRsp attr ret) =
  "MqGetAttrRsp: " ++ show attr ++ ", " ++ show ret
systemResponse2String (MqSetAttrRsp attr ret) =
  "MqSetAttrRsp: " ++ show attr ++ ", " ++ show ret
systemResponse2String (MqUnlinkRsp ret) = "MqUnlinkRsp: " ++ show ret
-----------------------------------
---- FILE INTERFACES (POSIX.1) ----
-----------------------------------
systemResponse2String (FPutsRsp e)    = "FPutsRsp "   ++ show e
systemResponse2String (FPutcRsp mc)   = "FPutcRsp "   ++ show mc
systemResponse2String (FGetsRsp ms)   = "FGetsRsp "   ++ show ms
systemResponse2String (FGetcRsp mc)   = "FGetcRsp "   ++ show mc
systemResponse2String (FTellRsp n)    = "FTellRsp "   ++ show n
systemResponse2String (FSeekRsp e)    = "FSeekRsp "   ++ show e
systemResponse2String (FOpenRsp mf)   = "FOpenRsp "   ++ show mf
systemResponse2String (FCloseRsp e)   = "FCloseRsp "  ++ show e
systemResponse2String (FReadRsp ms)   = "FReadRsp "   ++ show ms
systemResponse2String (FWriteRsp l)   = "FWriteRsp "  ++ show l
systemResponse2String RewindRsp       = "RewindRsp"
systemResponse2String (FFlushRsp e)   = "FFlushRsp "  ++ show e
systemResponse2String (SetVBufRsp e)  = "SetVBufRsp " ++ show e
------------------------------------------
---- FILE SYSTEM INTERFACES (POSIX.1) ----
------------------------------------------
systemResponse2String (MkdirRsp e)    = "MkdirRsp "   ++ show e
systemResponse2String (RmdirRsp e)    = "RmdirRsp "   ++ show e
systemResponse2String (LinkRsp e)     = "LinkRsp "    ++ show e
systemResponse2String (UnlinkRsp e)   = "UnlinkRsp "  ++ show e
systemResponse2String (ChmodRsp e)    = "ChmodRsp "   ++ show e
systemResponse2String (ChownRsp e)    = "ChownRsp "   ++ show e
----------------------------------------
---- DIRECTORY INTERFACES (POSIX.1) ----
----------------------------------------
systemResponse2String (CloseDirRsp e)  = "CloseDirRsp: " ++ show e
systemResponse2String (OpenDirRsp md)  = "OpenDirRsp: " ++ show md
systemResponse2String (ReadDirRsp md s i)=
  "ReadDirRsp: " ++ show md ++ ", " ++ s ++ ", " ++ show i
systemResponse2String RewindDirRsp     = "RewindDirRsp"
------------------------------------------
---- STANDARD IO INTERFACES (POSIX.1) ----
------------------------------------------
systemResponse2String (GetsRsp ms)    = "GetsRsp "    ++ show ms
systemResponse2String (GetCharRsp mc) = "GetCharRsp " ++ show mc
systemResponse2String (PutsRsp e)     = "PutsRsp "    ++ show e
systemResponse2String (PutCharRsp mc) = "PutCharRsp " ++ show mc
--------------------------
---- OSKER INTERFACES ----
--------------------------
systemResponse2String LogAuditRsp     = "Log audit Rsp"
--------------------------
---- DEMO INTERFACES -----
--------------------------
systemResponse2String (FindRsp fns)     = "Find: " ++ FN.outRelativeList fns

----------------------------------------------------------------------
-- Dynamic instances needed
----------------------------------------------------------------------

sysReqCon :: TyCon
sysReqCon = mkTyCon "System Request"
instance Typeable SystemRequest where
    typeOf _ = mkAppTy sysReqCon []

sysRspCon :: TyCon
sysRspCon = mkTyCon "System Response"
instance Typeable SystemResponse where
    typeOf _ = mkAppTy sysRspCon []

sysSpecRspCon :: TyCon
sysSpecRspCon = mkTyCon "Specific System Response"
instance Typeable SpecificSystemResponse where
    typeOf _ = mkAppTy sysSpecRspCon []

----------------------------------------------------------------------
-- The signals implemented by Osker.
-- This file contains the data and type declarations, and the
-- local library functions.
----------------------------------------------------------------------


-- The signals that must be handled for POSIX compliance
-- WARNING: Given the definition of sigfillset below, it matters that
-- SIGABRT is list first, and SIGTTOU is listed last.
-- Table taken from "The POSIX.1 standard, A Programmer's guide"
-- by Fred Zlotnik.
data Signal
    = SIGABRT -- Abnormal termination -- abort ()
    | SIGALRM -- Timeout              -- alarm ()
    | SIGFPE  -- Erroneous arithmetic operation //BMH
    | SIGHUP  -- Hangup on controlling terminal
    | SIGILL  -- Invalid hardware instruction //BMH
    | SIGINT  -- Interactive attention signal
    | SIGKILL -- Termination (cannot be caught / ignored)
    | SIGPIPE -- Write on a pipe not open for reading by any process
    | SIGQUIT -- Interactive termination signal (quit)
    | SIGSEGV -- Invalid memory reference
    | SIGTERM -- Termination signal
    | SIGUSR1 -- Application defined signal 1
    | SIGUSR2 -- Application defined signal 2
    | SIGCHLD -- Child process stopped or terminated
    | SIGCONT -- Continue
    | SIGSTOP -- Stop (cannot be caught / ignored)
    | SIGTSTP -- Interactive stop
    | SIGTTIN -- Read from controlling terminal by background process
    | SIGTTOU   -- Write to controlling terminal by background process
    deriving (Show, Eq, Ord, Enum)

-- The sigaction data structure, visible to users and to the system,
-- as specified in the POSIX standard
data SigAction
    = NullSigAction -- Unfortunate, required by sigaction () interface
    | SigAction
    { saHandler :: ExceptionHandler -- The exception handler
    , saMask    :: SignalMask       -- Extra mask during signal handling
    , saFlags   :: Int              -- I dunno yet
    }

instance Show SigAction where
    show NullSigAction = "NullSigAction"
    show (SigAction _handler mask flags) =
      "SigAction" ++ show mask ++ ", " ++ show flags

-- Possible actions when the signal comes
data SignalAction
    = NullSignalAction -- No action defined yet
    | TerminateProcess -- Kill the process
    | IgnoreSignal     -- Throw signal on the floor
    | ContinueProcess  -- Continue after a stop
    | Catch SigAction  -- Handle with the specified action

instance Show SignalAction where
    show NullSignalAction = "NullSignalAction"
    show TerminateProcess = "TerminateProcess"
    show IgnoreSignal     = "IgnoreSignal"
    show ContinueProcess  = "ContinueProcess"
    show (Catch _)        = "Catch *"

-- Determine if a signal action has a handerl
hasHandler :: SignalAction -> Bool
hasHandler (Catch _) = True
hasHandler _         = False

instance Eq SignalAction where
    NullSignalAction == NullSignalAction = True
    TerminateProcess == TerminateProcess = True
    IgnoreSignal     == IgnoreSignal     = True
    ContinueProcess  == ContinueProcess  = True
    _                == _                = False

-- The signal set is an important abstraction in the POSIX interfaces.
-- It is implemented here with the Edison set...
type SignalSet = [Signal]
-- The signal mask is just one use of the signal set
type SignalMask = SignalSet

-- The signal mask is maintained by the Osker system half. We felt
-- the structure might be useful to applications as well, so the
-- type definition is given here, visible to the world.
newtype SignalMap = SignalMap (FM.FiniteMap Signal SignalAction)

-- A utility program to get a signal call
getSigAction :: Signal -> SignalMap -> SignalAction
getSigAction signal (SignalMap sigmap) =
  FM.lookupWithDefaultFM sigmap NullSignalAction signal

-- Update the signal map
updateSignalMap :: SignalMap -> Signal -> SignalAction -> SignalMap
updateSignalMap (SignalMap sigmap) signal sigaction =
    SignalMap ( FM.addToFM sigmap signal sigaction )

instance Show SignalMap where
    show sm = outSignalMap sm

-- Print out a single entry of the signal map
outEntry :: (Signal, SignalAction) -> String
outEntry (sig, act) = "(" ++ show sig ++ "," ++ show act ++ ")"

-- Print out a list of entires for the signal map
outEntries :: [(Signal, SignalAction)] -> String
outEntries []  = ""
outEntries [e] = outEntry e
outEntries (e:es) = outEntry e ++ "\n\t" ++ outEntries es

-- Print out a signal map
outSignalMap :: SignalMap -> String
outSignalMap (SignalMap sigmap) = outEntries (FM.fmToList sigmap)

----------------------------------------------------------------------
-- Some of the posix signal calls can be, and are, implemented right
-- here as library calls, without going to the Osker kernel.
-- You can get man pages for these calls on your local Unix / Linux
-- system.
-- The signal set calls are implemented using the Edison library.
-- Many thanks to Chris Okasaki for this excellent library.
----------------------------------------------------------------------

-- Deliver an empty signal set
sigemptyset :: SignalSet
sigemptyset = []

-- Deliver a full empty set, i.e. a signal set with all the signals
-- in the required set (see data type Signal above)
sigfillset :: SignalSet
sigfillset = enumFromTo SIGABRT SIGTTOU

-- Add an element to a signal set
sigaddset :: SignalSet -> Signal -> SignalSet
sigaddset set sig = sig:set

-- Remove an element from a set
sigdelset :: SignalSet -> Signal -> SignalSet
sigdelset = flip delete

-- Check for membership in a set
sigismember :: SignalSet -> Signal -> Bool
sigismember = flip elem

-- Create a set from a list of signals (added by Osker)
siglisttoset :: [Signal] -> SignalSet
siglisttoset = id

-- Union of two signal sets (added by Osker)
sigunion :: SignalSet -> SignalSet -> SignalSet
sigunion s1 s2 = nub ( union s1 s2 )

-- Intersection of two signal sets (added by Osker)
sigintersect :: SignalSet -> SignalSet -> SignalSet
sigintersect = intersect

-- Complement of a signal set
sigcomplement :: SignalSet -> SignalSet
sigcomplement s = sigcomplement' sigfillset s

sigcomplement' :: SignalSet -> SignalSet -> SignalSet
sigcomplement' [] _s = []
sigcomplement' (u:us) s = 
  if elem u s
  then us
  else u:(sigcomplement' us s)

-- Definitions for sigprocmask():
-- How sigprocmask should update the signal mask
data SigProcMaskParam
    = SIG_BLOCK   -- Proc mask becomes union of old and new
    | SIG_UNBLOCK -- Proc mask becomes intersection of old and
                  -- complement of new
    | SIG_SETMASK -- Proc mask becomes new
    deriving (Eq, Ord, Enum, Show)

-- When used as a parameter, the signal mask can be Null, thus
-- unfortunately we need this data type.
data SignalSetParam
    = NullSignalSetParam
    | SignalSetParam SignalSet
    deriving (Eq, Show)

----------------------------------------------------------------------
-- Define the derault actions on each signal
----------------------------------------------------------------------

defaultSignalActions :: SignalMap
defaultSignalActions =
  SignalMap . FM.listToFM $
  [ (SIGABRT , TerminateProcess)
  , (SIGALRM , TerminateProcess)
  , (SIGFPE  , TerminateProcess)
  , (SIGHUP  , TerminateProcess)
  , (SIGILL  , TerminateProcess)
  , (SIGINT  , TerminateProcess)
  , (SIGKILL , TerminateProcess)
  , (SIGPIPE , TerminateProcess)
  , (SIGQUIT , TerminateProcess)
  , (SIGSEGV , TerminateProcess)
  , (SIGTERM , TerminateProcess)
  , (SIGUSR1 , TerminateProcess)
  , (SIGUSR2 , TerminateProcess)
  , (SIGCHLD , IgnoreSignal)
  , (SIGCONT , ContinueProcess)
  , (SIGSTOP , TerminateProcess)
  , (SIGTSTP , TerminateProcess)
  , (SIGTTIN , TerminateProcess)
  , (SIGTTOU , TerminateProcess)
  ]
----------------------------------------------------------------------
--
-- The user monad, supporting construction of user programs. It is
-- based once again on the resumption / state monad.
--
-- The timer tick is part of the state, i.e. part of the environment
-- of the user. Each bind operation advances the timer tick by one.
-- Thus, eventually, each operation in the user monad will be a
-- constructor that corresponds to one timer tick, i.e. one machine
-- language instruction. For now, the monad is considerably richer
-- than that.
--
----------------------------------------------------------------------


-- The internal state of the user monad. The ticker can be made
-- visible to the user, since the user can count how many instructions
-- were executed, if so desired.
-- When the user process executes a trap, then the time slice for that
-- process is done. The trap is fished out of the user state by the
-- kernel. This is a good model of how a trap works in an operating
-- system such as Linux.
data UserState =
    UserState { ticker     :: Int            -- Timer tick
              , slice      :: Int            -- Current time slice
              , trap       :: SystemRequest  -- Current user trap
              , response   :: SystemResponse -- Response to last call
              } deriving ( Show )

-- The user monad is a specialization of the resumption / state monad.
type U a = R.RSE UserState a

-- Print routines
putStr :: String -> U ()
putStr = R.putString
putStrLn :: String -> U ()
putStrLn = R.putLine

setTrap :: SystemRequest -> UserState -> UserState
setTrap req us = us { trap = req }

-- Make a system call
sysCall :: SystemRequest -> U ()
sysCall req = R.pauseSt ( setTrap req )

-- Get the system response out of the state
getRsp :: U SystemResponse
getRsp = R.observe response

-- Complete a system call, returning the response
osker :: SystemRequest -> U SystemResponse
osker req =
  do { sysCall req
     ; getRsp
     }

-- The user half program is a monad action returning ()
type UserProgram = U ()

startUserHalf :: Int -> U ()
startUserHalf n = R.putLine ("Test " ++ show n ++ " user process")

-- Fail a user program
fail :: Int -> String -> U ()
fail n str = putStrLn ( "\n///[User Half]...\t\t\ttest " ++ show n ++ " fails: " ++ str )
             >> error ( "SC.fail" )

----------------------------------------------------------------------
-- Manipulations by the system half
----------------------------------------------------------------------

-- When the user half pauses, it always produces a system request.
data UserTrap
    = UserTrap
      { state   :: UserState     -- Resulting state of user process
      , cont    :: U ()          -- Program location of user process
      , request :: SystemRequest -- Request made by the user process
      }
    | Complete

-- Determine if a user process has completed
isComplete :: UserTrap -> Bool
isComplete Complete = True
isComplete _        = False

instance Show UserTrap where
    show (UserTrap st _ req) = "UserTrap " ++ show st ++ ", " ++ show req
    show Complete            = "Complete"

-- Grant another time slice
newSlice :: Int -> UserState -> UserState
newSlice n us = us { slice = n }

----------------------------------------------------------------------
-- xception handling by user programs
----------------------------------------------------------------------

-- The different kinds of user half exceptions are all encapsulated
-- in this data type.
data UserException =
    -- The arrival of a POSIX.1 signal.
    -- At present, this is the only one implmenented
    UserSignal Signal
    deriving (Eq, Ord, Show)

-- Project out the signal
projSig :: UserException -> Signal
projSig (UserSignal sig) = sig

-- An exception handler
type ExceptionHandler = UserException -> U ()

-- An instance of dynamic
uheCon :: TyCon
uheCon = mkTyCon "User half exception"
instance Typeable UserException where
    typeOf _ = mkAppTy uheCon []
