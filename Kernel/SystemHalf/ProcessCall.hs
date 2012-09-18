-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ProcessCall
    ( forkProgram     -- Resumption program for fork system call
    , exitProgram     -- Resumption program for exit system call
    , getPidProgram   -- Resumption program for getPid system call
    , getPPidProgram  -- Resumption program for getPPid system call
    , getPGidProgram  -- Resumption program for getPGid system call
    , setPGidProgram  -- Resumption program for setPGic system call
    , setSidProgram   -- Resumption program for setSid system call
    ) where

----------------------------------------------------------------------
-- The process control related system calls
----------------------s------------------------------------------------

-- Haskell imports
import qualified Monad as M
-- Posix imports
import qualified SystemCall as SC
import qualified ProcessId as PID
import qualified Errno as ER
-- Domain imports
import qualified DomainGraph as DOMG
-- Actor imports
import qualified ActorThread as AT
-- Osker imports
import qualified SystemHalfActor as SHA
import qualified OskerMessage as OM
import qualified ProcessTags as PT

----------------------------------------------------------------------
-- The fork system call
----------------------------------------------------------------------
getPIDFromMsgData :: OM.OskerPay -> Maybe PID.ProcessId
getPIDFromMsgData msgData =
  case msgData of
    OM.FromKernelCore kcr -> 
      case kcr of
        OM.ForkResponse mpid -> mpid
        _otherwise -> error "Expecting Fork Response"
    _otherwise -> error "Message not from kernel: getPIDFromResponse"

-- The actor program for the fork system call: segment 1
forkProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    DOMG.ProcessNodeData -> -- Process node from process domain model
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
forkProgram b tags node pay =
  do { -- Let the kernel core take care of the fork
     ; ( SHA.toKernelCore b ) (OM.ForkProcess tags node)
     ; ( SHA.putStrLn b ) ("forkProgram: Got message from kernel core")
     ; kcrsp <- SHA.getFromChan b
     ; case getPIDFromMsgData pay of
         Nothing  ->
           SHA.sysRsp (Just ER.EAGAIN) (SC.ForkRsp 0)
         Just forkedpid ->
           SHA.sysRsp SC.noError (SC.ForkRsp forkedpid)
     }

----------------------------------------------------------------------
-- The exit system call
----------------------------------------------------------------------
-- The actor program for the exit system call, only one segment
exitProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
exitProgram b tags _pay =
  do { ( SHA.putStrLn b ) ("exitProgram")
     ; ( SHA.toKernelCore b ) (OM.ExitProcess (PT.processName tags) )
     ; return (AT.Exit)
     }

----------------------------------------------------------------------
-- The getPid system call, only one segment
----------------------------------------------------------------------
getPidProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
getPidProgram _b tags _pay =
  SHA.sysRsp SC.noError (SC.GetPidRsp (PT.processId tags))

----------------------------------------------------------------------
-- The getPPid system call, only one segment
----------------------------------------------------------------------
getPPidProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
getPPidProgram _b tags _pay =
  SHA.sysRsp SC.noError (SC.GetPPidRsp (PT.parentPid tags))

----------------------------------------------------------------------
-- The getPGrp system call, only one segment
----------------------------------------------------------------------
getPGidProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
getPGidProgram _b tags pay =
  do { let SC.GetPGidReq pid = OM.getSysReq pay
     ; if pid == PID.nullPID
       then -- When pid is zero, then return the process
            -- group id of the calling process
            SHA.sysRsp SC.noError (SC.GetPGidRsp (PT.processGroupId tags))
       else error "getPGidProgram: Write this soon, pid /= 0"
     }

----------------------------------------------------------------------
-- The setPGid system call
----------------------------------------------------------------------
setPGidProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
setPGidProgram b tags pay =
  do { let SC.SetPGidReq pid pgid = OM.getSysReq pay
     ; ( SHA.putStrLn b ) ("pid = " ++ show pid)
       -- When the incoming pid is zero, then the calling process
       -- specified.
     ; ( SHA.toKernelCore b ) (OM.AddProcessGroup pid pgid tags)
     ; kcrsp <- SHA.getFromChan b
     ; let OM.FromKernelCore (OM.AddProcessGroupResponse err) = kcrsp
        -- Response to the caller
     ; if err == ER.NOERROR
       then SHA.sysRsp SC.noError (SC.SetPGidRsp 0)
       else SHA.sysRsp (Just err) (SC.SetPGidRsp (-1))
     }

----------------------------------------------------------------------
-- The setsid system call
----------------------------------------------------------------------
setSidProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the prgoram
    PT.ProcessTags       -> -- Ids about the process
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram   -- In the system half program monad
setSidProgram b tags _pay =
  do { -- When the incoming pid is zero, then the calling process specified.
     ; ( SHA.toKernelCore b ) (OM.AddSession tags)
     ; ( SHA.putStrLn b ) ("setSidProgram: After sending to kernel core")
     ; kcrsp <- SHA.getFromChan b
     ; let OM.FromKernelCore (OM.AddSessionResponse err) = kcrsp
        -- Response to the caller
     ; if err == ER.NOERROR
       then SHA.sysRsp SC.noError (SC.SetSidRsp 0)
       else SHA.sysRsp (Just err) (SC.SetSidRsp (-1))
     }
