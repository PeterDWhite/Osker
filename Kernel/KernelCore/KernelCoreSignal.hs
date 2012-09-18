-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreSignal
    ( signalProcess -- Send a signal to a process
    ) where

----------------------------------------------------------------------
-- Kernel core support for signal POSIX interfaces
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified KernelCoreActor as KCA
import qualified UnsafeAS as UAS
import qualified OskerMessage as OM
import qualified KernelCoreActions as KCACT
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified JobControlData as JCD

-- An action to send a signal to the requested pid, segment 1
signalProcess :: KCA.KernelCoreSegment r
signalProcess pay =
  do { let kcr = OM.getKernelCoreReq pay
           OM.SignalProcess { OM.spPid = pid, OM.spSig = sig } = kcr
     ; UAS.unsafeASIO ("signalProcess: " ++ show pid ++ ", " ++ show sig)
     -- Access the job control data
     ; GPS.GfseJobControlData jcd <-
         KCACT.getGlobalStateElement
           (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     -- Get the process name of the receiving process
     ; let pidMap = JCD.jcdPidMap jcd
           mpname = FM.lookupFM pidMap pid
     ; case mpname of
         Nothing -> error "signalProcess.3"
         Just pname ->
           do { UAS.unsafeASIO ("signalProcess, pname: " ++
                                 PN.outProcessName pname)
              ; let tpay = OM.toSysHalf (OM.ReceiveSignal pid sig) pname
              ; KCA.waitSegment tpay (signalProcess2 pname) []
              }
     }

-- An action to send a signal to the requested pid, segment 2
signalProcess2 :: PN.ProcessName -> KCA.KernelCoreSegment r
signalProcess2 pname pay =
  do { let OM.ReceiveSignalComply flag = OM.getKernelCoreComply pay
     ; UAS.unsafeASIO ("signalProcess: " ++ show flag)
       -- A response is generated for the sending proc
     ; KCA.shRsp (OM.SignalProcessResponse flag) pname
     }
