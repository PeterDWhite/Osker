-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreCallTable ( kernelCoreCallTable ) where

----------------------------------------------------------------------
-- The specialization of the executive call table to the system half
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Osker imports
import qualified OskerDelta as PD
import qualified PartitionedState as PS
import qualified ExecutiveCallTable as ECT
import qualified KernelCoreActor as KCA
import qualified KernelCoreMQ as KCMQ
import qualified KernelCoreSignal as KCS
import qualified OskerMessage as OM
import qualified ProcessControl as PC
import qualified GlobalPartitionedStateKey as GPSK
import qualified KernelCoreMessage as KCM
import qualified CallTableIndex as CTI
import qualified Allocate as A

type KernelCoreCallTable r =
    ECT.ExecutiveCallTable
      KCA.KernelCorePartitionedStateKey
      KCA.KernelCorePartitionedStateElt
      () -- No global state for the kernel core
      OM.OskerPay
      r

-- The system call table maps the system call to the processing
-- components needed to carry out the system call within Osker.
kernelCoreCallTable :: KernelCoreCallTable r
-- Defining this will be the real work of the kernel implementation
kernelCoreCallTable =
    FM.listToFM
    [
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.ResourceRequestCode),
      ECT.ExecutiveCallData
        A.allocate
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKDomainModel)]
                   [])
           (PS.PSG [] []))
        1 -- Priority
     ),
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.SignalProcessCode),
      ECT.ExecutiveCallData
        KCS.signalProcess
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData)]
                   [])
           (PS.PSG [] []))
        1 -- Priority
     ),
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.ForkProcessCode),
      ECT.ExecutiveCallData
        PC.forkProcess
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData),
                    GPSK.GPSK (Right GPSK.GPSKPidGen),
                    GPSK.GPSK (Right GPSK.GPSKKernelCoreChan),
                    GPSK.GPSK (Right GPSK.GPSKFSCoreChan),
                    GPSK.GPSK (Right GPSK.GPSKFSRoot),
                    GPSK.GPSK (Right GPSK.GPSKDeviceMap),
                    GPSK.GPSK (Right GPSK.GPSKDomainModel),
                    GPSK.GPSK (Right GPSK.GPSKKernelCoreTid)]
                   [GPSK.GPSK (Right GPSK.GPSKDomainModel),
                    GPSK.GPSK (Right GPSK.GPSKPidGen),
                    GPSK.GPSK (Right GPSK.GPSKJobControlData)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Create a message queue
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.CreateMQCode),
      ECT.ExecutiveCallData
        KCMQ.createMQ
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)]
                   [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Open a message queue that exists
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.OpenMQCode),
      ECT.ExecutiveCallData
        KCMQ.openMQ
        (PD.OskerDelta
           (PS.PSG [ GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)
                   , GPSK.GPSK (Right GPSK.GPSKDomainModel)
                   ]
                   [ GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Close a message queue
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.CloseMQCode),
      ECT.ExecutiveCallData
        KCMQ.closeMQ
        (PD.OskerDelta
           (PS.PSG [ GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)]
                   [ GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Unlink a message queue
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.UnlinkMQCode),
      ECT.ExecutiveCallData
        KCMQ.unlinkMQ
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)]
                   [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Send to a message queue
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.SendMQCode),
      ECT.ExecutiveCallData
        KCMQ.sendMQ
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)]
                   [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Receive from a message queue
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.ReceiveMQCode),
      ECT.ExecutiveCallData
        KCMQ.receiveMQ
        (PD.OskerDelta
           (PS.PSG [ GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)
                   , GPSK.GPSK (Right GPSK.GPSKDomainModel)
                   ]
                   [GPSK.GPSK (Right GPSK.GPSKMessageQueueMap)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Exit from a process
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.ExitProcessCode),
      ECT.ExecutiveCallData
        PC.exitProcess
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData)]
                   [GPSK.GPSK (Right GPSK.GPSKJobControlData)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Local (process control) work: update the process
     (CTI.CallTableIndex (fromEnum KCM.FromProcessControlType)
                         (fromEnum OM.UpdateProcessCodePCR),
      ECT.ExecutiveCallData
        PC.updateProcess
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData)]
                   [GPSK.GPSK (Right GPSK.GPSKJobControlData)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     -- Local (process control) work: finish the fork
     (CTI.CallTableIndex (fromEnum KCM.FromProcessControlType)
                         (fromEnum OM.FinishProcessCodePCR),
      ECT.ExecutiveCallData
        PC.finishProcess
        (PD.OskerDelta
           (PS.PSG [] [])
           (PS.PSG [] []))
        1 -- Priority
     ),
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.AddProcessGroupCode),
      ECT.ExecutiveCallData
        PC.addProcessGroup
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData)]
                   [GPSK.GPSK (Right GPSK.GPSKJobControlData)])
           (PS.PSG [] []))
        1 -- Priority
     ),
     (CTI.CallTableIndex (fromEnum KCM.ToKernelCoreType)
                         (fromEnum OM.AddSessionCode),
      ECT.ExecutiveCallData
        PC.addSession
        (PD.OskerDelta
           (PS.PSG [GPSK.GPSK (Right GPSK.GPSKJobControlData)]
                   [GPSK.GPSK (Right GPSK.GPSKJobControlData)])
           (PS.PSG [] []))
        1 -- Priority
     )
    ]
