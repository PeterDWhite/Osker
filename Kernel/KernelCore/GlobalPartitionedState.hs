-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module GlobalPartitionedState
    ( GlobalPartitionedState         -- Partitioned state for kernel core
    , initGlobalPartitionedState     -- Initialize the global part. state
    , GlobalPartitionedStateElt (..) -- An element of the global part. state
      -- Specialized updates
    , updateJcd                      -- Update the job control data
    , updateKcTid                    -- Update the kernel core tid
    , updatePidGen                   -- Update the pid generator of GPS
    ) where

----------------------------------------------------------------------
-- The fractured state for the kernel core
----------------------------------------------------------------------

-- Haskell imports
import qualified OskerConcurrent as C
-- Utility imports
import qualified IntGen as IG
-- Posix imports
import qualified FileName as FN
-- Domain imports
import qualified DomainGraph as DG
import qualified GlobalResource as GR
-- Osker imports
import qualified PartitionedState as PS
import qualified LocalPartitionedState as LPS
import qualified OskerPool as PP
import qualified JobControlData as JCD
import qualified TCBPool as TCBP
import qualified GlobalPartitionedStateKey as GPSK
import qualified DeviceMap as DM
import qualified OskerMessage as OM
import qualified MQ as MQ

data GlobalPartitionedStateElt -- Abbreviation gfse
    = GfseTCB             TCBP.TCBPool       -- TCB Pool
    | GfsePlatform        DG.DomainGraph     -- Process / domain model
    | GfseKernelCoreTid   C.ThreadId         -- Tid of kernel core
    | GfseKernelCoreChan  OM.OskerChannel    -- Input chan of kernel core
    | GfseFSCoreChan      OM.OskerChannel    -- Input chan of file system
    | GfseFSRoot          FN.FileName        -- Root of the file system
    | GfseJobControlData  JCD.JobControlData -- Domain, session, process
                                             -- group, process data
    | GfsePidGen          IG.IntGen          -- PID Generator
    | GfseDeviceMap       DM.DeviceMap       -- Device info table
      -- Message queue descriptor pool
    | GfseMqd             MQ.MessageQueueDescriptor
    | GfseSemaphore                          -- Currently not used
    | GfseMessageQueueMap MQ.MessageQueueMap -- MQ map 

instance Show GlobalPartitionedStateElt where
    show (GfseTCB p)           = "GfseTCB: " ++ show p
    show (GfsePlatform p)      = "GfsePlatform: " ++ show p
    show (GfseKernelCoreTid _tid) = "GfseKernelCoreTid: "
    show (GfseKernelCoreChan chan) = "GfseKernelCoreChan " ++ show chan
    show (GfseFSCoreChan chan) = "GfseFileSystemChan " ++ show chan
    show (GfseFSRoot root)     = "GfseFSRoot " ++ show root
    show (GfseJobControlData jcd) =
      "GfseJobControlData: " ++ show jcd
    show (GfsePidGen g)        = "GfsePidGen: " ++ show g
    show (GfseMqd mqd)         = "GfseMqd: " ++ show mqd
    show (GfseDeviceMap dm)    = "GfseDeviceMap: " ++ show dm
    show (GfseSemaphore)       = "GfseSemaphore"
    show (GfseMessageQueueMap mqm) =
      "GfseMessageQueueMap: " ++ show mqm

type GlobalPartitionedState =
    LPS.LocalPartitionedState
      GPSK.GlobalPartitionedStateKey
      GlobalPartitionedStateElt

-- Initialize the fractured state with the system half components
initGlobalPartitionedState :: GlobalPartitionedState
initGlobalPartitionedState =
  PS.update
    (PS.empty)
    [PS.AddPS (GPSK.GPSK (Left GR.GRTimer)) (LPS.PoolValue (PP.poolMkEmpty 2))]
    GPSK.freeDelta

-- Update the job control data in the global partitioned state
updateJcd ::
    JCD.JobControlData -> GlobalPartitionedState -> GlobalPartitionedState
updateJcd jcd gps =
  PS.update gps
            [PS.AddPS (GPSK.GPSK (Right GPSK.GPSKJobControlData))
                      (LPS.VanillaValue (GfseJobControlData jcd))]
            GPSK.freeDelta

-- Update the kernel core Tid field of the global partitioned state
updateKcTid :: C.ThreadId -> GlobalPartitionedState -> GlobalPartitionedState
updateKcTid kcTid gps =
  PS.update gps
            [PS.AddPS (GPSK.GPSK (Right GPSK.GPSKKernelCoreTid))
                      (LPS.VanillaValue (GfseKernelCoreTid kcTid))]
            GPSK.freeDelta

-- Update the Pid Generator in the global partitioned stater
updatePidGen :: IG.IntGen -> GlobalPartitionedState -> GlobalPartitionedState
updatePidGen pidGen gps =
  PS.update gps
            [PS.AddPS (GPSK.GPSK (Right GPSK.GPSKPidGen))
                      (LPS.VanillaValue (GfsePidGen pidGen))]
            GPSK.freeDelta
