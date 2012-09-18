-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCore
    ( kernelCoreResponseAction        -- Response to message action
    , initKernelCorePartitionedState  -- Initialize the partitioned state
    , kernelCoreScreen                -- Screen incoming messages
    , kernelCoreReject                -- Reject incoming messages
    , KernelCoreExecutiveState        -- State of kc executive
    ) where

----------------------------------------------------------------------
-- Specialize the actor concept to the system half
----------------------------------------------------------------------

-- Haskell imports
import qualified OskerConcurrent as C
-- Actor imports
import qualified DomainBraid as DB
-- Utility imports
import qualified IntGen as IG
-- Posix imports
import qualified Errno as ER
import qualified FileName as FN
-- Domain imports
import qualified GlobalResource as GR
import qualified DomainGraph as DG
-- Osker imports
import qualified GlobalPartitionedStateKey as GPSK
import qualified ExecutiveState as ES
import qualified MQ as MQ
import qualified GlobalPartitionedState as GPS
import qualified KernelCoreActor as KCA
import qualified OskerPool as PP
import qualified PartitionedState as PS
import qualified LocalPartitionedState as LPS
import qualified OskerDelta as PD
import qualified ExecutiveParameters as EP
import qualified OskerMessage as OM
import qualified Message as M
import qualified JobControlData as JCD
import qualified DeviceMap as DM

----------------------------------------------------------------------
-- Rename the top level executive functions for the system half
----------------------------------------------------------------------

-- The system half action to output a response
-- This will be provided as parameter to the kernelCoreIOShell
-- (after partial evaluation)
kernelCoreResponseAction ::
    OM.OskerChannel -> OM.OskerRspMsg -> DB.DomainBraid ()
kernelCoreResponseAction chan sm = DB.lift (C.writeChan (OM.projPC chan) sm)

-- Initialize the fractured state with the system half components
initKernelCorePartitionedState ::
    DG.DomainGraph     ->        -- Process / domain model
    IG.IntGen          ->        -- Process Id generator
    JCD.JobControlData ->        -- Domain, session, pgrp, process info
    DM.DeviceMap       ->        -- Osker global vars
    OM.OskerChannel    ->        -- Input channel to kernel core
    OM.OskerChannel    ->        -- Input channel to file system core
    FN.FileName        ->        -- File system root
    KCA.KernelCorePartitionedState -- Partitioned state for kernel core
initKernelCorePartitionedState
  domGraph pidGen jcd devmap kcChan fsCoreChan fsRoot =
    PS.update
      (PS.empty)
      [
      PS.AddPS (GPSK.GPSK (Left GR.GRTimer))
               (LPS.PoolValue (PP.poolMkEmpty 20)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKDomainModel))
               (LPS.VanillaValue (GPS.GfsePlatform domGraph)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKMessageQueueMap))
               (LPS.VanillaValue (GPS.GfseMessageQueueMap MQ.emptyMQM)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKKernelCoreChan))
               (LPS.VanillaValue (GPS.GfseKernelCoreChan kcChan)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKFSCoreChan))
               (LPS.VanillaValue (GPS.GfseFSCoreChan fsCoreChan)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKFSRoot))
               (LPS.VanillaValue (GPS.GfseFSRoot fsRoot)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKJobControlData))
               (LPS.VanillaValue (GPS.GfseJobControlData jcd)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKPidGen))
               (LPS.VanillaValue (GPS.GfsePidGen pidGen)),
      PS.AddPS (GPSK.GPSK (Right GPSK.GPSKDeviceMap))
               (LPS.VanillaValue (GPS.GfseDeviceMap devmap))
      ]
      GPSK.freeDelta

type KernelCoreExecutiveState r =
    ES.ExecutiveState 
       GPSK.GlobalPartitionedStateKey -- Partitioned state key
       GPS.GlobalPartitionedStateElt  -- Partitioned state conponent
       ()                             -- No global key needed in kernel core
       OM.OskerPay                    -- Specialize message
       r

type KernelCoreOskerDelta =
    PD.OskerDelta
       GPSK.GlobalPartitionedStateKey -- Key to kernel core state
       ()                             -- Nothing global to kernel core

type KernelCoreScreenResult =
    EP.ScreenResult
       GPSK.GlobalPartitionedStateKey -- Partitioned state key
       GPS.GlobalPartitionedStateElt  -- Partitioned state conponent
       OM.OskerPay                    -- Specialize message

-- Screen function for the kernel core
kernelCoreScreen ::
    KernelCoreOskerDelta  -> -- Local executive state
    KernelCoreOskerDelta  -> -- For conflict resolution
    DG.ProcessNodeData    -> -- Process / domain model node
    OM.OskerRspMsg        -> -- Incoming Message
    KernelCoreScreenResult
-- For right now, this is very simple
kernelCoreScreen _delta1 _delta2 _node _msg = EP.RunNow

-- Reject an incoming message for the system half
kernelCoreReject ::
    OM.OskerRspMsg -> -- Incoming message
    ER.Errno       -> -- Reason for rejection
    OM.OskerRspMsg    -- Converted to reject
-- We do not need this function yet.
kernelCoreReject _msg _errno = error "kernelCoreReject not written yet"
