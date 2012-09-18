-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreActions
    ( getDomainModel        -- Get the process / domain model from GPS
    , getGlobalStateElement -- Get an element of the global partitioned state
    , updateThreadMap       -- Update the thread map
    , reduceThreadMap       -- Delete elements from the thread map
    , updateProcessMap      -- Update the process map
    , reduceProcessMap      -- Delete elements from the process map
    , reducePidMap          -- Delete elements from the pid map
    , updateJobControlData  -- Update the job control data
    ) where

----------------------------------------------------------------------
-- Some useful actions for kernel core actor programs
----------------------------------------------------------------------

-- Haskell imports
import qualified OskerConcurrent as C
import qualified FiniteMap as FM
-- Posix imports
import qualified ProcessId as PID
-- Domain imports
import qualified DomainGraph as DG
-- Osker imports
import qualified KernelCoreActor as KCA
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified OskerMessage as OM
import qualified JobControlData as JCD
import qualified ProcessName as PN
import qualified Process as P

-- Action to fish out the process / domain model
getDomainModel :: KCA.KernelCoreActor r DG.DomainGraph
getDomainModel =
  do { mdomain <- KCA.getVanillaElt (GPSK.GPSK (Right GPSK.GPSKDomainModel))
     ; case mdomain of
         Nothing -> error "No process domain model"
         Just (GPS.GfsePlatform platform) -> return platform
         _otherwise -> error "Sought process domain model, got junk"
     }

-- Action to fish out a specified global state element
getGlobalStateElement ::
    GPSK.GlobalPartitionedStateKey ->
    KCA.KernelCoreActor r GPS.GlobalPartitionedStateElt
getGlobalStateElement gfsk =
  do { melt <- KCA.getVanillaElt gfsk
     ; case melt of
         Nothing ->
           error ("GPS element sought, not found: " ++ show gfsk)
         Just elt -> return elt
     }

-- Action to update the job control data
updateJobControlData :: JCD.JobControlData -> KCA.KernelCoreActor r ()
updateJobControlData jcd =
  KCA.updateVanillaElt
     (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     (GPS.GfseJobControlData jcd)

-- Action to update the thread map
updateThreadMap ::
    C.ThreadId -> PN.ProcessName -> KCA.KernelCoreActor r ()
updateThreadMap tid pname =
  do { GPS.GfseJobControlData jcd <-
         getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let threadMap = JCD.jcdThreadMap jcd
           threadMap' = FM.addToFM threadMap tid pname
           jcd' = jcd { JCD.jcdThreadMap = threadMap' }
     ; KCA.updateVanillaElt
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
          (GPS.GfseJobControlData jcd')
     }

-- Action to reduce the thread map
reduceThreadMap :: C.ThreadId -> KCA.KernelCoreActor r ()
reduceThreadMap tid =
  do { GPS.GfseJobControlData jcd <-
         getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let threadMap = JCD.jcdThreadMap jcd
           threadMap' = FM.delFromFM threadMap tid
           jcd' = jcd { JCD.jcdThreadMap = threadMap' }
     ; KCA.updateVanillaElt
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
          (GPS.GfseJobControlData jcd')
     }

-- Action to update the process map
updateProcessMap ::
    PN.ProcessName            ->
    P.Process OM.OskerRspMsg  ->
    KCA.KernelCoreActor r ()
updateProcessMap pname process =
  do { GPS.GfseJobControlData jcd <-
         getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let processMap = JCD.jcdProcessMap jcd
           processMap' = FM.addToFM processMap pname process
           jcd' = jcd { JCD.jcdProcessMap = processMap' }
     ; KCA.updateVanillaElt
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
          (GPS.GfseJobControlData jcd')
     }

-- Action to reduce the process map
reduceProcessMap :: PN.ProcessName -> KCA.KernelCoreActor r ()
reduceProcessMap pname =
  do { GPS.GfseJobControlData jcd <-
         getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let processMap = JCD.jcdProcessMap jcd
           processMap' = FM.delFromFM processMap pname
           jcd' = jcd { JCD.jcdProcessMap = processMap' }
     ; KCA.updateVanillaElt
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
          (GPS.GfseJobControlData jcd')
     }

-- Action to reduce the pid map
reducePidMap :: PID.ProcessId -> KCA.KernelCoreActor r ()
reducePidMap pid =
  do { GPS.GfseJobControlData jcd <-
         getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let pidMap = JCD.jcdPidMap jcd
           pidMap' = FM.delFromFM pidMap pid
           jcd' = jcd { JCD.jcdPidMap = pidMap' }
     ; KCA.updateVanillaElt
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
          (GPS.GfseJobControlData jcd')
     }
