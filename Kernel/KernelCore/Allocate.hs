-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Allocate
    ( allocateGlobalResource
    , allocate
    ) where

----------------------------------------------------------------------
-- Global resource allocation
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Recursive graph imports
import qualified Deep as D
-- Posix imports
import qualified ProcessName as PN
import qualified DomainGraph as DOMG
-- Domain model imports
import qualified DomainGraph as DG
import qualified GlobalResource as GR
import qualified ResourceLimit as RL
-- Osker imports
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified KernelCoreActor as KCA
import qualified KernelCoreActions as KCACT
import qualified UnsafeAS as UAS
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- Now for the interfaces using the domain / process model
----------------------------------------------------------------------

allocateGlobalResource ::
    PN.ProcessName                 -> -- Process wanting resources
    GPS.GlobalPartitionedState     -> -- The global state
    GPSK.GlobalPartitionedStateKey -> -- The type of resource to get
    Int ->                            -- The number of the resource to get
    (GPS.GlobalPartitionedState,      -- New resource pools
     Maybe [GPS.GlobalPartitionedStateElt] -- Possible resources obtained
    )
allocateGlobalResource pname state key n =
  let graph = error "Still working on this"
      mnode = D.findNode pname graph
  in case mnode of
      Nothing -> (state, Nothing) -- Nothing was found
      Just node ->
        let procNode = D.nodeFreight node
            rlm   = RL.projRLM (DG.pndResourceLimitMap procNode)
            cm    = RL.projCM (DG.pndConsumptionMap procNode)
            gr    = GPSK.extractGlobalResource key
            mmaxN = FM.lookupFM rlm gr
            mcurN = FM.lookupFM cm gr
        in case mmaxN of
            Nothing -> error ("No resource limit: " ++ show key)
            Just maxN ->
             case mcurN of
              Nothing -> error ("No consumption for: " ++ show key)
              Just curN ->
               if curN + n <= maxN
               then let cm' = FM.addToFM cm gr (curN + n)
                        _procNode' = procNode { DG.pndConsumptionMap =
                                                RL.bundleCM cm'
                                              }
                        _graph' = undefined --G.updateNode procNode' pname graph
                    in (state, error "Still working on this")
               else (state, Nothing) -- Insufficient resources

allocate :: KCA.KernelCoreSegment r
allocate pay =
  do { let kcr = OM.getKernelCoreReq pay
           OM.ResourceRequest pname rsrc _n _special = kcr
     ; UAS.unsafeASIO ("Global timer request: " ++ show kcr)
     ; case rsrc of
        GR.GRMqd ->
          do { error "Now implementing get of Mqd"
             }
        GR.GRTimer ->
          do {

             -- Format the tcb to return
             ; let tcb = OM.TickTimerControlBlock
                           { OM.tcbProcessName = pname
                           , OM.tcbTickCount   = 0
                           }
             -- Get the process domain model
             ; domGraph <- KCACT.getDomainModel
             ; let mprocessNode = D.findNode pname domGraph
             ; UAS.unsafeASIO ("Process Node: " ++ show mprocessNode)
             ; case mprocessNode of
                Nothing -> error ("No process node: " ++ show pname)
                Just processNode ->
                  let mrl = DG.resourceLimitsOf (D.nodeFreight processNode)
                  in case mrl of
                       Nothing ->
                         error "allocateTCB: No resource limits"
                       Just (RL.ResourceLimitMap rl) ->
                         let mtimerLimit = FM.lookupFM rl GR.GRTimer
                            -- This is the wrong check, but
                            -- it will do for now
                         in case mtimerLimit of
                              Nothing -> error "Timer Limit"
                              Just timerLimit ->
                                let tcbrsp =
                                      if timerLimit > 0
                                      then Just (OM.TCBResponse tcb)
                                      else Nothing
                                in KCA.shRsp (OM.ResourceResponse tcbrsp) pname
             }
        _otherwise -> error "Bad timer request.1"
     }
