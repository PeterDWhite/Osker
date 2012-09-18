-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module JobControl
    ( initJobControl          -- Initialize job control data
    , JCS.JCState (..)        -- Job control state passed to initializer
    , JCS.initJobControlData  -- Initialize the job control data
    , JCS.getProcessMap       -- Get process map from job control data
    , JCS.first               -- Get first component of return
    , JCS.second              -- Get second component of return
    ) where

----------------------------------------------------------------------
-- Tables dealing with sessions, process groups, and processes
----------------------------------------------------------------------

-- Haskell imports
import qualified Monad as M
import qualified OskerConcurrent as C
-- Utility imports
import qualified IntGen as IG
import qualified Null as N
-- Braid imports
import qualified BraidUtilities as BU
import qualified BraidExternal as B
import qualified PlatformBraid as PlB
import qualified DomainBraid as DB
-- Posix imports
import qualified ProcessName as PN
-- Graph imports
import qualified Deep as D
-- Domain imports
import qualified DomainGraph as DOMG
-- Osker imports
import qualified Domain as DOM
import qualified Session as SD
import qualified ProcessGroup as PG
import qualified Process as P
import qualified GlobalPartitionedState as GPS
import qualified JobControlData as JCD
import qualified JCState as JCS
import qualified GlobalPartitionedStateKey as GPSK
import qualified LocalPartitionedState as LPS
import qualified PartitionedState as PS
import qualified OskerGlobal as OG
import qualified ProcessControl as PC
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- Initialize the job control information, from the
-- process / domain model.
-- At the top level, the process / domain model should be a single
-- recursive node, which is a domain node.
----------------------------------------------------------------------
initJobControl ::
    JCS.JCState      ->             -- Global state for kernel core
    DOMG.DomainGraph ->             -- The process domain model
    PlB.PlatformBraid ( JCS.JCState, JCS.JCRet DOM.Domain )
initJobControl jcs graph =
  let subNodes       = D.listImmediateNodes graph
      platformNodes  = filter DOMG.isPlatformNode subNodes
      -- The top level should be a single platform node
  in if length subNodes /= 1 ||
        not (DOMG.isPlatformNode (head platformNodes))
     then error "initJobControl: top level is not platform"
     else let platformNode  = head platformNodes
              platformGraph = D.subGraph platformNode
              platformName  = D.nodeName platformNode
              subNodes'     = D.listImmediateNodes platformGraph
              domNodes      = filter DOMG.isDomainNode subNodes'
          in if length subNodes' /= length domNodes
             then error "initJobControl: top level sub nodes should be domain"
             else do { (jcs1, jcret) <- initDomains jcs platformName domNodes
                       -- Install the top level domains
                     ; oskerGlob <-
                         PlB.lift (C.readMVar (JCS.jcsOskerMVar jcs))
                     ; let domains = JCS.third jcret
                           kcTid = BU.tiTid (OG.sgKernelCore oskerGlob)
                           jcs2  = JCS.addDomains jcs1 domains
                           jcs3  = JCS.updateGpsJcd jcs2
                           jcs4  = JCS.updateGps jcs3 (GPS.updateKcTid kcTid)
                     ; return (jcs4, jcret)
                     }

----------------------------------------------------------------------
-- Initialize the domains.
----------------------------------------------------------------------
initDomains ::
    JCS.JCState       ->            -- Input job control maps
    PN.ProcessName    ->            -- Accumulate node names
    [DOMG.DomainNode] ->            -- Domains to initialize
    PlB.PlatformBraid ( JCS.JCState             -- Output global state
                      , JCS.JCRet DOM.Domain    -- New system half start vars
                      )
initDomains jcs pn domNodes =
  PlB.lift
    ( BU.foldStateCombinator
        (\dn jcs' -> (PlB.proj (initDomain pn dn jcs')))
        JCS.append
        jcs
        domNodes
    )

-- Initialize a single domain
initDomain ::
    PN.ProcessName  ->              -- Accumulate node names
    DOMG.DomainNode ->              -- Domain to initialize
    JCS.JCState     ->              -- Input job control state
    PlB.PlatformBraid ( JCS.JCState             -- Output job control state
                      , JCS.JCRet DOM.Domain    -- New system half start vars
                      )
initDomain pn dom jcs =
  let domNodes  = filter DOMG.isDomainNode subNodes
      domName   = D.nodeName dom
      subNodes  = D.listImmediateNodes (D.subGraph dom)
      sessNodes = filter DOMG.isSessionNode subNodes
      extendPn  = pn ++ domName
  in do { M.when (length domNodes + length sessNodes /=
                  length subNodes)
                 (error "initDomain.1")
        ; (jcs1, jcret1) <- initSessions jcs extendPn sessNodes
        ; (jcs2, jcret2) <- initDomains jcs1 extendPn domNodes
        ; let sessions = JCS.third jcret1
              domains  = JCS.third jcret2
              jcs3 = JCS.addSessions jcs2 sessions
              jcs4 = JCS.addDomains jcs3 domains
        ; return ( jcs4,
                   JCS.append2 jcret1 jcret2 (combine (JCS.jcsDomainSt jcs2))
                 )
        }
  where combine :: DB.DomainSt          -> -- Domain braid state
                   JCS.JCRet SD.Session -> -- Return from sessions init
                   JCS.JCRet DOM.Domain -> -- Return from domains init
                   DOM.Domain
        combine domSt jcret1 jcret2 =
          DOM.Domain { DOM.ddDomainName = D.nodeName dom
                     , DOM.ddSessions   =
                         map SD.sdSessionName (JCS.third jcret1)
                     , DOM.ddDomains    =
                         map DOM.ddDomainName (JCS.third jcret2)
                     , DOM.ddBraidSt    = domSt
                     }

----------------------------------------------------------------------
-- Initialize the Sessions.
----------------------------------------------------------------------
initSessions ::
    JCS.JCState        ->           -- Input job control state
    PN.ProcessName     ->           -- Accumulate node names
    [DOMG.SessionNode] ->           -- Sessions to init
    PlB.PlatformBraid ( JCS.JCState             -- Output job control state
                      , JCS.JCRet SD.Session    -- Return value
                      )
initSessions jcs pn sessNodes =
  PlB.lift
    ( BU.foldStateCombinator
        (\sn jcs' -> (PlB.proj (initSession pn sn jcs')))
        JCS.append
        jcs
        sessNodes
    )

initSession ::
    PN.ProcessName     ->           -- Accumulate node names
    DOMG.SessionNode   ->           -- Session to initialize
    JCS.JCState        ->           -- Input job control state
    PlB.PlatformBraid ( JCS.JCState             -- Output job control state
                      , JCS.JCRet SD.Session    -- Return value
                      )
initSession pn sessionNode jcs =
  let subNodes  = D.listImmediateNodes (D.subGraph sessionNode)
      pgNodes   = filter DOMG.isProcessGroupNode subNodes
      extendPn  = pn ++ (D.nodeName sessionNode)
  in do { M.when (length pgNodes /= length subNodes) (error "initSession.1")
        ; ( jcs', jcret ) <- initProcessGroups jcs extendPn pgNodes
        ; let procGroups = JCS.third jcret
              jcs'' = JCS.addProcessGroups jcs' procGroups
              sess  = SD.Session extendPn (map PG.pgProcessGroupId procGroups)
        ; return ( jcs'', JCS.updateThird jcret [sess] )
        }

----------------------------------------------------------------------
-- Initialize the Process groups
----------------------------------------------------------------------
initProcessGroups ::
    JCS.JCState             ->        -- Input job control state
    PN.ProcessName          ->        -- Accumulate node names
    [DOMG.ProcessGroupNode] ->        -- Sessions to init
    PlB.PlatformBraid ( JCS.JCState               -- Output job control state
                      , JCS.JCRet PG.ProcessGroup -- Output process groups
                      )
initProcessGroups jcs pn pgNodes =
  PlB.lift
    ( BU.foldStateCombinator
        (\pgn jcs' -> (PlB.proj (initProcessGroup pn pgn jcs')))
        JCS.append
        jcs
        pgNodes
    )

-- Initialize a process group. The form of this function is to initialize
-- the processes within the process group, and then reformat and
-- reorganize the data into a process group.
initProcessGroup ::
    PN.ProcessName        ->        -- Accumulate node names
    DOMG.ProcessGroupNode ->        -- ProcessGroup to initialize
    JCS.JCState           ->        -- Input job control state
    PlB.PlatformBraid ( JCS.JCState             -- Output job control state
                      , JCS.JCRet PG.ProcessGroup -- Return process group
                      )
initProcessGroup pn pgNode jcs =
  let subNodes     = D.listImmediateNodes (D.subGraph pgNode)
      processNodes = filter DOMG.isProcessNode subNodes
      extendPn     = pn ++ D.nodeName pgNode
  in do { M.when (length processNodes /= length subNodes)
                 (error "initProcGroup.1")
        ; ( jcs1, jcret ) <- initProcesses jcs extendPn processNodes
        ; let processes = JCS.third jcret
              jcs2   = JCS.addProcesses jcs1 processes
              pgroup = PG.ProcessGroup
                         extendPn
                         -- For now, use first process Id for process group id
                         (P.prProcessId (P.prProcessTags (head processes)))
                         (map (P.prProcessId . P.prProcessTags) processes)
        ; M.when (null processes)
                 (error ("Empty process group: " ++ show extendPn))
        ; return ( jcs2, JCS.updateThird jcret [pgroup] )
        }

----------------------------------------------------------------------
-- Initialize the processes from the process / domain model.
-- This means that each process in the process / domain model will
-- be forked and execed. The process information will be added into
-- the relevant maps. All this will be done by formatting a message
-- for each process in the process / domain model, and sending the
-- message to the process control portion of the kernel core,
-- via the outbound message channel (from the kernel core executive)
----------------------------------------------------------------------

initProcesses ::
    JCS.JCState                ->    -- Input job control state
    PN.ProcessName             ->    -- process name except for last comp
    [DOMG.ProcessNode]         ->    -- Processes to initialize
    PlB.PlatformBraid ( JCS.JCState                 -- Output job control state
                      , JCS.JCRet (P.Process OM.OskerRspMsg) -- Out proc info
                      )
initProcesses jcs pn procNodes =
  let -- Get the PID generator out of the global fractured state
      LPS.VanillaValue (GPS.GfsePidGen pidGen) =
        PS.lookHard
          (JCS.jcsGps jcs)
          (GPSK.GPSK (Right GPSK.GPSKPidGen))
          GPSK.freeDelta
  in do { oskerGlob <- PlB.lift ( C.readMVar (JCS.jcsOskerMVar jcs) )
        ; (processes, (domSt', pidGen')) <-
            PlB.lift
              ( BU.foldBraidWithState
                  -- Func of state and procNode
                  (\n s -> (PlB.proj (initProcess oskerGlob n s)))
                  (JCS.jcsDomainSt jcs, pidGen) -- The state that is folded
                  procNodes                     -- The list to fold
              )
        ; let -- Now update the Pid generator with the fresh value.
              jcs2       = JCS.updateGps jcs (GPS.updatePidGen pidGen')
              jcs3       = JCS.updateDs jcs2 domSt'
              sys2starts = map (BU.tiStart . P.prIoShell) processes
              fsstarts   = map (BU.tiStart . P.prFileSysThread) processes
        ; return (jcs3, (JCS.mkJCRet sys2starts fsstarts processes))
        }
     where -- Create the new process, returning informaiton gathered
           -- about the new process
           initProcess ::
               OG.OskerGlobal   ->   -- Osker global table
               DOMG.ProcessNode ->   -- Process node to create
               (DB.DomainSt, IG.IntGen) ->   -- Dom st and Pid generator
               PlB.PlatformBraid ( P.Process OM.OskerRspMsg -- Output proc info
                                 , ( DB.DomainSt              -- Output dom st
                                   , IG.IntGen                -- Ouptut pid gen
                                   )
                                 )
           initProcess oskerGlob procNode (domSt, pidGen) =
             let DOMG.ProcessNode procNodeData = D.nodeFreight procNode
                 (pidGen', newPid) = IG.genNew pidGen
                 thispn = pn ++ D.nodeName procNode
             in do { -- Bust out the kernel core tid from osker globals
                   ; let devMap   = OG.sgDeviceMap oskerGlob
                         processTags =
                           P.ProcessTags
                           { P.prProcessName    = thispn
                           , P.prProcessId      = newPid
                           , P.prProcessGroupId = newPid
                           , P.prParentPid      = newPid
                           }
                   ; (domSt', procinfo) <-
                       PC.addProcessDomain
                         processTags             -- Process id information
                         procNodeData            -- Domain / proc model
                         (JCS.jcsKcChan jcs)     -- Kernel core channel
                         (JCS.jcsFsCoreChan jcs) -- File system core channel
                         devMap                  -- Device info table
                         (JCS.jcsFsRoot jcs)     -- File system root
                         domSt                   -- Input domain state
                   ; return ( procinfo, (domSt', pidGen') )
                   }
