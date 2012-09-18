-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ProcessMap
    ( ProcessMap     -- Mapping process name to Process structure
    , outProcessMap  -- Print out a process map
    , getSys2Chan    -- Get system half channel associated with proc name
    , getProcNode    -- Get process node data for a system half
    , addProcess     -- Add a process to the process map
    , addProcesses   -- Add a list of processes to the process map
    , addDeferred    -- Based on process control payload, add a process
    ) where

----------------------------------------------------------------------
-- A map from process name to process data
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Domain imports
import qualified DomainGraph as DOMG
-- Posix imports
import qualified ProcessName as PN
import qualified OskerMessage as OM
-- Osker imports
import qualified Process as P

-- The process map, maps the process name to dynamic information
-- about the process. The Pid map maps the process Id to the
-- process name, so that this information can be looked up based
-- on the PID as well.
type ProcessMap = FM.FiniteMap PN.ProcessName OM.OskerProcess

outProcessMap :: ProcessMap -> String
outProcessMap pm = show (FM.fmToList pm)

-- Add a process to the process map
addProcess :: ProcessMap -> OM.OskerProcess -> ProcessMap
addProcess pm proc =
  FM.addToFM pm (P.prProcessName (P.prProcessTags proc)) proc

-- Add a bunch of processes to the process map;
addProcesses :: ProcessMap -> [OM.OskerProcess] -> ProcessMap
addProcesses pm ps = foldl addProcess pm ps

-- Get the system half channel associated with a process name
getSys2Chan :: ProcessMap -> PN.ProcessName -> OM.OskerChannel
getSys2Chan pm pn =
  let mproc = FM.lookupFM pm pn
  in case mproc of
       Nothing   -> error ("getSys2Chan: " ++ PN.outProcessName pn)
       Just proc -> OM.OskerChannel (P.tiChan (P.prIoShell proc))

-- Get the process / domain node for a system half
getProcNode :: ProcessMap -> PN.ProcessName -> DOMG.ProcessNodeData
getProcNode pm pn =
  let mproc = FM.lookupFM pm pn
  in case mproc of
       Nothing   -> error ("getProcNode: " ++ PN.outProcessName pn)
       Just proc -> P.prProcessNode proc

-- Based on a deferred action, possibly add a process to the process map.
addDeferred :: OM.OskerPay -> ProcessMap -> ProcessMap
addDeferred pay pm =
  case pay of
    OM.FromProcessControl pcr ->
      case pcr of
        OM.UpdateProcessPCR { OM.forkedProcess  = forked
                            , OM.forkingProcess = _forking
                            } -> addProcess pm forked
        _otherwise -> pm
    _otherwise -> pm
