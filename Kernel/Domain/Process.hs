-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Process
    ( Process (..)        -- Process information, visible to the process
    , PT.ProcessTags (..) -- Identifying information about a process
    , targetChan          -- Get the input channel of the IO shell
    , getStartVars        -- Get the start up oneshots for the process
    , BU.ForkInfo (..)    -- Information stored about a thread
    , getProcessId        -- Get process Id from a process
    , getProcessName      -- Get process Name from a process
    , getThreadId         -- Get thread Id of IO shell of a process
    ) where

----------------------------------------------------------------------
-- Data structure describing the process
----------------------------------------------------------------------

-- Haskell imports
import qualified OskerChan as OC
-- Domain imports
import qualified DomainGraph as DOMG
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID
-- Braid imports
import qualified BraidUtilities as BU
-- Process imports
import qualified ProcessTags as PT

-- Process information
data Process m =
    Process
    { -- Thread information about the shell of the system half
      -- tid is specialized to process name
      prIoShell        :: BU.ForkInfo m PN.ProcessName
      -- Thread information about the file system thread for the process
    , prFileSysThread  :: BU.ForkInfo m PN.ProcessName
      -- The information from the process / domain model for the process
    , prProcessNode    :: DOMG.ProcessNodeData
      -- The identification information about the process
    , prProcessTags    :: PT.ProcessTags
    }

-- Get the process Id from a process
getProcessId :: Process m -> PID.ProcessId
getProcessId = PT.processId . prProcessTags

-- Get the process name from a process
getProcessName :: Process m -> PN.ProcessName
getProcessName = PT.processName . prProcessTags

-- Get the thread Id (of the io shell) of the process
-- Thread Id is specialized now to process name
getThreadId :: Process m -> PN.ProcessName
getThreadId = BU.tiTid . prIoShell

instance Show (Process m) where
    show proc = show (prProcessTags proc) ++ ", " ++ show (prProcessNode proc)

-- Get the channel of the io shell
targetChan :: Process m -> OC.Chan m
targetChan = BU.tiChan . prIoShell

-- Get each of the start variables from the process information.
-- Get them in the correct order to start them:
--    First Shell
--    Second user half
--    Third, file system thread for process
getStartVars ::
    Process m ->  -- Process to analyze
    [BU.OneShot]  -- Resulting list of start variables
getStartVars process =
    [ BU.tiStart (prIoShell process)
    , BU.tiStart (prFileSysThread process)
    ]
