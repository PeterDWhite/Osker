-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module ProcessGroup
    ( ProcessGroup (..) -- Information stored about a process group
    , mkProcessGroup    -- Constructor for ProcessGroup
    ) where

----------------------------------------------------------------------
-- Data structure describing the process group, a job control concept.
----------------------------------------------------------------------

-- POSIX imports
import qualified ProcessName as PN
import qualified ProcessId as PID

-- ProcessGroup information
data ProcessGroup = -- abbreviation sd
    ProcessGroup
    { pgProcessGroupName :: PN.ProcessGroupName
    , pgProcessGroupId   :: PID.ProcessGroupId
    , pgProcesses        :: [PID.ProcessId]
    }

instance Show ProcessGroup where
    show pg =
      "{" ++ show (pgProcessGroupName pg) ++
      ", " ++ show (pgProcessGroupId pg) ++
      ", " ++ show (pgProcesses pg) ++ "}"

-- Build a brand new process group structure
mkProcessGroup ::
    PN.SessionName ->
    PID.ProcessId ->
    ProcessGroup
mkProcessGroup sessionName pid
  = ProcessGroup
    { pgProcessGroupName = sessionName ++ [show pid]
    , pgProcessGroupId = pid
    , pgProcesses = [pid]
    }
