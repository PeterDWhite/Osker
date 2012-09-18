-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ProcessState
    ( ProcessState     -- State internal to a process braid
    , processId        -- Get the local process Id
    , processGroupId   -- Get the local process group Id
    , parentPid        -- Get the parent pid
    , processName      -- Get the local process Name
    , ProcRet (..)     -- Return value from a process braid
    ) where

-- Utility imports
import qualified Null as N
-- Braid imports
import qualified BraidLocal as B
-- Posix imports
import qualified ProcessId as PID
import qualified ProcessName as PN
-- Local imports
import qualified ProcessTags as PT

----------------------------------------------------------------------
-- Internal state to a process braid
----------------------------------------------------------------------

-- Structure with local data for a process thread
data ProcState = ProcState
    { tags :: PT.ProcessTags
    }

instance N.Null ProcState where
    N.mkNull = ProcState { tags = N.mkNull }

instance Show ProcState where
    show (ProcState t) = "ProcState " ++ show t

-- The process state
type ProcessState = B.LocalState ProcState PN.ProcessName

-- Get the process Id
processId :: B.Observer ProcState PN.ProcessName PID.ProcessId
processId = B.access (PT.processId . tags)

-- Get the process Group Id
processGroupId :: B.Observer ProcState PN.ProcessName PID.ProcessId
processGroupId = B.access (PT.processGroupId . tags)

-- Get the parent process Id
parentPid :: B.Observer ProcState PN.ProcessName PID.ProcessId
parentPid = B.access (PT.parentPid . tags)

-- Get the process Name
processName :: B.Observer ProcState PN.ProcessName PN.ProcessName
processName = B.access (PT.processName . tags)

-- Return value from a process braid
data ProcRet = ProcRet deriving (Show)
