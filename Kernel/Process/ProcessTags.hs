-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ProcessTags ( ProcessTags (..) ) where

----------------------------------------------------------------------
-- Data structure describing the various tags of the process,
-- includind process Id, process Name, parent process, ...
----------------------------------------------------------------------

-- Utility imports
import qualified Null as N
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID

-- Various Ids about a process
data ProcessTags = ProcessTags { processName    :: PN.ProcessName
                               , processId      :: PID.ProcessId
                               , processGroupId :: PID.ProcessGroupId
                               , parentPid      :: PID.ProcessId
                               }

instance Show ProcessTags where
    show pt = "{ProcessTags: " ++ PN.outProcessName (processName pt) ++
              "," ++ show (processId pt) ++
              "," ++ show (processGroupId pt) ++
              "," ++ show (parentPid pt) ++ "}"

instance N.Null ProcessTags where
    N.mkNull = ProcessTags { processName    = N.mkNull
                           , processId      = N.mkNull
                           , processGroupId = N.mkNull
                           , parentPid      = N.mkNull
                           }
