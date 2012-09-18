-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (C) OHSU, 2001, 2002, 2003
module ProcessId
    ( ProcessId       -- Data type for process Id
    , nullPID         -- The null process Id
    , ProcessGroupId  -- Process group Id, the same as process Id
    , nullPGID        -- The null process group Id
    ) where

----------------------------------------------------------------------
-- This is the POSIX process ID, which is a common definition for
-- both the user prodess and Osker itself. According to Posix, the
-- process group Id type is the same as the process Id type.
----------------------------------------------------------------------

type ProcessId = Int

-- The process Id of zero has special meanings in some system calls
-- (See for example setpgid)
nullPID :: ProcessId
nullPID = 0

-- A process group id is the same as a process Id
type ProcessGroupId = ProcessId

-- The process Id of zero has special meanings in some system calls
nullPGID :: ProcessGroupId
nullPGID = 0
