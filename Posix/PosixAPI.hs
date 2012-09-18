-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module PosixAPI
    ( fork     -- Posix fork interface
    , exit     -- Posix exit interface
    , getPid   -- Posix getPid interface
    , getPPid  -- Posix getPPid interface
    , getPGid  -- Posix getPGid interface
    , getPGrp  -- Posix getPGrp interface
    , setPGid  -- Posix setPGid interface
    , setPGrp  -- Posix getPGrp interface
    , setSid   -- Posix getSid interface
    ) where

----------------------------------------------------------------------
-- The Posix interfaces, as seen by the user half
-- TBD: Eventually, take out the kernel gate parameter by partial
-- evaluation, or by a Posix monad
----------------------------------------------------------------------

-- Posix imports
import qualified SystemCall as SC
import qualified ProcessId as PID

-- The fork interface makes a copy of the process
fork :: SC.U SC.SystemResponse
fork = SC.osker SC.ForkReq

-- The exit () interface
exit :: Int -> SC.U SC.SystemResponse -- IO Action
exit code = SC.osker (SC.ExitReq code)

-- The getPid () interface
getPid :: SC.U SC.SystemResponse -- Return the PID
getPid = SC.osker SC.GetPidReq

-- The getPPid () interface
getPPid :: SC.U SC.SystemResponse -- Return the PID
getPPid = SC.osker SC.GetPPidReq

-- The getPGid () interface
getPGid :: PID.ProcessId -> SC.U SC.SystemResponse
getPGid pid = SC.osker (SC.GetPGidReq pid)

----------------------------------------------------------------------
-- The getPGrp () interface
-- Get the process group id of the calling process.
-- Equivalent to getpgid 0
----------------------------------------------------------------------
getPGrp :: SC.U SC.SystemResponse -- Return the PID
getPGrp = SC.osker (SC.GetPGidReq PID.nullPID)

----------------------------------------------------------------------
-- The setPGid () interface.
--
-- Warning: "Process group" refers to a job control concept. This has
-- nothing to do with the group id used in permissions on files.
--
-- setpgid pid pgid: Set the process group id of the process
-- specified by pid to pgid.
-- When pid == 0, the calling process is specified.
-- When pgid == 0, the process group id is set to the process id
-- specified by pid. This can result in the creation of a new
-- process group.
-- Thus when both are zero, the process group id of the calling
-- process is set to its process id.
--
-- Restrictions:
-- 1. A process can be moved to another process group in the same
--    session, but not to another process group in a different
--    session.
-- 2. When pgrp /= 0: A process can only be moved to a process
--    group that already exists. Specifying a new process group
--    id in this call will result in an error return, not in the
--    creation of a new process group.
--
----------------------------------------------------------------------
setPGid :: PID.ProcessId -> PID.ProcessId -> SC.U SC.SystemResponse
setPGid pid pgrpid = SC.osker (SC.SetPGidReq pid pgrpid)

----------------------------------------------------------------------
-- setPGrp:
-- On most Posix systems, this call is equivalent to setpgid 0 0
----------------------------------------------------------------------
setPGrp :: SC.U SC.SystemResponse -- Return the PID
setPGrp = SC.osker (SC.SetPGidReq PID.nullPID PID.nullPID)

----------------------------------------------------------------------
-- setSid: Set the session id.
-- Create a new session, with a single new process group. The single
-- new process group has only one process member, namely the calling
-- process. The process group id will be equal to the process id of
-- the calling process.
----------------------------------------------------------------------
setSid :: SC.U SC.SystemResponse -- Return the PID / PGID
setSid = SC.osker SC.SetSidReq
