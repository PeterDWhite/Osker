-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Session ( Session (..) ) where

----------------------------------------------------------------------
-- Data structure describing the session, a job control concept
----------------------------------------------------------------------

-- POSIX imports
import qualified ProcessName as PN
import qualified ProcessId as PID

-- Session information
data Session = -- abbreviation sd
    Session { sdSessionName    :: PN.SessionName
            , sdProcessGroups  :: [PID.ProcessGroupId]
            }

instance Show Session where
    show sess = "{" ++ show (sdSessionName sess) ++ ", " ++
                show (sdProcessGroups sess) ++ "}"
