-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ThreadMap
    ( ThreadMap      -- Mapping thread Id to process name
    , outThreadMap   -- Print out a thread map
    , isSystemHalf   -- Check if tid is for a system half
    , getProcessName -- Get process name for system half tid
    ) where

----------------------------------------------------------------------
-- Data structure describing the kernel threads
-- Note: Only the thread Ids of system half threads are stored
-- in the thread map. Thus, when there is no entry for a tid
-- in the thread map, either the thread does not exist, or it
-- exists but is not a system half thread.
----------------------------------------------------------------------

-- Haskell imports
import Maybe
import qualified OskerConcurrent as C
import qualified FiniteMap as FM
-- Posix imports
import qualified ProcessName as PN

-- The process name allows you to look up the rest of
-- the process information. The incoming thread Id is the
-- thread Id of the system half (i.e. of the IO shell of
-- the system half)
type ThreadMap = FM.FiniteMap C.ThreadId PN.ProcessName

-- A nice print out of the thread map
outThreadMap :: ThreadMap -> String
outThreadMap pm = show (FM.fmToList pm)

-- Check if a thread id is for a system half
isSystemHalf :: ThreadMap -> C.ThreadId -> Bool
isSystemHalf tm tid = isJust ( FM.lookupFM tm tid)

-- Get the process name, don't take no for an answer
getProcessName :: ThreadMap -> C.ThreadId -> PN.ProcessName
getProcessName tm tid = case FM.lookupFM tm tid of
                          Nothing -> error "getProcessName"
                          Just pn -> pn
