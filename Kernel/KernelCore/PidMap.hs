-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module PidMap
    ( PidMap    -- Process Id Map
    , outPidMap -- Observer on PidMap
    ) where

----------------------------------------------------------------------
-- A map from process id to process name
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID

type PidMap = FM.FiniteMap PID.ProcessId PN.ProcessName

outPidMap :: PidMap -> String
outPidMap pm = show (FM.fmToList pm)
