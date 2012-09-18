-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ProcessBraid
    ( ProcessBraid    -- The braid of actors
    , PS.ProcessState -- The higher state of a process braid
    ) where

-- Braid imports
import qualified BraidExternal as B
-- Actor imports
import qualified ActorState as AS
-- Local imports
import qualified ProcessState as PS

----------------------------------------------------------------------
-- A process braid, having actor state as its local state
----------------------------------------------------------------------
type ProcessBraid ast a =
    B.Braid PS.ProcessState (AS.ActorState ast) PS.ProcRet a
