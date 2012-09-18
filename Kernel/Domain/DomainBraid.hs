-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module DomainBraid
    ( DomainBraid      -- The braid of actors
    , DS.DomainState   -- Higher state of a domain braid
    ) where

-- Braid imports
import qualified BraidExternal as B
-- Posix imports
import qualified ProcessName as PN
-- Process imports
import qualified ProcessState as PS
-- Local imports
import qualified DomainState as DS

----------------------------------------------------------------------
-- A domain braid, braiding together threads that represent processes.
-- Thread id is specialized to process name
----------------------------------------------------------------------
type DomainBraid a =
    B.Braid PN.ProcessName DS.DomainState PS.ProcessState DS.DomainRet a
