-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module PlatformBraid
    ( PlatformBraid    -- The braid of actors
    , PS.PlatformState -- Global state to a platform braid
    ) where

-- Braid imports
import qualified BraidExternal as B
-- Domain imports
import qualified DomainState as DS
-- Local imports
import qualified PlatformState as PS

----------------------------------------------------------------------
-- A domain braid, braiding together threads that represent processes.
----------------------------------------------------------------------
type PlatformBraid a = B.Braid PS.PlatformState DS.DomainState PS.PlatformRet a
