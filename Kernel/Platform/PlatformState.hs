-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module PlatformState
    ( PlatformState    -- State internal to the platform braid
    , PlatformRet (..) -- Return value from a platform braid
    ) where

-- Utility imports
import qualified Null as N
-- Braid imports
import qualified BraidLocal as L
-- Posix imports
import qualified ProcessName as PN
-- Local imports
import qualified DomainState as DS
import qualified DeviceMap as DM

----------------------------------------------------------------------
-- Internal state to a platform braid
----------------------------------------------------------------------

-- Structure with data local to a platform state
data PlatState = PlatState { devMap :: DM.DeviceMap }

instance N.Null PlatState where
    N.mkNull = PlatState { devMap = N.mkNull }

-- The platform state, tid is specialized to process name
type PlatformState = L.LocalState DS.DomainState PN.ProcessName

-- Return value from a platform braid
data PlatformRet = PlatformRet deriving (Show)

instance N.Null PlatformRet where
    N.mkNull = PlatformRet
