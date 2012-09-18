-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module DomainState
    ( DomainState    -- State internal to the domain braid
    , DomainRet (..) -- Return value from a domain braid
    ) where

-- Utility imports
import qualified Null as N
-- Braid imports
import qualified BraidLocal as B
-- Posix imports
import qualified ProcessName as PN
-- Local imports
import qualified ProcessState as PS

----------------------------------------------------------------------
-- Internal state to a domain braid
----------------------------------------------------------------------

-- Structure with local data for a process thread
data DomState
    = SubDomain DomainState        -- When the subbraid represents a subdomain
    | ProcessState PS.ProcessState -- When the subbraid represents a process

instance N.Null DomState where
    N.mkNull = ProcessState N.mkNull

instance Show DomState where
    show (SubDomain ds)    = show ds
    show (ProcessState ps) = show ps

-- State global to the domain braid
-- Variants of the state local to the threads within the domain braid
type DomainState = B.LocalState DomState PN.ProcessName

-- The return value from a domain braid
data DomainRet = DomainRet deriving (Show)

instance N.Null DomainRet where
    N.mkNull = DomainRet
