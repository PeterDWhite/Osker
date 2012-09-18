-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Domain ( Domain (..) ) where

----------------------------------------------------------------------
-- Data structure describing the domain, a job control concept
----------------------------------------------------------------------

-- Posix imports
import qualified ProcessName as PN
-- Braid imports
import qualified DomainBraid as DB

-- Domain information
data Domain =
    Domain { ddDomainName :: PN.DomainName    -- Domain name
           , ddSessions   :: [PN.SessionName] -- Sessions of domain
           , ddDomains    :: [PN.DomainName]  -- Domains immediately under
           , ddBraidSt    :: DB.DomainSt      -- Braid for the domain
           }

instance Show Domain where
    show dom = "{"  ++ show (ddDomainName dom) ++
               ", " ++ show (ddSessions dom) ++
               ", " ++ show (ddDomains dom) ++ "}"
