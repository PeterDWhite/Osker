-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module OskerProcess ( OskerProcess ) where

----------------------------------------------------------------------
-- The process structure specialized to 
----------------------------------------------------------------------}

-- Osker imports
import qualified Process as P
import qualified OskerMessage as OM

type OskerProcess = P.Process OM.OskerMsg
