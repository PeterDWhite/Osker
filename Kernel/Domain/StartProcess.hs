-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module StartProcess ( startProcess ) where

----------------------------------------------------------------------
-- Data structure describing the process
----------------------------------------------------------------------

-- Braid imports
import qualified DomainBraid as DB
import qualified BraidUtilities as BU
-- Process imports
import qualified Process as P

-- Start a process, by starting each of the three portions in
-- proper order: 1st System half, 2nd shell, 3rd User half.
-- The action is part of the domain braid, thus it is not a liftable
-- action. It is not a "safe" action in that it affects other
-- threads in the braid.
startProcess :: P.Process m -> DB.DomainBraid ()
startProcess process =
  let vars = P.getStartVars process
  in BU.shootManyOneShot vars 0
