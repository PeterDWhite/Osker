-- Copyright (c) Peter Duncan White, 2003
module BraidMVar
    ( MVar (..)        -- Data type for mutable variables
    , mkMVar           -- Constructor for MVar
    , MV.MVarId        -- Handle for MVar
    , MV.mkMVarId      -- Constructor for MVarId
    , MV.name          -- Get the name of an MVar
    , MV.tid           -- Get the tid of an MVar
    , MV.isSub         -- Check if MVar is subordinate to a tid
    , MV.atLevel       -- Check if MVar is at level of tid
    , MV.mvidTid       -- Compare an MVarId with a tid
    , MVal             -- Local MVar value type
    , MVars            -- Local mapping from MVar id to MVar value
    , lookupMVar       -- Look up a value for an MVar
    , lookupMVarHard   -- Look up MVinfo associated with an mvar id
    , updateMVar       -- Update the value associated with an MVar
    , nullMVar         -- Null the value of an MVar
    , addMVar          -- Add a new MVar to the map
    , deleteMVar       -- Remove an MVar from the map
    , sizeMVars        -- How many MVars are there?
    ) where

-- Haskell imports
import Dynamic (Dynamic, Typeable, TyCon, mkTyCon, typeOf, mkAppTy)
import FiniteMap ( FiniteMap, emptyFM, fmToList, lookupFM, addToFM
                 , delFromFM, sizeFM)
-- Utility imports
import Null
-- Braid imports
import qualified MVarId as MV

----------------------------------------------------------------------
-- Model for the MVars, within a local thread
--
-- These definitions support local MVar operations within a liftable
-- thread. The local MVar operations can be globalized by the braid
-- in which the thread is executed.
----------------------------------------------------------------------

-- The Local MVar data type
data MVar a = MVar { mvarId :: MV.MVarId } deriving (Eq, Ord, Show)

-- Constructor
mkMVar :: MV.MVarId -> MVar a
mkMVar = MVar

-- Make an MVar a dynamic type
mvCon :: TyCon
mvCon = mkTyCon "MVar"
instance Typeable (MVar a) where
    typeOf _ = mkAppTy mvCon []

-- The Local MVar value type
-- When the value is Nothing, it means the MVar does not have a
-- value yet.
type MVal   = Maybe Dynamic

----------------------------------------------------------------------
-- The structure containing mapping from ids to value for MVars at
-- the local level
----------------------------------------------------------------------

data MVars = MVars (FiniteMap MV.MVarId MVal)

instance Null MVars where
    mkNull = MVars emptyFM

instance Show MVars where
    show (MVars mvars) = show (fmToList mvars)

-- Lookup a value from the mvar collection
lookupMVar :: MVars -> MV.MVarId -> Maybe MVal
lookupMVar (MVars mvars) mvid = lookupFM mvars mvid

-- Lookup a value from the mvar collection
lookupMVarHard :: MVars -> MV.MVarId -> MVal
lookupMVarHard mvars mvid =
  case lookupMVar mvars mvid of
    Nothing     -> error ("lookupMVarHard/Local: " ++ show mvid)
    Just mvinfo -> mvinfo

-- Update the value associated with an MVar
updateMVar :: MVars -> MV.MVarId -> MVal -> MVars
updateMVar (MVars mvars) mvid val = MVars (addToFM mvars mvid val)

-- Null the value of an MVar
nullMVar ::MVars -> MV.MVarId -> MVars
nullMVar mvars mvid = updateMVar mvars mvid Nothing

-- Add a new MVar to the mapp
addMVar :: MVars -> MV.MVarId -> MVal -> MVars
addMVar = updateMVar

-- Remove an MVar from the map
deleteMVar :: MVars -> MV.MVarId -> MVars
deleteMVar (MVars mvars) mvid = MVars (delFromFM mvars mvid)

-- Find out how many MVars there are
sizeMVars :: MVars -> Int
sizeMVars (MVars mvars) = sizeFM mvars

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkMVar         #-}
{-# INLINE lookupMVar     #-}
{-# INLINE lookupMVarHard #-}
{-# INLINE updateMVar     #-}
{-# INLINE nullMVar       #-}
{-# INLINE addMVar        #-}
{-# INLINE deleteMVar     #-}
{-# INLINE sizeMVars      #-}
