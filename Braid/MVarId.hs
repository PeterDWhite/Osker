-- Copyright (c) Peter Duncan White, 2003
module MVarId
    ( MVarId          -- Handle for MVar
    , name            -- Name of the MVar
    , tid             -- Tid of the MVar
    , mkMVarId        -- Constructor
    , isSub           -- Check if an MVarId is subordinate to a tid
    , isStrictSub     -- Check if an MVarId is strictly subordinate to a tid
    , atLevel         -- Check if an MVarId is at the level of the tid
    , isHigher        -- Check if an MVarId is at a higher level than the tid
    , mvidTid         -- Compare an MVarId with a tid
    ) where

-- Utility imports
import qualified PartialOrder as PO
-- Braid imports
import qualified ThreadId as TID

----------------------------------------------------------------------
-- Identifier for the MVar, including the path name to the node
-- that holds the actual MVar information.
----------------------------------------------------------------------

-- Local MVarId is a name
data MVarId = MVarId { tid  :: TID.ThreadId
                     , name :: String
                     } deriving (Eq, Ord)

instance Show MVarId where
    show mvid = "MVID(" ++ show (tid mvid) ++ "/" ++ name mvid ++ ")"

-- Constructor
mkMVarId :: TID.ThreadId -> String -> MVarId
mkMVarId t s = MVarId { tid = t, name = s }

-- Check if an MVarId is subordinate to a tid. Subordinate means
-- at the same level or at a lower level.
-- The comparision is satisfied when the threadid is a prefix
-- of the MVarId.
isSub :: TID.ThreadId -> MVarId -> Bool
isSub threadid mvid = TID.isSub threadid (tid mvid)

-- Check if an MVarId is strictly subordinate to a tid.
-- Strictly subordinate means at a lower level, and not at
-- the same level.
isStrictSub :: TID.ThreadId -> MVarId -> Bool
isStrictSub threadid mvid = TID.isStrictSub threadid (tid mvid)

-- Check if an MVarId is at the same level as a braid thread Id.
atLevel :: TID.ThreadId -> MVarId -> Bool
atLevel threadid mvid = TID.atLevel threadid (tid mvid)

-- Check if an MVarId is at a higher level than a tid
-- The comparision is satisfied when the MVarId is a prefix
-- of the threadid.
isHigher :: TID.ThreadId -> MVarId -> Bool
isHigher threadid mvid = tid mvid PO.>= threadid

-- Compare the MVarId with the thread id
mvidTid :: TID.ThreadId -> MVarId -> PO.Comparison
mvidTid threadid mvid = PO.compare threadid (tid mvid)

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkMVarId      #-}
{-# INLINE mvidTid       #-}
{-# INLINE isSub         #-}
{-# INLINE isStrictSub   #-}
{-# INLINE atLevel       #-}
{-# INLINE isHigher      #-}
