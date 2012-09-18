-- Copyright (c) Peter Duncan White, 2003
module MVarInfo
    ( MVarInfo (..)    -- Information about MVar in threader state
    , MVars            -- Mapping MVar id to MVar information
    , mvarsStats       -- Statistics about an MVars structure
    , MVarGen          -- Handle to generate MVars
    , outMVarP         -- Print out an MVar
    , clobbers         -- Clobber evidence of a tid from collection of MVars
    , addTaker         -- Make another tid waiting on full
    , addPutter        -- Make another tid waiting on empty
    , lookupMVarHard   -- Look up MVinfo associated with an mvar id
    , lookupMVar       -- L hjgymmmmook up MVinfo associated with an mvar id
    , updateMVar       -- Update the value associated with an MVar
    , updateMVarInfo   -- Update the information associated with an MVar
    , newMVar          -- Make up a new MVar info
    , deleteMVar       -- Remove an MVar from the map
    , addMVar          -- Add a new MVar to the map
    , mvarsToList      -- Convert MVars to list
    , sizeMVars        -- How many MVars are there?
    , hasValue         -- Check if MVar has a value
    , isEmpty          -- Check if MVar is empty
    , takeValPutter    -- Take the value from the mvar, and first putter
    , putValTaker      -- Put the value in, return first taker
    ) where

----------------------------------------------------------------------
-- Information about MVars in the threader state
----------------------------------------------------------------------

-- Haskell imports
import Dynamic (Dynamic)
import FiniteMap (FiniteMap, emptyFM, lookupFM, addToFM, fmToList, sizeFM, mapFM, delFromFM)
import List
import Maybe
-- Utility imports
import Null
-- Osker imports
import qualified BraidMVar as MV
-- Resumption imports
import ThreadId

-- Data for waiting on an MVar to become empty
data WaitEmpty = WaitEmpty {putter :: ThreadId, val :: Dynamic} deriving (Show)

instance Eq WaitEmpty where
    we1 == we2 = putter we1 == putter we2

-- The stuff associated with the mvar in the MVar state.
data MVarInfo =
    MVarInfo
    { waitingEmpty :: [WaitEmpty] -- Threads waiting on empty
    , waitingFull  :: [ThreadId]  -- Threads waiting on full
    , value        :: MV.MVal     -- MVar value, Nothing when empty
    , creator      :: ThreadId    -- Who created the MVar
    } deriving (Show)

type Transerver a = MVarInfo -> (MVarInfo, a)

-- The collection of MVars in the threader state
data MVars = MVars (FiniteMap MV.MVarId MVarInfo)

-- Take a value from the MVar, and also the first putter, if there is one
takeValPutter :: Transerver (Maybe ThreadId)
takeValPutter mvinfo =
  case waitingEmpty mvinfo of
    [] ->     ( mvinfo { value = Nothing }, Nothing )
    (w:ws) -> ( mvinfo { value = Nothing, waitingEmpty = ws }
              , Just ( putter w )
              )

-- Put a value from the MVar, and return the first taker, if there is one
putValTaker :: Dynamic -> Transerver (Maybe ThreadId)
putValTaker dyn mvinfo =
  case waitingFull mvinfo of
    [] ->     ( mvinfo { value = Just dyn }, Nothing )
    (w:ws) -> ( mvinfo { value = Just dyn, waitingFull = ws }, Just w )

instance Null MVars where
    mkNull = MVars emptyFM

-- See if an MVar has a value
hasValue :: Maybe MVarInfo -> Bool
hasValue Nothing       = False
hasValue (Just mvinfo) = isJust ( value mvinfo )

-- See if an MVar is empty
isEmpty :: Maybe MVarInfo -> Bool
isEmpty Nothing       = False
isEmpty (Just mvinfo) = isNothing ( value mvinfo )

-- Lookup a value from the mvar collection
lookupMVarHard :: MVars -> MV.MVarId -> MVarInfo
lookupMVarHard mvars mvid =
  case lookupMVar mvars mvid of
    Nothing     -> error ("lookupMVarHard: " ++ show mvid)
    Just mvinfo -> mvinfo

-- Lookup a value from the mvar collection
lookupMVar :: MVars -> MV.MVarId -> Maybe MVarInfo
lookupMVar (MVars mvars) mvid = lookupFM mvars mvid

-- Update the value associated with an MVar
updateMVar :: MVars -> MV.MVarId -> Maybe Dynamic -> MVars
updateMVar mvars mvid mdyn =
  let mvinfo' = ( lookupMVarHard mvars mvid ) { value = mdyn }
  in updateMVarInfo mvars mvid mvinfo'

-- Add a new MVar to the map
addMVar :: MVars -> MV.MVarId -> MVarInfo -> MVars
addMVar = updateMVarInfo

-- Update the info associated with an MVar
updateMVarInfo :: MVars -> MV.MVarId -> MVarInfo -> MVars
updateMVarInfo (MVars mvmap) mvid mvinfo = MVars (addToFM mvmap mvid mvinfo)

-- Convert MVars to list
mvarsToList :: MVars -> [(MV.MVarId, MVarInfo)]
mvarsToList (MVars mvars) = fmToList mvars

-- Find out how many MVars there are
sizeMVars :: MVars -> Int
sizeMVars (MVars mvars) = sizeFM mvars

-- Get statistics about a single MVar
mvarStats :: MVarInfo -> (Int, Int)
mvarStats mvinfo = ( length (waitingEmpty mvinfo)
                   , length (waitingFull mvinfo)
                   )

-- Combine the statistics of two MVars into a summary statistic
combineMvarStats :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineMvarStats (m1, n1) (m2, n2) = (m1 + m2, n1 + n2)

-- Get stats about a list of mvars with Ids.
mvarsStats :: MVars -> (Int, Int)
mvarsStats (MVars mvars) =
  let lmvars = fmToList mvars
  in foldr combineMvarStats (0, 0) (map (mvarStats . snd) lmvars)

-- A handle to generate new MVars.
type MVarGen = Int

-- Print out an MVarId, MVarInfo pair
outMVarP :: (MV.MVarId, MVarInfo) -> String
outMVarP (mvid, mvinfo) =
  let empties = waitingEmpty mvinfo
      fulls   = waitingFull mvinfo
      mval = value mvinfo
      tidout = if null empties && null fulls
               then "None Waiting"
               else "E=" ++ show empties ++ ",F=" ++ show fulls
      valout = case mval of
                 Nothing  -> "Empty"
                 Just _   -> "Full "
  in "\t MVar= " ++ show mvid ++ " | " ++ valout ++ " |< " ++ tidout ++ " >|\n"

-- Delete entry for a tid from a list of wait empties.
deleteTid :: ThreadId -> [WaitEmpty] -> [WaitEmpty]
deleteTid _tid [] = []
deleteTid tid (we:wes) = if (putter we == tid) then wes else we:deleteTid tid wes

-- Get rid of information about an thread from an MVar.
clobber :: ThreadId -> MVarInfo -> MVarInfo
clobber tid mvinfo =
    mvinfo { waitingEmpty = deleteTid tid (waitingEmpty mvinfo)
           , waitingFull  = delete tid (waitingFull mvinfo)
           }

-- Clobber all information about a tid from a collection of MVars
clobbers :: ThreadId -> MVars -> MVars
clobbers tid (MVars mvars) =
  MVars (mapFM (\_mvid mvar -> clobber tid mvar) mvars)

-- Make up a new MVar info
newMVar :: ThreadId -> Maybe Dynamic -> MVarInfo
newMVar tid mdyn = MVarInfo { waitingEmpty = []
                            , waitingFull  = []
                            , value        = mdyn
                            , creator      = tid
                            }

-- Remove an MVar from the map
deleteMVar :: MVars -> MV.MVarId -> MVars
deleteMVar (MVars mvars) mvid = MVars (delFromFM mvars mvid)

-- Make a new tid waiting on full
addTaker :: ThreadId -> MV.MVarId -> MVars -> MVars
addTaker tid mvid mvars =
  let mvinfo  = lookupMVarHard mvars mvid
      mvinfo' = mvinfo { waitingFull = nub ((waitingFull mvinfo) ++ [tid])
                       , value       = Nothing
                       }
  in updateMVarInfo mvars mvid mvinfo'

-- Make a new tid waiting on empty
addPutter :: ThreadId -> MV.MVarId -> Dynamic -> MVars -> MVars
addPutter tid mvid dyn mvars =
  let mvinfo  = lookupMVarHard mvars mvid
      waitE   = WaitEmpty { putter = tid, val = dyn }
      mvinfo' = mvinfo { waitingEmpty = nub (waitingEmpty mvinfo ++ [waitE] ) }
  in updateMVarInfo mvars mvid mvinfo'

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE takeValPutter    #-}
{-# INLINE putValTaker      #-}
{-# INLINE hasValue         #-}
{-# INLINE isEmpty          #-}
{-# INLINE lookupMVarHard   #-}
{-# INLINE lookupMVar       #-}
{-# INLINE updateMVar       #-}
{-# INLINE addMVar          #-}
{-# INLINE updateMVarInfo   #-}
{-# INLINE mvarsToList      #-}
{-# INLINE sizeMVars        #-}
{-# INLINE mvarStats        #-}
{-# INLINE combineMvarStats #-}
{-# INLINE clobber          #-}
{-# INLINE clobbers         #-}
{-# INLINE newMVar          #-}
{-# INLINE deleteMVar       #-}
{-# INLINE addTaker         #-}
