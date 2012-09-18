-- Copyright (c) Peter Duncan White, 2003
module MVarProc
    ( -- Operators in support of MVarxfs
    , newMVar               -- Create a new MVar
    , deleteMVar            -- Remove MVar from state
    , updateMVar            -- Update value associated with an MVar
    , dereferenceMVar       -- Get value of MVar from threader state
    , unliftedTakeMVar      -- takeMVar for unlifted threads
    , unliftedPutMVar       -- putMVar for unlifted threads
    , processRsps           -- Process a single MVar response
    , processLiftedReqs     -- Process lifted requests
    , processNonLocalReqs   -- Process non lifted requests
    , MV.MVar               -- Abstract type for MVars
      -- Operators in support of MVarOps
    , nullUpwardRelay       -- Null out the upward relay
    , locale                -- A destructor of braid state
    , setLocale             -- Set the whole locale
    , nullLocale            -- Null the locale in a braid state
    , takeUpwardReqs        -- Take out upward requests, null them
    , isUpwardReq           -- Check if MVar Ops are null
    , takeLocalReqs         -- Get requests that are now lifted
    , takeLocalRsps         -- Get responses that are now lifted
      -- Operators in support of delay
    , takeDelay             -- Get delay value from lifted state
    , globalizeDelay        -- Delay value from a lifted thread
      -- In support of localize / globalize
    , localize              -- Move MVar ops from braid to lifted thread
    , globalize             -- Move MVar ops from lifted thread to braid
    ) where

-- Haskell imports
import List
import Maybe
import Dynamic (Dynamic, Typeable, fromDyn)
-- Utility imports
import qualified PartialOrder as PO
import Null
-- import Unsafe
-- Local imports
import ThreadId ( ThreadId, level )
import BraidState
import qualified LiftedState as LS
import qualified LiftedThread as LT
import qualified ThreadInfo as TI
import qualified MVarInfo as MVI
import qualified BraidMVar as MV
import qualified Locale as L

----------------------------------------------------------------------
--
-- Primitives in support of MVar operations by lifted threads
--
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Create a new MVar, for a lifted or an unlifted thread
----------------------------------------------------------------------

-- Currently, MVars are always created locally
-- Since there is only one difference between the versions for lifted
-- and unlifted threads, we combine both into one function. The update
-- of the lifted state is an extraneous (and harmless) operation for
-- unlifted threads
newMVar :: (Show gs, Show ls) =>
    ThreadId      -> -- Thread id of the creator
    String        -> -- Name of the MVar
    Maybe Dynamic -> -- New value for the mvar
    Transerver t gs ls (MV.MVar a)  -- Output state and the new MVar
newMVar tid mvname dyn bst =
  let mvname' = mvname ++ "." ++ show (mvarGen bst)
      mvid   = MV.mkMVarId tid mvname'
  in case (lookupMVar mvid bst) of
       Nothing ->
         let mvinfo = MVI.newMVar (current bst) dyn
             mvars' = MVI.addMVar (mvars bst) mvid mvinfo
             bst'   = transformLifted tid (LS.made mvid) bst
          in ( bst' {mvarGen = mvarGen bst + 1, mvars = mvars'}, MV.MVar mvid )
       Just mvinfo -> error ( "newMVar: " ++ mvname' ++
                              "\n\tMVID:   " ++ show mvid ++
                              "\n\tMVINFO: " ++ show mvinfo )

-- For a lifted MVar request, do not return the MVar, it is internal to
-- the response
liftedNew :: (Show gs, Show ls) =>
    ThreadId         -> -- Thread id of the creator
    String           -> -- Name of the MVar
    Maybe Dynamic    -> -- New value for the mvar
    Transformer t gs ls -- Output state and the new MVar
liftedNew tid nom mdyn = fst . newMVar tid nom mdyn

----------------------------------------------------------------------
-- take an MVar, for a lifted thread
----------------------------------------------------------------------

-- Convert a braid state transformer to a transerver that returns
-- Nothing on the side
withNothing :: Transformer t gs ls -> Transerver t gs ls (Maybe a)
withNothing f = \bst -> ( f bst, Nothing )

-- Update the information for an MVar
updateMVarInfo :: MV.MVarId -> MVI.MVarInfo -> Transformer t gs ls
updateMVarInfo mvid mvinfo bst@(BraidState { mvars = mv }) =
  bst { mvars = MVI.updateMVarInfo mv mvid mvinfo }

-- Take the value from an MVar, leaving it empty, and return the
-- first putter of the MVar, if there are any
-- This function is only called when the required MVar exists
takeValPutter :: (Show gs, Show ls) =>
    MV.MVarId          -> -- MVar to get
    MVI.MVarInfo       -> -- Information about the MVar
    Transerver t gs ls (Maybe ThreadId)
takeValPutter mvid mvinfo bst =
  let (mvinfo', (mtid)) = MVI.takeValPutter mvinfo
      bst' = updateMVarInfo mvid mvinfo' bst
  in ( bst', mtid )
--  in unsafeReturn ( "MV.takeValPutter" ) ( bst', mtid )

-- Make another tid waiting on an MVar to become full
addTaker :: ThreadId -> MV.MVarId -> Transformer t gs ls
addTaker tid mvid bst = bst { mvars = MVI.addTaker tid mvid (mvars bst) }

-- Respond to a take request
respondTake :: L.Req -> MVI.MVarInfo -> L.Rsp
respondTake req mvinfo =
  L.TakeRsp { L.rspId  = L.reqId req
            , L.rspVal = (fromJust (MVI.value mvinfo))
            , L.rspTid = L.reqTid req
            }

-- A data type for case analysis
type LiftedCaseAnalysis t gs ls a =
    Int                -> -- For debugging
    PO.Comparison      -> -- Compare braid level to MVar
    PO.Comparison      -> -- Compare braid level to taker / putter of MVar
    Bool               -> -- If the MVar is available for the taking
    ThreadId           -> -- Of the taker
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    LT.LiftedSt ls     -> -- The state of the lifted thread
    Transerver t gs ls ( Maybe a ) -- Gen a xform on the braid state

-- A data type for MVar request processing from any source
type LiftedProcessing t gs ls =
    Int                -> -- For debugging
    L.Req              -> -- The MVar request
    LT.LiftedSt ls     -> -- The lifted state of the taker
    BraidState t gs ls -> -- The state of the enclosing braid
    BraidState t gs ls    -- The resulting state of the enclosing braid

-- A data type for checking if this is a valid case
type LiftedCaseValidity =
    PO.Comparison      -> -- Compare braid level to MVar
    PO.Comparison      -> -- Compare braid level to taker / putter of MVar
    Bool               -> -- If the MVar is available for the taking
    ThreadId           -> -- Of the taker
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    Bool                  -- Is this case valid?

-- A type for checking a condition on an MVar
type MVarCond = Maybe MVI.MVarInfo -> Bool

-- A type for getting a thread id from an MVar request
type GetTid = L.Req -> ThreadId

-- Convert a comparison of a taker / putter to a direction
toDir :: PO.Comparison -> L.Direction
toDir PO.EQ = L.Local
toDir PO.LT = L.Down
toDir PO.GT = L.Up
toDir PO.NC = L.Up

-- Use the factors for a lifted takeMVar to determine a lifted state
-- transformer and an braid state transerver.
liftedTakeCases :: (Show gs, Show ls) => LiftedCaseAnalysis t gs ls ThreadId
--             MVar  Taker Avail Local
liftedTakeCases _n PO.EQ PO.EQ True taker mvid mmvinfo _req ls bst =
--  unsafeRet ( "liftedTakeCases.1: " ++ show mvid )
    ( let ls' = LS.taken (MVI.value (fromJust mmvinfo)) ls
      in ( takeValPutter mvid (fromJust mmvinfo) .
           updateLiftedState taker ls' ) bst
    )
liftedTakeCases _n PO.EQ btaker True _taker mvid mmvinfo req _ls bst =
--  unsafeRet ( "liftedTakeCases.2: " ++ show mvid        ++
--              "\n\tbtaker  = "      ++ show btaker      ++
--              "\n\ttaker   = "      ++ show taker       ++
--              "\n\tmmvinfo = "      ++ show mmvinfo     ++
--              "\n\treq     = "      ++ show req )
    ( let (bst', mtid) = takeValPutter mvid (fromJust mmvinfo) bst
          bst'' = addRsp (toDir btaker) (respondTake req (fromJust mmvinfo)) bst'
          tidls = LS.put (L.reqId req) -- This is an ls transformer
      in case mtid of 
           Nothing  -> (bst'', mtid)
           Just tid -> (transformLiftedState tid tidls bst'', mtid)
    )
liftedTakeCases _n PO.EQ _     False taker  mvid _mmvinfo _req _ls bst =
--  unsafeRet ( "liftedTakeCases.3: " ++ show mvid )
    ( let stat = TI.WaitingCuzEmpty mvid
      in withNothing ( addTaker taker mvid . updateTidStateHard taker stat )
    ) bst
-- error for the rest of the cases, since they should have been filtered
-- out by liftedTakeValidity
liftedTakeCases n bmv btaker avail taker mvid mmvinfo req ls bst =
  error ( "MVP.liftedTakeCases: " ++ show n ++ "," ++ show bmv ++
          "," ++ show btaker ++ "," ++ show avail ++ "," ++ show taker ++
          "," ++ show mvid ++ "," ++ show mmvinfo ++ "," ++ show req  ++
          "," ++ show ls ++ "," ++ show bst )

-- Check if a valid lifted case, for take or put MVar operations
liftedValidity :: LiftedCaseValidity
liftedValidity bmv _btaker _avail _taker _mvid mmvinfo _req =
  bmv == PO.EQ && isJust mmvinfo

-- Handle a takeMVar for a lifted (lifted) thread at the same level
-- as the enclosing braid. This level condition is checked during the
-- call to liftedTakeCases, and an error is generated if it fails.
-- This core function is also used by the lifted putMVar
coreMVar :: (Show ls, Show gs) =>
    MVarCond                            -> -- Check condition on MVar info
    GetTid                              -> -- How to get tid from request
    LiftedCaseAnalysis t gs ls ThreadId -> -- Case analyzer for lifted takeMVar
    LiftedCaseValidity                  -> -- Check if a valid case
    LiftedProcessing t gs ls               -- Processing for the request
coreMVar cond gettid zeta check n req ls bst =
  let mvid    = L.reqId req
      mmvinfo = lookupMVar mvid bst
      bmv     = level (local bst) (MV.tid mvid)
      tid     = gettid req -- ThreadId of the taker or putter
      btaker  = level (local bst) tid
      avail   = -- unsafeRet ( " >>> coreMVar: " ++ show bst )
                          ( cond mmvinfo )
  in if check bmv btaker avail tid mvid mmvinfo req
     then
       let -- Use the factors to get lifted and unlifted state transformers
           (bst', mtid) = zeta n bmv btaker avail tid mvid mmvinfo req ls bst
       in case mtid of
            Nothing   -> bst'
            Just ntid -> -- unsafeRet ( "%%% coreMVar" ++ show bst' )
                         ( complete (L.reqVal req) ntid bst' )
     else error ( "coreMVar: " ++ show bmv ++ "\n\t" ++ show mvid ++
                  "\n\t" ++ show req ++ "\n\t" ++ show mmvinfo )

liftedTakeMVar :: (Show gs, Show ls) => LiftedProcessing t gs ls
liftedTakeMVar =
--  unsafeRet ( ">>> liftedTakeMVar" )
    ( coreMVar MVI.hasValue L.reqTid liftedTakeCases liftedValidity )

----------------------------------------------------------------------
-- put an MVar, for a lifted (lifted) thread
----------------------------------------------------------------------
-- Respond to a take request
respondPut :: L.Req -> L.Rsp
respondPut req = L.PutRsp { L.rspId  = L.reqId req
                          , L.rspTid = L.reqTid req
                          }

-- Take the value from an MVar, leaving it empty, and return the
-- first taker of the MVar, if there are any
-- This function is only called when the required MVar exists
putValTaker :: ( Show ls, Show gs ) =>
    MV.MVarId          -> -- MVar to put
    MVI.MVarInfo       -> -- Information about the MVar
    Dynamic            -> -- Value to put
    Transerver t gs ls (Maybe ThreadId)
putValTaker mvid mvinfo val bst =
  let (mvinfo', mtid) = MVI.putValTaker val mvinfo
      bst' = updateMVarInfo mvid mvinfo' bst
  in ( bst', mtid )
-- unsafeReturn ( "MV.putValTaker: mtid = " ++ show mtid ++
--                     "\n\tmvinfo  = " ++ show mvinfo ++
--                     "\n\tmvinfo' = " ++ show mvinfo' ++ "\n" )

-- Make another tid waiting on an MVar to become full
addPutter :: ThreadId -> MV.MVarId -> Dynamic -> Transformer t gs ls
addPutter tid mvid dyn bst =
  bst { mvars = MVI.addPutter tid mvid dyn (mvars bst) }

-- Use the factors for a lifted putMVar to determine a lifted state
-- transformer and an braid state transerver.
liftedPutCases :: (Show ls, Show gs) => LiftedCaseAnalysis t gs ls ThreadId
--            MVar  Putr Avail Local
liftedPutCases _n PO.EQ PO.EQ True putter mvid mmvinfo req ls bst =
--  unsafeRet ( "liftedPutCases.1: " ++ show req ++
--              "\n\tputter = " ++ show putter ++
--              "\n\tmvid = " ++ show mvid ++
--              "\n\tmmvinfo = " ++ show mmvinfo )
    ( let ls' = LS.put (L.reqId req) ls -- This is an ls transformer
      in ( putValTaker mvid (fromJust mmvinfo) (L.reqVal req) .
           updateLiftedState putter ls'
         ) bst
    )

liftedPutCases _n PO.EQ bputter True _putter mvid mmvinfo req _ls bst =
--  unsafeRet ( "liftedPutCases.2: " ++ show req ++
--              "\n\tbputter = " ++ show bputter ++
--              "\n\tputter = " ++ show putter ++
--              "\n\tmvid = " ++ show mvid ++
--              "\n\tmmvinfo = " ++ show mmvinfo )
    ( let (bst', mtid) =
             putValTaker mvid (fromJust mmvinfo) (L.reqVal req) bst
          bst'' = addRsp (toDir bputter) (respondPut req) bst'
          tidls = LS.taken (Just (L.reqVal req)) -- **** liftedTakeCases
      in case mtid of
           Nothing  -> (bst'', mtid)
           Just tid ->
--             unsafeReturn ( "liftedPutCases.2a" )
              ( transformLiftedState tid tidls bst''
              , mtid
              )
    )

-- liftedPutCases _n PO.EQ _     False putter  mvid mmvinfo req =
--   unsafeRet ( "liftedPutCases.3: " ++ show req ++
--               "\n\tputter = " ++ show putter ++
--               "\n\tmvid = " ++ show mvid ++
--               "\n\tmmvinfo = " ++ show mmvinfo )
liftedPutCases _n PO.EQ _     False putter  mvid _mmvinfo req _ls bst =
   ( withNothing ( addPutter putter mvid (L.reqVal req) .
                   updateTidStateHard putter (TI.WaitingCuzFull mvid) ) ) bst
-- error for the rest of the cases, since they should have been filtered
-- out by liftedPutValidity
liftedPutCases _n bmv bputter avail putter mvid mmvinfo req ls bst =
  error ( "MVP.liftedPutCases: " ++ show bmv ++ "," ++ show bputter ++
          "," ++ show avail ++ "," ++ show putter ++
          "," ++ show mvid ++ "," ++ show mmvinfo ++ "," ++ show req ++
          "," ++ show ls ++ "," ++ show bst )

liftedPutMVar :: (Show ls, Show gs) => LiftedProcessing t gs ls
liftedPutMVar = coreMVar MVI.isEmpty L.reqTid liftedPutCases liftedValidity

----------------------------------------------------------------------
--
-- Primitives in support of MVar operations by non-local threads
-- i.e. by threads (lifted or unlifted) from another layer, on
-- MVars at the level of the current braid.
--
----------------------------------------------------------------------

-- A data type for case analysis
type NonLocalCaseAnalysis t gs ls a =
    PO.Comparison      -> -- Compare braid level to taker / putter of MVar
    Bool               -> -- If the MVar is available for the taking
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    Transerver t gs ls (Maybe ThreadId)  -- Gen a xform on the braid state

-- A data type for MVar request processing from any source
type NonLocalProcessing t gs ls =
    L.Req              -> -- The MVar request
    BraidState t gs ls -> -- The state of the enclosing braid
    BraidState t gs ls    -- The resulting state of the enclosing braid

-- A data type for checking if this is a valid case
type NonLocalCaseValidity =
    PO.Comparison      -> -- Compare braid level to taker / putter of MVar
    Bool               -> -- If the MVar is available for the taking
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    Bool                  -- Is this case valid?

-- Use the factors for a non-lifted takeMVar to determine
-- a braid state transerver.
nonLocalTakeCases :: (Show gs, Show ls) =>
    NonLocalCaseAnalysis t gs ls ThreadId
nonLocalTakeCases btaker True mvid  mmvinfo req =
  takeValPutter mvid (fromJust mmvinfo) .
  addRsp (toDir btaker) (respondTake req (fromJust mmvinfo))
nonLocalTakeCases _btaker False mvid _mvinfo req =
  withNothing ( addTaker (L.reqMVarTid req) mvid )

-- Check if a valid lifted case, for take or put MVar operations
nonLocalValidity :: NonLocalCaseValidity
nonLocalValidity _btaker _avail _mvid mmvinfo _req = isJust mmvinfo

-- Handle a takeMVar for a lifted (lifted) thread at the same level
-- as the enclosing braid. This level condition is checked during the
-- call to liftedTakeCases, and an error is generated if it fails.
-- This core function is also used by the lifted putMVar
nonLocalCoreMVar :: (Show ls, Show gs) =>
    MVarCond                               -> -- Check condition on MVar info
    GetTid                                 -> -- How to get tid from request
    NonLocalCaseAnalysis t gs ls ThreadId -> -- Case analyz for nonlifted take
    NonLocalCaseValidity                  -> -- Check if a valid case
    NonLocalProcessing t gs ls               -- Processing for the request
nonLocalCoreMVar cond gettid zeta check req bst =
  let mvid    = L.reqId req
      mmvinfo = lookupMVar mvid bst
      tid     = gettid req -- ThreadId of the mvar request
      btaker  = level (local bst) tid
      avail   = cond mmvinfo
  in if check btaker avail mvid mmvinfo req
     then
       let -- Use the factors to get lifted and unlifted state transformers
          g   = zeta btaker avail mvid mmvinfo req
          -- Apply unlifted transerver, see if there is tid to make active
          (bst',  mtid) = g bst
       in case mtid of
            Nothing   -> bst'
            Just ntid -> -- unsafeRet ( "%%% nonLocalCoreMVar/addPaused" )
                                   ( addPaused TI.Paused ntid bst' )
     else error ( "nonLocalcoreMVar" )

nonLocalTakeMVar :: (Show gs, Show ls) => NonLocalProcessing t gs ls
nonLocalTakeMVar =
  nonLocalCoreMVar MVI.hasValue L.reqTid nonLocalTakeCases nonLocalValidity

----------------------------------------------------------------------
-- Non lifted putMVar
----------------------------------------------------------------------

-- Use the factors for a non-lifted takeMVar to determine
-- a braid state transerver.
nonLocalPutCases :: (Show gs, Show ls) => NonLocalCaseAnalysis t gs ls ThreadId
nonLocalPutCases bputter True mvid  mmvinfo req =
  putValTaker mvid (fromJust mmvinfo) (L.reqVal req) .
  addRsp (toDir bputter) (respondPut req)
nonLocalPutCases _bputter False mvid _mvinfo req =
  withNothing ( addPutter (L.reqMVarTid req) mvid (L.reqVal req) )

nonLocalPutMVar :: (Show ls, Show gs) => NonLocalProcessing t gs ls
nonLocalPutMVar =
  nonLocalCoreMVar MVI.hasValue L.reqTid nonLocalPutCases nonLocalValidity

----------------------------------------------------------------------
--
-- Primitives in support of MVar operations by unlifted (unlifted)
-- threads on MVars at the same level as the braid.
--
----------------------------------------------------------------------

type UnliftedCaseAnalysis t gs ls =
    PO.Comparison      -> -- Compare braid level to MVar
    PO.Comparison      -> -- Compare braid level to taker of MVar
    Bool               -> -- If the MVar is available for the taking
    ThreadId           -> -- Of the taker
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    ( Transerver t gs ls ( Maybe ThreadId ) )

type UnliftedProcessing t gs ls =
    L.Req         ->  -- The takeMVar request
    Transformer t gs ls -- Returns new state, value (when appropriate)

type UnliftedCaseValidity =
    PO.Comparison      -> -- Compare braid level to MVar
    PO.Comparison      -> -- Compare braid level to taker of MVar
    Bool               -> -- If the MVar is available for the taking
    ThreadId           -> -- Of the taker
    MV.MVarId          -> -- MVar to get
    Maybe MVI.MVarInfo -> -- Information about the MVar
    L.Req              -> -- The mvar request to process
    Bool                  -- Is this case valid?

unliftedTakeCases :: (Show gs, Show ls) => UnliftedCaseAnalysis t gs ls
unliftedTakeCases PO.EQ PO.EQ True taker mvid mmvinfo _req bst =
--  unsafeRet ( "unliftedTakeCases.1: taker: " ++ show taker ++
--              "\n\tReq: " ++ show req ++ "\n\tMVid: " ++ show mvid ++
--              "\n\tMMvinfo: " ++ show mmvinfo )
-- unliftedTakeCases PO.EQ PO.EQ True _taker mvid mmvinfo _req =
    ( let mvinfo = fromJust mmvinfo
          (bst', mtid) = takeValPutter mvid mvinfo bst
          stat = TI.Completed (fromJust (MVI.value mvinfo))
      in ( updateTidStateHard taker stat bst', mtid ) )
-- unliftedTakeCases PO.EQ btaker True taker mvid mmvinfo req =
--   unsafeRet ( "unliftedTakeCases.2: taker: " ++ show taker ++
--               "\n\tReq: " ++ show req ++ "\n\tMVid: " ++ show mvid ++
--               "\n\tMMvinfo: " ++ show mmvinfo ++
--               "\n\tbtaker: " ++ show btaker)
unliftedTakeCases PO.EQ btaker True taker mvid mmvinfo req bst =
    ( let mvinfo = fromJust mmvinfo
          (bst', mtid) = takeValPutter mvid mvinfo bst
          stat = TI.Completed (fromJust (MVI.value mvinfo))
          bst'' = updateTidStateHard taker stat bst'
      in ( addRsp (toDir btaker) (respondTake req (fromJust mmvinfo)) bst''
         , mtid
         ) )
-- unliftedTakeCases PO.EQ _     False  taker mvid mmvinfo req =
--   unsafeRet ( "unliftedTakeCases.3: taker: " ++ show taker ++
--               "\n\tReq: " ++ show req ++ "\n\tMVid: " ++ show mvid ++
--               "\n\tMMvinfo: " ++ show mmvinfo )
unliftedTakeCases PO.EQ _     False  taker mvid _mmvinfo _req bst =
   ( let stat = TI.WaitingCuzEmpty mvid
      in ( ( addTaker taker mvid ( ( updateTidStateHard taker stat ) bst ) )
         , Nothing ) )
-- error for the rest of the cases, since they should have been filtered
-- out by unliftedTakeValidity
unliftedTakeCases bmv btaker avail taker mvid mmvinfo req _bst =
  error ( "MVP.unliftedTakeCases: " ++ show bmv ++ "," ++ show btaker ++
          "," ++ show avail ++ "," ++ show taker ++
          "," ++ show mvid ++ "," ++ show mmvinfo ++ "," ++ show req )

unliftedValidity :: UnliftedCaseValidity
unliftedValidity _bmv _btaker _avail _tid _mvid mmvinfo _req = isJust mmvinfo

-- Handle a takeMVar for a lifted (lifted) thread at the same level
-- as the enclosing braid. This level condition is checked during the
-- call to unliftedValidity, and an error is generated if it fails.
coreUnliftedMVar :: (Show gs, Show ls) => --
    MVarCond                     -> -- Conditions on MVars
    GetTid                       -> -- How to get tid from MVar request
    UnliftedCaseValidity         -> -- Check validity of case
    UnliftedCaseAnalysis t gs ls -> -- Analysis of cases
    UnliftedProcessing t gs ls      -- Processing performed on the MVar req
coreUnliftedMVar cond gettid check zeta req bst =
  let mvid    = L.reqId req
      mmvinfo = lookupMVar mvid bst
      bmv     = level (local bst) (MV.tid mvid)
      tid     = gettid req -- ThreadId of the MVar request
      btaker  = level (local bst) tid
      mvcond  = cond mmvinfo
  in if check bmv btaker mvcond tid mvid mmvinfo req
     then
       let -- Use the factors to get lifted and unlifted state transformers
           g = zeta bmv btaker mvcond tid mvid mmvinfo req
           -- Apply unlifted transerver, see if there
           -- is tid to make active
           (bst', mtid) = g bst
       in case mtid of
            Nothing   -> bst'
            Just ntid -> -- unsafeRet ( "%%% coreUnliftedMVar/addPaused" )
                                   ( addPaused TI.Paused ntid bst' )
     else error ( "coreUnliftedMVar" )

unliftedTakeMVar :: (Show ls, Show gs) => UnliftedProcessing t gs ls
unliftedTakeMVar =
  coreUnliftedMVar MVI.hasValue L.reqTid unliftedValidity unliftedTakeCases

----------------------------------------------------------------------
-- put an MVar, for a unlifted (unlifted) thread at the same level as
-- the braid
----------------------------------------------------------------------
unliftedPutCases :: (Show gs, Show ls) => UnliftedCaseAnalysis t gs ls
unliftedPutCases PO.EQ PO.EQ True _putter mvid mmvinfo req =
  putValTaker mvid (fromJust mmvinfo) (L.reqVal req)
unliftedPutCases PO.EQ bputter True _putter mvid mmvinfo req =
  ( putValTaker mvid (fromJust mmvinfo) (L.reqVal req) .
    addRsp (toDir bputter) ( respondTake req (fromJust mmvinfo) ) )
unliftedPutCases PO.EQ _     False putter mvid _mmvinfo req =
   withNothing ( addPutter putter mvid (L.reqVal req) .
                 updateTidStateHard putter (TI.WaitingCuzFull mvid) )
-- error for the rest of the cases, since they should have
-- been filtered out by unliftedTakeValidity
unliftedPutCases bmv btaker empty putter mvid mmvinfo req =
  error ( "MVP.unliftedPutCases: " ++ show bmv ++ "," ++ show btaker ++
          "," ++ show empty ++ "," ++ show putter ++
          "," ++ show mvid ++ "," ++ show mmvinfo ++ "," ++ show req )

-- Handle a putMVar for a lifted (lifted) thread at the same
-- level as the enclosing braid. This level condition is
-- checked during the call to unliftedValidity, and an error
-- is generated if it fails.
unliftedPutMVar :: (Show gs, Show ls) => UnliftedProcessing t gs ls
unliftedPutMVar =
  coreUnliftedMVar MVI.isEmpty L.reqTid unliftedValidity unliftedPutCases

--------------------------------------------------------------------
-- Other support functions
--------------------------------------------------------------------

-- Delete an MVar
deleteMVar :: MV.MVar a -> Transformer t gs ls
deleteMVar (MV.MVar mvid) bst = bst { mvars = MVI.deleteMVar (mvars bst) mvid }

-- Update the value associated with an MVar
updateMVar :: MV.MVarId -> Maybe Dynamic -> Transformer t gs ls
updateMVar mvid val bst@(BraidState { mvars = mv }) =
  bst { mvars = MVI.updateMVar mv mvid val }

-- Get the value of an MVar, if it is set, otherwise return Nothing
dereferenceMVar :: (Typeable a) =>
    MV.MVar a          -> -- MVar to dereference
    BraidState t gs ls -> -- Braid state in which to evaluate MVars
    Maybe a               -- Value, if found
dereferenceMVar (MV.MVar mvid) bst =
  fmap (\dyn -> fromDyn dyn ((error "dereferenceMVar.dyn") :: a))
       (MVI.value (lookupMVarHard mvid bst))

--------------------------------------------------------------------
-- MVarOp manipulations at the braid state level
--------------------------------------------------------------------

-- Set the whole locale in the braid state
setLocale :: L.Locale -> Transformer t gs ls
setLocale loc bst = bst { locale = loc }

-- Null the locale
nullLocale :: Transformer t gs ls
nullLocale bst = setLocale mkNull bst

-- Null the upward bound relay
nullUpwardRelay :: Transformer t gs ls
nullUpwardRelay bst = setLocale (L.nullUpwardRelay (locale bst)) bst

-- Add a response in the specified direction
addRsp :: L.Direction -> L.Rsp -> Transformer t gs ls
addRsp dir rsp bst =
  bst { locale = L.addRsp dir rsp (locale bst) }

-- Take the upward requests, and null them in the lifted state
takeUpwardReqs :: (Show gs, Show ls) =>
    ThreadId -> Transerver t gs ls [ L.Req ]
takeUpwardReqs tid bst =
  let ls          = getLiftedState tid bst
      (ls', reqs) = LS.takeUpwardReqs ls
  in ( updateLiftedState tid ls' bst, reqs )

-- Check if MVarsOps is null
isUpwardReq :: Observer t gs ls Bool
isUpwardReq = L.isUpwardReq . locale

--------------------------------------------------------------------
--  In support of localization and globalization
--------------------------------------------------------------------
localize :: (Show ls, Show gs) =>
    ThreadId          -> -- Id of thread to localize
    LT.LiftedSt ls    -> -- Incoming value of lifted state
    Transerver t gs ls (LT.LiftedSt ls) -- Transforms the braid state,
                                        -- returns a local state
localize tid ls0 bst0 =
  let (down, up)    = L.localize tid (LT.locale ls0) (locale bst0)
      (down', mrsp) = L.getFirstLocalRsp down
      ls1  = LS.setLocale down' ls0
      ls2  = case mrsp of
               Nothing  -> ls1
               Just rsp -> -- unsafeRet ( "... MVP.localize.1: " ++ show tid ++
--                                        "\n\t" ++ show rsp )
                           ( LS.localize rsp ls1 )
      bst1 = updateLiftedState tid ls2 ( bst0 { locale = up } )
      res = ( bst1, ls2 )
  in res
--     if L.hasDownward (locale bst0)
--     then unsafeRet ( "... MVP.localize.2: " ++ show tid ++
--                      "\n\tbst0: " ++ show (locale bst0) ++
--                      "\n\tbst1: " ++ show (locale bst1) ++
--                      "\n\tup:   " ++ show up ++
--                      "\n\tls0:  " ++ show (LT.locale ls0) ++
--                      "\n\tls2:  " ++ show (LT.locale ls2))
--                    res

-- Raise MVar ops from lifted state to braid state
-- Also store the continuation program
globalize :: (Show ls, Show gs) => --
    Int                 -> -- For debugging
    ThreadId            -> -- ThreadId of lifted thread
    LT.LiftedSt ls      -> -- Incomding lifted state
    LT.Program ls       -> -- New program state
    Transerver t gs ls ( Maybe Int      -- Delay request
                       , [L.Req]        -- Requests to become lifted
                       , LT.LiftedSt ls -- New value of lifted state
                       )
globalize _n tid ls prog bst@(BraidState {locale = loc, local = addr})=
  let (down, up, reqs) = L.globalize addr (LS.locale ls) loc
      (ls', mn)        = LS.globalize down ls -- Sets the locale and gets delay
  in ( updateLifted tid ls' prog ( bst { locale = up } )
     , ( mn, reqs, ls' )
     )
--     if L.hasUpward (LT.locale ls)
--     then unsafeRet ( "MVP.globalize / not null: " ++ show tid ++
--                      "\n\tls:   " ++ show (LS.locale ls) ++
--                      "\n\tup:   " ++ show up ++
--                      "\n\treqs: " ++ show reqs ++
--                      "\n\tbst:  " ++ show loc )

--------------------------------------------------------------------
--
-- Process MVar responses
--
--------------------------------------------------------------------

-- Process a single MVar response
processRsp :: (Show gs, Show ls) => L.Rsp -> Transformer t gs ls
processRsp (L.TakeRsp {L.rspId=rid,L.rspVal=val,L.rspTid=tid}) bst =
  let mtinfo = lookupThreadInfo tid bst
  in case mtinfo of
       Nothing    -> bst -- Maybe the thread died while waiting for MVar op
       Just tinfo ->
        if TI.getState tinfo == TI.WaitingCuzFull rid
        then case TI.getProgram tinfo of
               TI.UnliftedProgram _ -> error ( "processRsp.1: " )
               TI.LiftedProgram _   -> error ( "processRsp.2: " )
               TI.WaitProgram w     ->
                   ( updateTid tid TI.Paused (TI.UnliftedProgram (w val)) bst )
         else error ( "processRsp.3: " ++ show (TI.getState tinfo) )
processRsp ( L.PutRsp { L.rspId = rid, L.rspTid = tid } ) bst =
  let mtinfo = lookupThreadInfo tid bst
  in case mtinfo of
       Nothing    -> bst -- Maybe the thread died while waiting for MVar op
       Just tinfo ->
        if TI.getState tinfo == TI.WaitingCuzEmpty rid
        then case TI.getProgram tinfo of
               gp@(TI.UnliftedProgram _) -> updateTid tid TI.Paused gp bst
               lp@(TI.LiftedProgram _)   -> updateTid tid TI.Paused lp bst
               TI.WaitProgram _          -> error ( "processRsp.4: " )
        else error ( "processRsp.5: " ++ show (TI.getState tinfo) )
processRsp mvop _bst = error ( "processRsp.6 / newrsp " ++ show mvop )

-- Process many MVar responses
processRsps :: (Show gs, Show ls) => [L.Rsp] -> Transformer t gs ls
processRsps rsps bst = foldl (flip processRsp) bst rsps

-- Process a single MVar request for requester at the level of the braid
processLiftedReq :: (Show ls, Show gs) => --
    Int -> ThreadId -> LT.LiftedSt ls -> LS.Req -> Transformer t gs ls
processLiftedReq n tid ls req bst =
  case req of
    LS.NewEmptyReq { LS.reqId = mvid } ->
       liftedNew tid (LS.name mvid) Nothing bst
    LS.NewReq { LS.reqId = r, LS.reqVal = v } ->
       liftedNew tid (LS.name r) (Just v) bst
    LS.PutReq  { LS.reqTid = _ }    -> liftedPutMVar n req ls bst
    LS.TakeReq { LS.reqTid = _ }    -> liftedTakeMVar n req ls bst
    LS.ThrowReq { LS.reqExp = e }   -> fst (throw e bst)
    LS.ThrowToReq { LS.reqTid = _ } -> throwTo req bst

-- Process many MVar requests for requester at the level of the braid
processLiftedReqs :: (Show ls, Show gs) => --
    Int               -> -- For debugging
    ThreadId          -> -- Tid of currently executing (pausing) thread
    LT.LiftedSt ls    -> -- Lifted state to modify
    [L.Req]           -> -- Requests to process
    Transformer t gs ls  -- New braid state
processLiftedReqs n tid ls rs bst =
  foldl (flip (processLiftedReq n tid ls)) bst rs

-- Process many MVar requests for requester at a different level from
-- the level of the braid
processNonLocalReqs :: (Show gs, Show ls) => [L.Req] -> Transformer t gs ls
processNonLocalReqs reqs bst = foldl (flip processNonLocalReq) bst reqs

-- Process a single MVar request for requester at a different level from
-- the level of the braid
processNonLocalReq :: (Show gs, Show ls) => L.Req -> Transformer t gs ls
processNonLocalReq req bst =
  case req of
    L.NewEmptyReq { LS.reqTid = _ } -> error ( "processNonLocalReq.1" )
    L.NewReq  { LS.reqTid = _ }     -> error ( "processNonLocalReq.2" )
    L.PutReq  { LS.reqTid = _ }     -> nonLocalPutMVar req bst
    L.TakeReq { LS.reqTid = _ }     -> nonLocalTakeMVar req bst
    LS.ThrowReq { LS.reqExp = e }   -> fst (throw e bst)
    LS.ThrowToReq { LS.reqTid = _ } -> throwTo req bst

-- Take the lifted requests from the braid state
takeLocalReqs :: Transerver t gs ls [L.Req]
takeLocalReqs bst = let (loc', reqs) = L.takeLocalReqs (locale bst)
                    in (setLocale loc' bst, reqs)

-- Take the lifted responses from the braid state
takeLocalRsps :: Transerver t gs ls [L.Rsp]
takeLocalRsps bst = let (loc', rsps) = L.takeLocalRsps (locale bst)
                    in (setLocale loc' bst, rsps)

----------------------------------------------------------------------
--  In support of delays
----------------------------------------------------------------------
-- Take the delay count, and null it in the lifted state
takeDelay :: (Show gs, Show ls) =>
    ThreadId -> Transerver t gs ls ( Maybe Int )
takeDelay tid bst =
  let ls            = getLiftedState tid bst
      (ls', mdelay) = LS.takeDelay ls
  in ( updateLiftedState tid ls' bst, mdelay )

-- Globalize a thread delay operation
globalizeDelay ::
    Int              -> -- Current elapsed time
    ThreadId         -> -- Tid of currently executing (pausing) thread
    Maybe Int        -> -- Incoming delay count
    Transformer t gs ls -- Transform braid state
globalizeDelay elapsed tid mcount bst =
  case mcount of
    Nothing    -> bst
    Just count -> ( delay tid (elapsed + ( count `div` 10000)) bst )

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE newMVar                 #-}
{-# INLINE liftedNew               #-}
{-# INLINE withNothing             #-}
{-# INLINE updateMVarInfo          #-}
{-# INLINE takeValPutter           #-}
{-# INLINE addTaker                #-}
{-# INLINE respondTake             #-}
{-# INLINE deleteMVar              #-}
{-# INLINE liftedTakeCases         #-}
{-# INLINE liftedValidity          #-}
{-# INLINE liftedTakeMVar          #-}
{-# INLINE respondPut              #-}
{-# INLINE putValTaker             #-}
{-# INLINE liftedPutCases          #-}
{-# INLINE liftedPutMVar           #-}
{-# INLINE unliftedTakeCases       #-}
{-# INLINE unliftedValidity        #-}
{-# INLINE unliftedTakeMVar        #-}
{-# INLINE unliftedPutCases        #-}
{-# INLINE unliftedPutMVar         #-}
{-# INLINE updateMVar              #-}
{-# INLINE dereferenceMVar         #-}
{-# INLINE setLocale               #-}
{-# INLINE nullLocale              #-}
{-# INLINE nullUpwardRelay         #-}
{-# INLINE isUpwardReq             #-}
{-# INLINE takeUpwardReqs          #-}
{-# INLINE takeDelay               #-}
{-# INLINE localize                #-}
{-# INLINE globalize               #-}
{-# INLINE addPutter               #-}
{-# INLINE processRsps             #-}
{-# INLINE processNonLocalReq      #-}
{-# INLINE processNonLocalReqs     #-}
{-# INLINE takeLocalReqs           #-}
{-# INLINE takeLocalRsps           #-}
{-# INLINE processLiftedReq        #-}
{-# INLINE processLiftedReqs       #-}
{-# INLINE globalizeDelay          #-}
{-# INLINE addRsp                  #-}
