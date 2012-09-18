-- Copyright (c) Peter Duncan White, 2003
module BraidState
    ( -- Braid state including MVars
    , BraidState (..)          -- The braid state internal to the braid
    , Transformer              -- Braid state transformer
    , Observer                 -- An observer of the braid state
    , Transerver               -- Combined transformer and observer
    , initBraidState           -- Initialize the braid state
    , nThreads                 -- Number of threads in the braid
    , addThread                -- Add a thread to the braid state
    , addThreadWithTid         -- Add a thread with tid already selected
    , delThread                -- Delete a thread from the braid state
    , genLifted                -- Genarate one new local thread Id
    , genGlobal                -- Genarate one new global thread Id
      -- Supporting MVar operations
    , lookupMVar               -- Lookup info associated with MVar
    , lookupMVarHard           -- Lookup info associated with MVar
      -- Operations on a single thread within the braid
    , getLiftedState           -- Get local state from braid state
    , getState                 -- Get thread state of lifted or unlifted thread
    , getProgram               -- Get the thread program for a tid
    , updateProg               -- Update local thread program for a tid
    , lookupThreadInfo         -- Get information pertaining to a tid
    , lookupThreadInfoHard     -- Get information pertaining to a tid
    , updateThread             -- Update thread program and local state
    , updateLiftedState        -- Update a local state (for a tid)
    , transformLiftedState     -- Transform a local state (for a tid)
    , updateLifted             -- Update local state and program
    , transformLifted          -- Transform the local state
    , updateTid                -- Update program and thread state
    , updateTidState           -- Update thread state for a tdi
    , updateTidStateHard       -- Update thread state for a tid, (not paused)
    , clobber                  -- Initialize the entry for a given tid
    , MV.MVar                  -- Abstract type for MVars
      -- Operators in support of exception handling
    , addCurrentCatcher        -- Update catcher for running thread
    , throwTo                  -- Throw asynchronous exception (non locally)
    , throw                    -- Throw synchronous exception (locally)
{-P:
      -- Operators used in separation properties
    , TidState                 -- State info legitimately affected by a tid
    , getTidState              -- Get state info legitimately affected by tid
 -}
      -- Operators in support of scheduling and weaving
    , Scheduler                -- Type of scheduling algorithms
    , getGlobalRun             -- Get a run from a tid and a braid state
    , getLiftedRun             -- Get a run from a tid and a braid state
    , delayRequested           -- Check if a delay has been requested
    , scheduleAndUpdate        -- Select first paused thread to run
    , pause                    -- Pause the current thread
    , addPaused                -- Add a tid to the paused list
    , complete                 -- Complete an operation
    , bumpThreadCount          -- Increment the thread count
      -- Re-exports
    , TS.ThreadInit (..)       -- Data required for thread initialization
    , TI.ProgramType (..)      -- Is thread local or global
    , TI.ThreadState (..)      -- Internal state of a thread
    , TI.Catcher               -- Type of an exception catcher
    , TI.Program (..)          -- Global or local program
    , TI.getUnliftedProg       -- Get global program out of program
    , TI.mkUnliftedProg        -- Constructor for a global program
    , TI.mkLiftedProg          -- Constructor for a local program
      -- Debugging operators
    , localeNull               -- Determine if the locale is null
      -- Internal implementation of delay
    , delay                    -- Delay a thread
    , getExpired               -- Get the expired delay threads
    , requestDelay             -- Request a delay for the current thread
    ) where

-- Haskell imports
import List
import Maybe
import Dynamic (Dynamic)
-- Utility imports
import Null
-- import Unsafe
-- Resumption imports
import qualified Ex as E
-- Local imports
import qualified ThreadId as TID
import qualified LiftedState as LS
import qualified Threads as TS
import qualified ThreadInfo as TI
import qualified MVarInfo as MVI
import qualified BraidMVar as MV
import qualified Locale as L
import qualified LiftedState as LS
import qualified LiftedThread as LT
import qualified Delay as D

----------------------------------------------------------------------
-- This braid has a collection of threads, with no ability
-- to create new threads (fork), but this braid supports MVars,
-- and the communication between threads supported by MVars.
-- The type of the overall braid is a parameter to the braid stata
-- type, so that the braid state can contain threads of type Braid ()
-- 
-- The braid also has a global (braid wide) and lifted (thread local)
-- state as type parameters
----------------------------------------------------------------------

-- A braid state transformer.
-- Many of the functions in this file, after partial evaluation, are
-- braid state transformers.
type Transformer t gs ls =
    BraidState t gs ls -> BraidState t gs ls

-- An observer of the braid state
type Observer t gs ls a  = BraidState t gs ls -> a

-- A combined transformer and observer
type Transerver t gs ls a =
    BraidState t gs ls -> (BraidState t gs ls, a)

-- The mapping from lifted MVars to global MVars
data BraidState t gs ls =
    BraidState
    { -- Items in support of threads
      threads      :: TS.Threads t ls  -- The threads, and their states
    , current      :: TID.ThreadId     -- The currently running thread
    , paused       :: TID.ThreadSet    -- Tids currently in paused state
      -- Items in support of thread delay
    , delayQ       :: D.Delay          -- Delay queue
    , ticker       :: Int              -- Current tick count
    , delayReq     :: Int              -- Requesting a delay?
      -- Items in support of MVars, from Braid1 state
    , mvars        :: MVI.MVars        -- The current collection of MVars
    , mvarGen      :: MVI.MVarGen      -- For generating new MVar Ids
      -- Items in support of forking, new to Braid state
    , forkedLifted :: TID.ThreadId     -- Most recently forked lifted thread
    , forkedGlobal :: TID.ThreadId     -- Most recently forked global thread
      -- Global state
    , global       :: gs               -- Braid wide state
      -- Items in support of the layered braid
    , locale       :: L.Locale         -- MVar ops for globalize / localize
      -- The local thread id of this braid
    , local        :: TID.ThreadId     -- Local thread id
      -- In support of debugging
    , threadCount  :: Int              -- Count of threads scheduled
    }

-- Determine if there are no threads in the braid yet
-- noThreads :: Observer t gs ls Bool
-- noThreads bst = current bst == mkNull

instance (Null gs) => Null (BraidState t gs ls) where
    mkNull = BraidState { threads       = mkNull
                        , current       = mkNull
                        , paused        = mkNull
                        , delayQ        = mkNull
                        , delayReq      = mkNull
                        , ticker        = mkNull
                        , mvars         = mkNull
                        , mvarGen       = mkNull
                        , forkedLifted  = mkNull
                        , forkedGlobal  = mkNull
                        , global        = mkNull
                        , locale        = mkNull
                        , local         = mkNull
                        , threadCount   = mkNull
                        }

-- Initialize the braid state with a bunch of threads
-- All of the threads are added in the paused state
initBraidState ::
    gs                    -> -- Initial value of the global state
    TID.ThreadId          -> -- Local thread id
    Int                   -> -- Starting value of ticker
    BraidState t gs ls
initBraidState gs tid start  =
  BraidState { threads       = mkNull
             , current       = mkNull
             , paused        = mkNull
             , delayQ        = mkNull
             , delayReq      = mkNull
             , ticker        = start
             , mvars         = mkNull
             , mvarGen       = mkNull
             , forkedLifted  = mkNull
             , forkedGlobal  = mkNull
             , global        = gs
             , locale        = mkNull
             , local         = tid
             , threadCount   = mkNull
             }

-- Initialize the information about a given thread.
-- The null value of the thread info sets the thread state to "Paused"
-- This function observes the invariant that when a thread is in
-- the paused state, it should be in the paused list.
clobber :: (Null t, Null ls) =>
    TID.ThreadId        -> -- The thread Id to clobber
    Transformer t gs ls    -- Transformation of the braid state
clobber tid bst =
 bst { threads = TS.update (threads bst) tid mkNull
     , paused  = TID.addThreadSet (paused bst) tid
     , mvars   = MVI.clobbers tid (mvars bst)
       -- No change to mvargen
     , current = if tid == current bst
                 then mkNull
                 else current bst
     }

----------------------------------------------------------------------
--  Accessors
----------------------------------------------------------------------

-- Check if an MVarId is subortinate to a braid state
isSub :: TID.ThreadId -> Observer t gs ls Bool
isSub tid bst = TID.isSub (current bst) tid

-- Lookup the lifted state
getLiftedState :: (Show gs, Show ls) =>
    TID.ThreadId -> BraidState t gs ls -> LT.LiftedSt ls
getLiftedState tid = TI.getLiftedState . lookupThreadInfoHard tid

-- Get the state of a thread (unlifted or lifted)
getState :: (Show gs, Show ls) =>
    TID.ThreadId -> BraidState t gs ls  -> TI.ThreadState
getState tid = TI.getState . lookupThreadInfoHard tid

-- Lookup the lifted program for a tid
getProgram :: (Show gs, Show ls) =>
    TID.ThreadId -> Observer t gs ls (TI.Program t ls)
getProgram tid = TI.getProgram . lookupThreadInfoHard tid

-- Get a run of the program
getLiftedRun :: (Show gs, Show ls) =>
    TID.ThreadId -> Observer t gs ls (LT.LiftedSt ls, LT.Program ls)
getLiftedRun tid bst =
  let tinfo = lookupThreadInfoHard tid bst
  in case TI.progType tinfo of
       TI.Lifted  -> ( TI.getLiftedState tinfo
                     , TI.getLiftedProg (TI.getProgram tinfo)
                     )
       TI.Unlifted -> error "getLiftedRun.1"
       TI.Wait     -> error "getLiftedRun.2"

getGlobalRun :: (Show gs, Show ls) => TID.ThreadId -> Transerver t gs ls t
getGlobalRun tid bst =
  let tinfo = lookupThreadInfoHard tid bst
  in case TI.progType tinfo of
       TI.Lifted   -> error "getGlobalRun.1"
       TI.Unlifted -> ( bst,  TI.getUnliftedProg (TI.getProgram tinfo) )
       TI.Wait     -> error "getGlobalRun.2"

-- Get number of threads in the braid
nThreads :: BraidState t gs ls -> Int
nThreads = TS.size . threads

----------------------------------------------------------------------
--  Updaters (constructors)
----------------------------------------------------------------------

-- Bump the threads scheduled
bumpThreadCount :: Transformer t gs ls
bumpThreadCount bst = bst { threadCount = threadCount bst + 1 }

-- Update thread program, no change to the thread state
updateProg :: (Show gs, Show ls) =>
    TID.ThreadId -> TI.Program t ls -> Transformer t gs ls
updateProg tid prog = transformThread tid (TI.updateProgram prog)

-- Update the state for a lifted thread
updateLiftedState :: (Show gs, Show ls) =>
    TID.ThreadId -> LT.LiftedSt ls -> Transformer t gs ls
updateLiftedState tid ls = transformThread tid (TI.updateLiftedState ls)

-- Perform a transformer for a thread info for a specified thread id
transformThread :: (Show ls, Show gs) =>
    TID.ThreadId -> TI.Transformer t ls -> Transformer t gs ls
transformThread tid f bst =
  updateThreadInfo tid (f (lookupThreadInfoHard tid bst)) bst

-- Perform a transerver for a thread info for a specified thread id
transerveThread :: (Show gs, Show ls) =>
    TID.ThreadId -> TI.Transerver t ls a -> Transerver t gs ls a
transerveThread tid f bst =
  let (tinfo, a) = f (lookupThreadInfoHard tid bst)
  in (updateThreadInfo tid tinfo bst, a)

-- Transfomer the state for a lifted thread
transformLiftedState :: (Show gs, Show ls) =>
    TID.ThreadId -> LT.Transformer ls -> Transformer t gs ls
transformLiftedState tid f = transformThread tid (TI.transformLifted f)

-- Update the lifted state and the lifted program
updateLifted :: (Show gs, Show ls) =>
    TID.ThreadId -> LT.LiftedSt ls -> LT.Thread ls () -> Transformer t gs ls
updateLifted tid ls prog = transformThread tid (TI.updateLifted ls prog)

-- Update the thread state and the program
updateTid :: (Show gs, Show ls) =>
    TID.ThreadId -> TI.ThreadState -> TI.Program t ls -> Transformer t gs ls
updateTid tid s prog = transformThread tid (TI.updateTid s prog)

-- Transform the lifted state
transformLifted :: (Show gs, Show ls) =>
    TID.ThreadId -> LT.Transformer ls -> Transformer t gs ls
transformLifted tid f = transformThread tid (TI.transformLifted f)

-- Look up info associated with MVar id.
lookupMVarHard :: MV.MVarId -> Observer t gs ls MVI.MVarInfo
lookupMVarHard mvid bst = MVI.lookupMVarHard (mvars bst) mvid

-- Look up info associated with MVar id
lookupMVar :: MV.MVarId -> Observer t gs ls (Maybe MVI.MVarInfo)
lookupMVar mvid bst = MVI.lookupMVar (mvars bst) mvid

-- Lookup thread information for a tid
lookupThreadInfo ::
    TID.ThreadId -> BraidState t gs ls -> Maybe (TI.ThreadInfo t ls)
lookupThreadInfo tid bst = TS.lookup (threads bst) tid

-- Lookup thread information for a tid
lookupThreadInfoHard :: (Show gs, Show ls) =>
    TID.ThreadId -> BraidState t gs ls -> TI.ThreadInfo t ls
lookupThreadInfoHard tid bst =
  case lookupThreadInfo tid bst of
    Nothing    -> error ( "lookupThreadInfoHard: " ++ show tid ++ show bst )
    Just tinfo -> tinfo
-- TS.lookupHard (threads bst) tid

-- Update thread information for a tid
-- The thread is added to the paused list when the thread is in the
-- Paused state, thus the invariant that paused threads are in the
-- paused list is observed.
-- The thread state is not changed
updateThreadInfo ::
    TID.ThreadId       -> -- Tid of thread to update
    TI.ThreadInfo t ls -> -- Thread information structure
    Transformer t gs ls   -- This is a state transformer
updateThreadInfo tid tinfo bst@(BraidState {threads = ts}) =
  let bst' = bst { threads = TS.update ts tid tinfo }
  in -- unsafeRet ( "%%% updateThreadInfo: " ++ show tid )
               ( addPaused (TI.getState tinfo) tid bst' )

-- Update thread program and lifted state for a Tid
updateThread :: TID.ThreadId -> TS.ThreadInit t ls -> Transformer t gs ls
updateThread tid tinit bst =
  updateThreadInfo tid (TS.initThreadInfo tid tinit) bst

-- Update the tid state, when it is known that the new state is not "paused"
updateTidStateHard :: TID.ThreadId -> TI.ThreadState -> Transformer t gs ls
updateTidStateHard _tid TI.Paused _bst = error ( "updateTidStateHard" )
updateTidStateHard tid stat bst@(BraidState { threads = ts }) =
--   unsafeRet ( "%%% updateTidStateHard: " ++ show tid )
            ( bst { threads = TS.updateThreadState ts tid stat } )

-- Update the state of a tid, including the paused list, to maintain
-- the invariant that if a thread is in the paused state, it is
-- also found in the paused list
updateTidState ::
    TID.ThreadId         -> -- Id of thread to update
    TI.ThreadState       -> -- New value of running thread
    Transformer t gs ls     -- A state transformer
updateTidState tid stat bst@(BraidState { threads = ts }) =
  let bst' = bst { threads = TS.updateThreadState ts tid stat }
  in  -- unsafeRet ( "%%% updateTidState" )
                ( addPaused stat tid bst' )

-- Update the state of several tids to the same state.
-- Also maintains the paused state invariant.
updateTidsState ::
    [TID.ThreadId]       -> -- Id of thread to update
    TI.ThreadState       -> -- New value of running thread
    Transformer t gs ls     -- A state transformer
updateTidsState tids stat bst =
  foldl (\bst' tid -> updateTidState tid stat bst') bst tids

-- Update state of the running thread
updateRunningBraidState :: TI.ThreadState -> Transformer t gs ls
updateRunningBraidState stat bst@(BraidState { current = cur }) =
  updateTidState cur stat bst

-- Add a new thread, with a tid already selected. The invariant that
-- a thread is in the paused list when its state is Paused is observed.
addThreadWithTid ::
    TID.ThreadId            -> -- The thread id of the new thread
    TS.ThreadInit t ls      -> -- Initial values for thread
    Transformer t gs ls        -- Final braid state
addThreadWithTid tid tinit bst@(BraidState { threads = ts }) =
  let bst' =bst { threads = TS.addThread ts tid tinit }
  in addPaused TI.Paused tid bst'

-- Add a new lifted thread, which also adds it to the paused list
-- Should never add a thread that is already in a wait state.
addThread ::
    TS.ThreadInit t ls           -> -- Initial values for thread
    Transerver t gs ls TID.ThreadId -- Final braid state
addThread tinit bst =
  let (bst', tid) = case TS.progType tinit of
                       TI.Lifted   -> genLifted bst
                       TI.Unlifted -> genGlobal bst
                       TI.Wait     -> error ( "addThread/Wait" )
  in ( addThreadWithTid tid tinit bst', tid )

-- Remove a thread from the state. By deleting both from the threads and
-- paused list, the invariant that a Paused thread is in the paused list
-- is maintained.
delThread :: TID.ThreadId -> Transformer t gs ls
delThread tid bst@(BraidState { threads = ts, paused = p, current = cur}) =
  bst { threads = TS.delThread ts tid
      , paused  = TID.fromList (delete tid (TID.toList p))
      , current = if tid == cur
                  then mkNull
                  else cur
      }

----------------------------------------------------------------------
-- We now accumulate in one data type the collection of things
-- in a braid state that are legitimately affected by a thread
-- with the specified tid.
----------------------------------------------------------------------
{-P:
data TidState t ls =
    TidState { -- Items relating to lifted threads
               tidState   :: TI.ThreadMutable t ls -- Thread state info
               -- Items relating to MVars
             , tidMVars   :: MVI.MVars        -- MVar information
             , tidMVarGen :: MVI.MVarGen      -- For generating MVar Ids
               -- Items relating to forking
             , tidFork    :: ThreadId     -- For generating TID
             }

getTidState :: ThreadId -> BraidState t gs ls -> Maybe (TidState t ls)
getTidState tid bst =
    case lookupThreadInfo tid bst of
      Nothing    -> Nothing
      Just tinfo -> 
        Just ( TidState { -- Items relating to lifted threads
                          tidState   = TI.threadMutable tinfo
                          -- Items relating to MVars
                        , tidMVars   = mvars bst
                        , tidMVarGen = mvarGen bst
                        }
             )
 -}

----------------------------------------------------------------------
-- Functions in support of forking
----------------------------------------------------------------------

-- Generate a new lifted tid
genLifted :: Transerver t gs ls TID.ThreadId
genLifted bst =
  let firstl = TID.firstLiftedSub (local bst)
      nextl  = TID.bumpTid (forkedLifted bst)
  in if TID.noTid (forkedLifted bst)
     then ( bst { forkedLifted = firstl }, firstl )
     else ( bst { forkedLifted = nextl  }, nextl )

-- Generate a new global tid
genGlobal :: Transerver t gs ls TID.ThreadId
genGlobal bst =
  let firstg = TID.firstUnliftedSub (local bst)
      nextg  = TID.bumpTid (forkedGlobal bst)
  in if TID.noTid (forkedGlobal bst)
     then ( bst { forkedGlobal = firstg }, firstg )
     else ( bst { forkedGlobal = nextg  }, nextg )

----------------------------------------------------------------------
-- Functions in support of exception handling
----------------------------------------------------------------------

-- Add a remote exception to the locale
throwTo :: L.Req -> Transformer t gs ls
throwTo req bst =
  if isSub (L.reqTo req) bst
  then addDownwardReq req bst
  else addUpwardReq req bst

-- Perform braid state level processing for an exception thrown
-- to the currently executing thread
throw :: (Show ls, Show gs) =>
    E.Exception -> Transerver t gs ls (Maybe (TI.Program t ls))
throw e bst = transerveThread (current bst) (TI.takeFirstCatcher e) bst

-- Update the thread catcher for the current thread
addCurrentCatcher :: (Show gs, Show ls) =>
    TI.Catcher t ls -> Transformer t gs ls
addCurrentCatcher catcher bst =
  transformThread (current bst) (TI.addCatcher catcher) bst

----------------------------------------------------------------------
-- Output functions
----------------------------------------------------------------------

instance (Show ls, Show gs) => Show (BraidState t gs ls) where
    show bst =
      let mvarz  = mvars bst
          lmvarz = MVI.mvarsToList mvarz
      in "\nBraid State{ "    ++ show (local bst) ++
         " // current = "     ++ show (current bst) ++
         " // delayReq = "    ++ show (delayReq bst) ++
         " // #mvars = "      ++ show (MVI.sizeMVars (mvars bst)) ++
         " // locale = "      ++ show (locale bst) ++
         " // #threads = "    ++ show (TS.lengthThreads (threads bst)) ++
         " // #paused = "     ++ show (TID.size (paused bst)) ++
         " // #delayed = "    ++ show (D.size (delayQ bst)) ++
         " // ticker = "      ++ show (ticker bst) ++
         " // threadCount = " ++ show (threadCount bst) ++
         " // global = "      ++ show (global bst) ++
         "\n\t...Paused: "    ++ show (paused bst) ++
         "\n\t...DelayQ: "    ++ show (delayQ bst) ++
         ( if length lmvarz == 0
           then ""
           else "\n" ++ concat (map MVI.outMVarP lmvarz)
         ) ++ "\t" ++ show (threads bst) ++ "\n"


----------------------------------------------------------------------
-- Functions in support of scheduling and weaving
----------------------------------------------------------------------

-- A schedule is a function that chooses the next thread to run,
-- and updates the state
type Scheduler t gs ls = Transerver t gs ls (Maybe TID.ThreadId)

-- Select the first paused thread to run
scheduleAndUpdate :: Scheduler t gs ls
scheduleAndUpdate bst@(BraidState { paused = p }) =
  if TID.nullTS p
  then ( bst, Nothing )
  else let curr = TID.headTS p
       in ( bst { paused = TID.tailTS p, current = curr }, Just curr )

-- Pause the current thread
pause :: TI.ThreadState -> Transformer t gs ls
pause thst bst = ( addPausedCurrent thst .  updateRunningBraidState thst ) bst

-- Complete an operation, such as an MVar operation
complete :: Dynamic -> TID.ThreadId -> Transformer t gs ls
complete dyn tid =
--  unsafeRet ( ">>> BST.complete: " ++ show tid )
    ( updateTidStateHard tid (TI.Completed dyn) .
      addPaused (TI.Completed dyn) tid )

-- Add a thread to the paused list, only if the state is Paused
-- or if the state is Completed
addPaused :: TI.ThreadState -> TID.ThreadId -> Transformer t gs ls
addPaused thst tid bst =
  if TI.isPausible thst
  then bst { paused = TID.addThreadSet (paused bst) tid }
  else bst

-- Add the current tid to the end of the paused tids
addPausedCurrent :: TI.ThreadState -> Transformer t gs ls
addPausedCurrent thst bst@(BraidState { current = cur }) =
  addPaused thst cur bst

{-P: {-< invariant of the braid state >-}
-- Check the invariant that if there are any threads in the paused state,
-- they should be in the paused list.
invariant1 :: Observer t gs ls Bool
invariant1 bst =
  let filt1 :: TID.ThreadId -> TI.ThreadInfo t ls -> Bool
      filt1 _tid tinfo = TI.getThreadState tinfo == TI.Paused
      pausedThreads  = TS.filterThreads filt1 (threads bst)
      filt2 :: BraidState t gs ls -> TID.ThreadId ->TI.ThreadInfo t ls -> Bool
      filt2 bst' tid _tinfo = not (elemTS tid (paused bst'))
      badThreads     = TS.filterThreads (filt2 bst) pausedThreads
  in if TS.size badThreads == 0
     then True
     else error ( "invariant1\nbadthreads: " ++ show badThreads ++
                  "pausedThreads: " ++ show pausedThreads ++ show bst)
 -}

----------------------------------------------------------------------
-- Internal implementation of thread delay, to avoid lots of polling
-- by delaying threads
----------------------------------------------------------------------

-- Delay a thread by specified tick count
-- There is an invariant maintained, that anything in the delay state
-- should be in the delay queue (and vice-versa)
delay :: TID.ThreadId -> Int -> Transformer t gs ls
delay tid count bst@(BraidState { delayQ = dq, paused = p }) =
  let bst' = bst { delayQ   = D.delay (D.mkDelayTid tid count) dq
                 , paused   = TID.delThreadSet p tid
                 , delayReq = 0
                 }
  in updateTidState tid TI.Delayed bst'

-- Request a delay for the current thread
requestDelay :: Int -> Transformer t gs ls
requestDelay t bst = bst { delayReq = t }

-- Get the list of threads that have expired
-- This also moves the expired threads to the paused state.
-- By using the updateTidState function, the invariant that paused
-- threads are in the paused state is maintained, and the invariant
-- that threads that are not delayed are not in the delay queue is
-- also maintained.
getExpired :: Int -> Transformer t gs ls
getExpired tick bst@(BraidState { delayQ = dQ }) =
  let ( dq, tids ) = D.getExpired tick dQ
  in updateTidsState tids TI.Paused ( bst { delayQ = dq } )

-- Check if a delay has been requested
delayRequested :: Observer t gs ls Bool
delayRequested bst = delayReq bst /= 0

--------------------------------------------------------------------
-- Locale manipulations at the braid state level
--------------------------------------------------------------------
-- Add an upward bound Req to the braid state
addUpwardReq :: L.Req -> Transformer t gs ls
addUpwardReq req bst@(BraidState { locale = l }) =
  bst { locale = L.addUpwardReq req l }

-- Add a downward bound Req to the lifted state
addDownwardReq :: L.Req -> Transformer t gs ls
addDownwardReq req bst@(BraidState { locale = l }) =
  bst { locale = L.addDownwardReq req l }

----------------------------------------------------------------------
-- Debugging operators
----------------------------------------------------------------------
localeNull :: Observer t gs ls Bool
localeNull = L.localeNull . locale

----------------------------------------------------------------------
-- Inline some functions
----------------------------------------------------------------------
{-# INLINE isSub                    #-}
{-# INLINE getLiftedState           #-}
{-# INLINE getProgram               #-}
{-# INLINE nThreads                 #-}
{-# INLINE updateLiftedState        #-}
{-# INLINE updateLifted             #-}
{-# INLINE transformLifted          #-}
{-# INLINE lookupThreadInfoHard     #-}
{-# INLINE updateThreadInfo         #-}
{-# INLINE updateRunningBraidState  #-}
{-# INLINE updateThread             #-}
{-# INLINE updateTidState           #-}
{-# INLINE updateTidsState          #-}
{-# INLINE addThreadWithTid         #-}
{-# INLINE addThread                #-}
{-# INLINE delThread                #-}
{-# INLINE genLifted                #-}
{-# INLINE genGlobal                #-}
-- {-# INLINE getCurrentThreadCatchers #-}
-- {-# INLINE getThreadCatcher         #-}
{-# INLINE throwTo                  #-}
{-# INLINE throw                    #-}
{-# INLINE addCurrentCatcher        #-}
{-# INLINE scheduleAndUpdate        #-}
{-# INLINE pause                    #-}
{-# INLINE addPaused                #-}
{-# INLINE addPausedCurrent         #-}
{-# INLINE delay                    #-}
{-# INLINE getExpired               #-}
{-# INLINE requestDelay             #-}
{-# INLINE updateProg               #-}
{-# INLINE addUpwardReq             #-}
{-# INLINE addDownwardReq           #-}
