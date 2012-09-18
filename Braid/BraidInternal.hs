-- Copyright (c) Peter Duncan White, 2003
module BraidInternal
    ( -- Braid braid state with MVars (Braid)
    , Braid                    -- Braid using basic braid state
    , threads                  -- Get the internal RSE monad out of the braid
    , BraidSt                  -- State internal to Braid
    , lift                     -- Lift an IO action to the braid
      -- Internal braid value, and associated methods
    , LT.Thread                -- A single thread monad
    , Threads                  -- A resumption monad of many threads
      -- Unlifted threads
    , UnliftedThread           -- An unlifted thread in the braid monad
      -- Operations used in the briad
    , fetch                    -- Fetch the braid state
    , runM                     -- Run a resumption in a given state,
                               -- staying in the underlying monad (ID for now)
    , nullPause                -- Generate a pause in the action
    , pauseInState             -- Pause with specified thread state
      -- Operators in support of MVars
    , newMVar                  -- Create a new MVar
    , takeMVar                 -- Take the value of an MVar
    , putMVar                  -- Put a value into an MVar
    , deleteMVar               -- Remove MVar from state
    , dereferenceMVar          -- Get value of MVar from braid state
    , MV.MVar                  -- Abstract type for MVars
      -- Operators in support of forking and dynamic threads
    , killThread               -- Remove a thread from the braid
    , TID.ThreadId             -- Abstract type for thread ids
    , myThreadId               -- Get the current thread Id
    , fork                     -- For a not necessarily lifted thread
      -- In support of output to the screen
    , putStr                   -- Put a string to the screen
    , putStrLn                 -- Put string plus line terminator to the screen
    , showState                -- For debugging only
    , getLocal                 -- Get local id of the braid
    , threadCount              -- Count thread scheduling
      -- Operators in support of exception handling
    , addCurrentCatcher        -- Add a thread catcher for running thread
    , E.Exception (..)         -- Re-export exception stuff
    , throw                    -- A local exception throw
    , throwTo                  -- Throw an exception to a thread
    , Catcher                  -- Type of thread catchers
      -- Functions in support of scheduling and weaving
    , scheduleAndUpdate        -- Choose first paused thread to run
    , weave                    -- Weave threads into a braid
    , Scheduler                -- Type of the scheduling algorithm
    , threadDelay              -- Delay for specified tick count
    , getElapsed               -- Get the elapsed time
    , Program                  -- Global or local program
    , BST.mkUnliftedProg       -- A global program constructor
      -- In support of braid initialization
    , mkBraid                  -- Make a braid, run it to completion
      -- Definitions used only in separation property
    , P.Schedule               -- A schedule of thread Ids to run
    , ThreadFilter             -- Thread ids to omit
      -- Thread initialization
    , BST.ThreadInit (..)      -- Data required for thread initialization
    , BST.ProgramType (..)     -- Lifted or unlifted thread?
      -- In support of the layered braid
    , forkLift                 -- Lift a local thread into the braid (no tid)
    , bundle                   -- Lower braid as higher thread
    ) where

-- Haskell imports
import Dynamic (Dynamic, Typeable, toDyn, fromDyn)
import Prelude hiding ( putStrLn, putStr )
import Monad
import Maybe
-- Utility imports
import Null
-- Resumption imports
import qualified Ex as E
-- Local imports
import qualified ThreadId as TID
import qualified BraidState as BST
import qualified MVarProc as MVP
import qualified BraidMVar as MV
import qualified Locale as L
import qualified LiftedState as LS
import qualified LiftedThread as LT
import qualified Thread as T
import qualified BraidLocal as BL
-- Imports for the separation property
import qualified Policy as P

----------------------------------------------------------------------
--  A haskell 98 substitute for the IO monad with concurrency.
----------------------------------------------------------------------

----------------------------------------------------------------------
--  braid uses the basic braid state, supporting a fixed
--  set of threads (no fork), and no interthread communications
--
--  Within this module, we get the lifted separation property,
--  However, programs coded in Braid itself have no useful
--  separation property, since they can manipulate the shared
--  state in an arbitrary fashion.
--
--  The braid is built up in the following fashion:
--
--  LiftedState:
--    The state visible to a single thread
--  Thread a:
--    A thread that operates on a lifted state
--  BraidState:
--    Braid state, used to combine many threads. Braid is the
--    simplest version, having no support for forkIO or MVars
--  BraidSt:
--    BraidState specialized to a thread type of Braid ()
--  Threads:
--    A resumption having BraidSt as its internal state
--  Braid:
--    Tags a Threads with the Braid type, allowing for recursive
--    type definitions.
----------------------------------------------------------------------

-- An unlifted thread
type UnliftedThread gs ls = Braid gs ls ()
-- A global (unlifted) program, or a lifted program
type Program gs ls   = BST.Program (UnliftedThread gs ls) ls
type Catcher gs ls   = E.Exception -> UnliftedThread gs ls

-- Specialize the braid state
type BraidSt gs ls     = BST.BraidState (UnliftedThread gs ls) gs ls
type Transformer gs ls = BST.Transformer (UnliftedThread gs ls) gs ls
type Observer gs ls a  = BST.Observer (UnliftedThread gs ls) gs ls a
-- A combined transformer and observer
type Transerver gs ls a = BST.Transerver (UnliftedThread gs ls) gs ls a

-- A resumption monad over a collection of threads
-- Note that:  Threads gs ls a = Thread (BraidSt gs ls) a
type Threads gs ls a  = T.Thread (BraidSt gs ls) a
-- A braid of many threads, using base braid state (Braid)
data Braid gs ls a    = Braid { threads :: Threads gs ls a }

instance (Null a, Null gs) => Null (Braid gs ls a) where
    mkNull = Braid mkNull

instance Show (Braid gs ls a) where
    show _ = "Braid *"

-- Lift from the thread monad into the threads monad
-- This lift runs the lifted thread using only the lifted state
-- corresponding to the lifted thread, thus the thread is run
-- in isolation from other threads.
-- This is the key part of the lift function that yields the
-- separation theorems.
-- This version returns delay count, zero if no delay
-- Also returns a flag when the lifted thread is done.
liftThread :: (Show ls, Show gs) =>
    Int -> TID.ThreadId -> Braid gs ls (Maybe Int, Bool)
liftThread n tid =
  do { (ls, prog) <- prepareThread tid
     ; (ls', prog', done) <- lRunM ls prog
     ; md <- updateThread n tid ls' prog'
     ; return (md, done)
     }

-- The preparation phase of the thread lifting.
-- The lifted state is accessed, and paired with the lifted thread to run.
prepareThread :: (Show gs, Show ls) =>
    TID.ThreadId -> Braid gs ls (LT.LiftedSt ls, LT.Program ls)
prepareThread tid =
  do { (ls, prog) <- observe ( BST.getLiftedRun tid )
     ; ls' <- localize tid ls
     ; return (ls', prog)
     }

localize :: (Show gs, Show ls) =>
    TID.ThreadId      -> -- Thread Id of lifted thread
    LT.LiftedSt ls    -> -- Incoming value of lifted state
    Braid gs ls ( LT.LiftedSt ls ) -- Outgoing value of lifted state
localize tid ls =
  do { -- bst <- fetch
--      ; when (not (BST.localeNull bst))
--             (putStrLn ( "... localize / not null.2: " ++ show tid ++
--                         "\n\tbst loc: " ++ show (BST.locale bst) ++
--                         "\n\tls  loc: " ++ show (LT.locale ls)))
     ; lifta (MVP.localize tid ls)
     }

-- Update a lifted thread, after executing it.
updateThread :: (Show ls, Show gs) =>
    Int                  -> -- For debugging
    TID.ThreadId         -> -- The id applied to the thread
    LT.LiftedSt ls       -> -- Resulting lifted state from the run
    LT.Program ls        -> -- Resulting program from the run
    Braid gs ls ( Maybe Int         -- Possible thread delay value
                )
updateThread n tid ls prog =
  do { ( mdelay  -- Possible delay requested
       , reqs    -- Upward bound requests
       , ls'     -- New value of lifted state
       ) <- lifta (MVP.globalize n tid ls prog)
     ; liftSt ( MVP.processLiftedReqs n tid ls' reqs )
     ; return ( mdelay )
     }

-- Fork a not necessarily lifted thread, when a tid has already
-- been generated.
forkWithTid :: (Null ls) =>
    String                -> -- The name of the new program
    TID.ThreadId          -> -- The thread id for the new thread
    Program gs ls         -> -- Thread program (lifted or global)
    Braid gs ls TID.ThreadId -- The braid action
forkWithTid name tid program =
  let tinit = BST.ThreadInit { BST.name        = name
                             , BST.program     = program
                             , BST.liftedState = LS.mkLiftedState name tid
                             }
  in addThreadWithTid tid tinit >> return tid

-- Fork a thread, which is not necessarily lifted
fork :: (Null ls) => String -> Braid gs ls () -> Braid gs ls TID.ThreadId
fork name program =
   genGlobal >>= \tid -> forkWithTid name tid (BST.mkUnliftedProg program)

-- Fork a lifted thread
forkLift :: (Null ls) =>
    String                -> -- Name of the thread to lift
    LT.Thread ls ()       -> -- The thread to lift
    Braid gs ls TID.ThreadId -- Thread lifted and forked
forkLift name prog =
  genLifted >>= \tid ->forkWithTid name tid (BST.mkLiftedProg prog)

-- Show the braid state
showState :: (Show ls, Show gs) => String -> Braid gs ls ()
showState s = do { bst <- fetch
                 ; putStrLn ( "\nshowState: " ++ s ++ " // " ++ show bst )
                 }

getLocal :: Braid gs ls (TID.ThreadId)
getLocal = observe BST.local

{-P: {-< Properties of lift >-}
-- Lifting two threads into a braid produces two threads that commute
-- in the context of the braid.
assert LiftCommutation =
    All tid1 :: tid.
    All th1  :: LT.Thread ls ().   -- Stating this for null return value
    All th2  :: LT.Thread ls ().   -- Stating this for null return value
    All tid2 :: tid.
      ( -/ ( { tid1 } === { tid2 } ) ) ==>
      ( { lift tid1 th1 >> lift tid2 th2 } ===
        { lift tid2 th2 >> lift tid1 th1 }
      )

-- Same property, where the threads can have return values
assert LiftCommutationa =
    All tid1 :: tid.
    All th1  :: LT.Thread ls a.
    All th2  :: LT.Thread ls b.
    All tid2 :: tid.
      ( -/ ( { tid1 } === { tid2 } ) ) ==>
      ( { lift tid1 th1 >>= \_ -> lift tid2 th2 } ===
        { lift tid2 th2 >>= \_ -> lift tid1 th1 }
      )
 -}

-- Lift a braid state transformer to a Braid
liftSt :: Transformer gs ls -> Braid gs ls ()
liftSt = Braid . T.update

-- Lift a braid state observer to a Braid
observe :: Observer gs ls a -> Braid gs ls a
observe = Braid . T.observe

-- Lift a braid state transformer to a Braid, including a return value
lifta :: Transerver gs ls a -> Braid gs ls a
lifta = Braid . T.lifta

-- The braid is a monad, adding the Braid constructor to the underlying
-- RSE monad return and bind.
instance Monad (Braid gs ls) where
    return = Braid . return
    Braid p >>= k = Braid ( p >>= threads . k )

----------------------------------------------------------------------
-- Lift some of the RSE operations
----------------------------------------------------------------------

-- Run the resumption monad, in the underlying monad
-- This version is for a global (unlifted) thread
runM ::
    UnliftedThread gs ls ->     -- Incoming program state
    BraidSt gs ls        ->     -- Incoming braid state
    IO ( BraidSt gs ls          -- Outgoing braid state
       , ( UnliftedThread gs ls -- Outgoing program state
         , Bool                 -- If thread is done
         )
       )
runM prog bst =
   do { (bst', prog', done) <- T.runM bst (threads prog)
      ; return ( bst', ( Braid prog', done ) )
      }

-- Run the resumption monad, in the underlying monad
-- Returns a flag when the thread is done
braidRunM :: UnliftedThread gs ls -> Braid gs ls ( UnliftedThread gs ls, Bool )
braidRunM prog = liftma (runM prog)

-- Run the resumption monad, in the underlying monad
-- This version is for a lifted thread
lRunM ::
    LT.LiftedSt ls   -> -- Incoming value of lifted state
    LT.Thread ls a   -> -- Incoming lifted thread state
    Braid gs ls ( LT.LiftedSt ls    -- Outgoing value of lifted state
                , LT.Thread ls a    -- Outgoing thread state
                , Bool              -- True when thread is done
                )
lRunM ls thread = lift ( LT.runM ls thread )

-- Store a new state variable
store :: BraidSt gs ls -> Braid gs ls ()
store = Braid . T.store

-- Fetch the state component of the Braid
fetch :: Braid gs ls (BraidSt gs ls)
fetch = Braid T.fetch

-- A null pause, i.e. a pause returning a unit type
nullPause :: (Null a) => Braid gs ls a
nullPause = Braid T.nullPause

-- Get the elapsed time
getElapsed :: Braid gs ls Int
getElapsed = Braid T.getElapsed

-- Delay for specified tick count
threadDelay :: Int -> Braid gs ls ()
threadDelay n =
  getElapsed >>= \t -> pauseSt ( BST.requestDelay ( t + ( n `div` 10000 ) ) )

-- Run until a result is returned, and return only the state
-- This version stays in the underlying monad, which for now is
-- the ID monad.
completeStateM :: Braid gs ls a -> BraidSt gs ls -> IO (BraidSt gs ls)
completeStateM (Braid r) bst = T.completeStateM r bst

-- Lift an IO action into the braid
lift :: IO a -> Braid gs ls a
lift = Braid . T.lift

-- Lift a state delta with return value from the underlying monad (IO)
liftma :: (BraidSt gs ls -> IO (BraidSt gs ls, a)) -> Braid gs ls a
liftma = Braid . T.liftma

-- Observe the current thread Id
myThreadId :: Braid gs ls TID.ThreadId
myThreadId = Braid (T.observe BST.current)

{-P: {-< Functions in support of the separation property >-}
-- Observe the lifted state of a tid
getTidState ::
    TID.ThreadId ->
    Braid gs ls (Maybe (BST.TidState (UnliftedThread gs ls) ls))
getTidState tid = Braid (T.observe (BST.getTidState tid))

 -}

-- Pause the thread state of the currently running thread, which is
-- always the first thread in the list. At the pause, the currently
-- running thread is made the last thread in the list.
pauseInState :: BST.ThreadState -> Braid gs ls ()
pauseInState thst = Braid (T.pauseSt (BST.pause thst))

-- Create a pause from a braid action
pause :: Braid gs ls a -> Braid gs ls a
pause = Braid . T.pause . threads

-- Pause a state transformer
pauseSt :: (Null a) => Transformer gs ls -> Braid gs ls a
pauseSt = Braid . T.pauseSt

-- Pause a state transformer with a unit return
-- pauseStUnit :: Transformer gs ls -> Braid gs ls ()
-- pauseStUnit = Braid . T.pauseSt 

-- Get the thread count from the internal state
threadCount :: Braid gs ls Int
threadCount = observe BST.threadCount

----------------------------------------------------------------------
--  Lift some braid state stuff into the Braid monad
----------------------------------------------------------------------

-- Delete a thread from the braid
killThread :: TID.ThreadId -> Braid gs ls ()
killThread = liftSt . BST.delThread

----------------------------------------------------------------------
-- Primitives in support of MVar manipulation
----------------------------------------------------------------------

-- Take an MVar. This version is for global (unlifted) threads,
-- operating at the level of the braid, taking an MVar at any
-- level.
takeMVar :: (Typeable a, Show ls, Show gs) => MV.MVar a -> Braid gs ls a
takeMVar mvar =
  do { -- putStrLn ( ">>> BI.takeMVar: " ++ show mvar )
     ; current <- myThreadId
     ; liftSt ( MVP.unliftedTakeMVar (L.mkTakeReq (MV.mvarId mvar) current) )
--      ; putStrLn ( "... BI.takeMVar.2: " ++ show mvar ++
--                   " // " ++ show current )
     ; st <- observe (BST.getState current)
--      ; putStrLn ( "... BI.takeMVar.3: " ++ show st )
     ; case st of
         BST.Completed dyn ->
           let a = fromDyn dyn ((error ( "BI.takeMVar.Dyn: " ++ show st)) :: a)
           in return a
         _              -> error ( "BI.takeMVar: " ++ show st )
     }

-- Put a value in an MVar
putMVar :: (Typeable a, Show ls, Show gs) => MV.MVar a -> a -> Braid gs ls ()
putMVar mvar a =
  do { current <- myThreadId
     ; let mvop = L.mkPutReq (MV.mvarId mvar) current (toDyn a)
     ; liftSt (MVP.unliftedPutMVar mvop)
     ; return ()
     }

-- Add an MVar
newMVar :: (Show gs, Show ls) =>
    String -> Maybe Dynamic -> Braid gs ls (MV.MVar a)
newMVar name mdyn = do { tid <- myThreadId
                       ; lifta (MVP.newMVar tid name mdyn)
                       }

-- Delete an MVar
deleteMVar :: MV.MVar a -> Braid gs ls ()
deleteMVar mvar = liftSt (MVP.deleteMVar mvar)

-- Observe the value associated with an MVar id
dereferenceMVar :: (Typeable a) => MV.MVar a -> Braid gs ls (Maybe a)
dereferenceMVar mvar = Braid (T.observe (MVP.dereferenceMVar mvar))

{-P: {-< Functions used only in properties >-}
-- Reset the state associated with the specified tid
clobber :: (Null gs, Null ls) => TID.ThreadId -> Braid gs ls ()
clobber tid = liftSt (BST.clobber tid)

-- Clobber states corresponding to several tids
clobbers :: (Null gs, Null ls) => [TID.ThreadId] -> Braid gs ls ()
clobbers = foldl (\a tid -> a >> clobber tid) (return ())

-- Initializing tid2 has no effect upon a lifted computation under tid1
-- Note: I am currently using the P-Logic "===", but I find this to
-- be meaningless. I think I need to switch to my RSEEq relation.
assert InitTidCommutation =
    All tid1 :: tid.
    All th   :: LT.Thread ls ().   -- Stating this for null return value
    All tid2 :: tid.
      ( -/ ( { tid1 } === { tid2 } ) ) ==>
      ( { lift tid1 th >> clobber tid2 } ===
        { clobber tid2 >> lift tid1 th }
      )

assert InitTidCommutationa =
    All tid1 :: tid.
    All th   :: LT.Thread ls a.   -- Stating this for any return value
    All tid2 :: tid.
      ( -/ ( { tid1 } === { tid2 } ) ) ==>
      ( { lift tid1 th >>= \_ -> clobber tid2 } ===
        { clobber tid2 >> lift tid1 th }
      )

-- Initializing the thread information for a tid erases the effect of
-- a computation lifted under the tid.
assert InitTidAbsorption =
    All tid  :: tid.
    All th   :: LT.Thread ls ().
      ( { lift tid th >> clobber tid } === { clobber tid } )

 -}

----------------------------------------------------------------------
-- Functions in support of forking
----------------------------------------------------------------------

-- Generate a lifted tid and update the forked value
genLifted :: Braid gs ls TID.ThreadId
genLifted = lifta BST.genLifted

-- Generate a global tid and update the forked value
genGlobal :: Braid gs ls TID.ThreadId
genGlobal = lifta BST.genGlobal

-- Add a thread to the braid, with the tid already specified
addThreadWithTid ::    
    TID.ThreadId                       -> -- Tid of new thread
    BST.ThreadInit (Braid gs ls ()) ls -> -- Thread init info
    Braid gs ls ()                        -- A braid action
addThreadWithTid tid tinit = liftSt (BST.addThreadWithTid tid tinit)

----------------------------------------------------------------------
-- Functions in support of exception handling
----------------------------------------------------------------------

-- Convert a braid internal level catcher into a braid state level
-- catcher
unLiftCatcher :: Catcher gs ls -> BST.Catcher (UnliftedThread gs ls) ls
unLiftCatcher catcher = \e -> BST.UnliftedProgram (catcher e)

-- Update the thread catcher for the current thread
addCurrentCatcher :: (Show gs, Show ls) => Catcher gs ls -> Braid gs ls ()
addCurrentCatcher c = liftSt (BST.addCurrentCatcher (unLiftCatcher c))

-- A local exception throw
-- When the braid state level throw returns a program, it should be
-- run immediately. It has already been stored in the braid state thread
-- info.
throw :: (Show gs, Show ls) => E.Exception -> Braid gs ls ()
throw e =
  do { mprog <- lifta (BST.throw e)
     ; case mprog of
          Nothing   -> return ()
          Just prog ->
            case prog of
              BST.UnliftedProgram p ->
                do { putStrLn ("BI.throw.1: " ++ show e)
                   ; pause p
                   }
              BST.LiftedProgram _p  -> error ( "BI.throw / LiftedProgram" )
              BST.WaitProgram _p    -> error ( "BI.throw / WaitProgram" )
     }

-- Perform the throw at the braid level
throwTo :: (Show gs, Show ls) =>
    TID.ThreadId -> E.Exception -> Braid gs ls ()
throwTo tid e =
  do { mytid <- myThreadId
     ; let req = L.ThrowToReq { L.reqTo  = tid
                              , L.reqExp = e
                              , L.reqTid = mytid
                              }
     ; liftSt (BST.throwTo req)
     }

----------------------------------------------------------------------
-- Functions in support of output to the screen
----------------------------------------------------------------------

-- The braid version of the Haskell function putStrLn
putStrLn :: String -> Braid gs ls ()
putStrLn = Braid . T.putStrLn

-- The braid version of the Haskell function putStr.
putStr :: String -> Braid gs ls ()
putStr = Braid . T.putStr

----------------------------------------------------------------------
--  Scheduling and weaving
--  A braid consists of threads, and the threads can be interleaved
--  in many ways. For example, round robin would run each thread
--  until it pauses, and then repeat for another round. Sequential
--  scheduling would be to choose one thread, and run it unitl it
--  completes (if it ever does), and then choose another thread to
--  complete.
--
--  We want the separation properties to be independent of the
--  scheduler, thus we make the scheduler a parameter
----------------------------------------------------------------------

-- A schedule is a function that chooses the next thread to run,
-- and updates the state.
type Scheduler gs ls = Braid gs ls (Maybe TID.ThreadId)

-- A scheduler that selects the first paused state to run, by
-- specialising the braid state scheduler.
scheduleAndUpdate :: Scheduler gs ls
scheduleAndUpdate = lifta BST.scheduleAndUpdate

-- Weave the braid, probably forever
-- This function sets up the loop for weave', which is actually
-- the loop.
weave :: (Show ls, Show gs) =>
    Scheduler gs ls -> BraidSt gs ls -> Braid gs ls ()
weave sched bst =  store bst >> weave' sched 0

-- Weave the braid forever, counting thread switches.
-- This is a helper function for weave.
weave' :: (Show ls, Show gs) => Scheduler gs ls -> Int -> Braid gs ls ()
weave' sched n =
  do { when (n `mod` 32768 == 0)
            ( do { loc <- observe BST.local
                 ; nt  <- observe BST.nThreads
                 ; putStrLn ( "* Threads " ++ show loc ++ " = " ++
                              show n ++ " / " ++ show nt )
--                 ; when (n == 32768) (showState "weave'")
                 }
            )
     ; done <- oneWeave' n sched
     ; if done
       then return ()
       else weave' sched (n + 1)
     }

-- One cycle through, including the scheduling
oneWeave' :: (Show ls, Show gs) =>
    Int -> Scheduler gs ls -> Braid gs ls Bool
oneWeave' n sched =
  do { elapsed <- getElapsed
     ; getExpired elapsed
     ; mtid    <- sched
     ; case mtid of
         Nothing  -> do { nt <- observe BST.nThreads
                        ; return (nt == 0)
                        }
         Just tid -> do { oneWeave n tid
                        ; return False
                        }
     }

-- One cycle through, not including the scheduling
oneWeave :: (Show ls, Show gs) => Int -> TID.ThreadId -> Braid gs ls ()
oneWeave n tid =
  if TID.isLifted tid
  then do { (mdelay, done) <- liftThread n tid
          ; t <- getElapsed
            -- Raise the local delay to the enclosing braid, where it
            -- can be implemented.
          ; liftSt (MVP.globalizeDelay t tid mdelay)
          ; if done
            then killThread tid
            else return ()
          }
  else do { prog  <- prepareWeave tid
          ; (prog', done) <- braidRunM prog
          ; liftSt (updateWeave tid prog')
            -- When requests are moved into the braid state,
            -- from a non-local source and they turn out to be for
            -- a local MVar, process them here. This is somewhat confusing
            -- terminology, since the only way that a request becomes
            -- a "local" request in the braid is when it starts out
            -- "non-local", from another layer of the layered braid.
            -- Requests that originate locally are processed by the
            -- take and put MVar operations in MVarProc.hs
          ; reqs <- lifta MVP.takeLocalReqs
          ; when (not (null reqs))
                 (putStrLn ( "...oneWeave / local reqs: " ++ show reqs ) )
            -- The requests in the local braid came from non-local
            -- source, so call non-local processing for them.
          ; liftSt (MVP.processNonLocalReqs reqs)
            -- Process responses to MVars operations that
            -- originated here
          ; rsps <- lifta MVP.takeLocalRsps
          ; liftSt (MVP.processRsps rsps)
          ; if done
            then killThread tid
            else return ()
          }

-- Update the result of stepping a global thread
updateWeave :: (Show ls, Show gs) =>
    TID.ThreadId         -> -- The thread just run
    UnliftedThread gs ls -> -- Incoming value of thread program
    Transformer gs ls       -- 
updateWeave tid prog bst =
  let unlifted = BST.UnliftedProgram prog
      delreq   = BST.delayReq bst
      bst'     = seq delreq (BST.delay tid delreq bst)
  in if BST.delayRequested bst
     then BST.updateProg tid unlifted bst'
     else BST.updateProg tid unlifted bst

-- Prepare for execution of a thread
prepareWeave :: (Show gs, Show ls) =>
    TID.ThreadId  -> -- Thread Id of unlifted thread
    Braid gs ls (UnliftedThread gs ls) -- Outgoing thread state
prepareWeave tid = observe (BST.getUnliftedProg . (BST.getProgram tid))

-- Get the expired delayed threads
getExpired :: Int -> Braid gs ls ()
getExpired elapsed = liftSt (BST.getExpired elapsed)

----------------------------------------------------------------------
--  Intransitive purge for a schedule
--  Based on John Rushby "Noninterference, Transitivity, and
--  Channel-Control Security Policies", December 1992
--
--  For the intransitive purge, we need a global view of the sequence
--  of actions
----------------------------------------------------------------------

----------------------------------------------------------------------
--  Functions in support of braid initialization
----------------------------------------------------------------------

-- Take an initializer thread and run it, producing an initial braid
-- state, and an unlifted thread to start the execution of the braid.
mkBraidInitializer :: (Null ls) =>
    String             -> -- Name of the first thread
    TID.ThreadId       -> -- Tid for the new braid
    gs                 -> -- Initializer for global braid state
    Int                -> -- Start value for ticker
    Program gs ls      -> -- Program for the first thread
    BraidSt gs ls         -- Initialized braid state
mkBraidInitializer name tid gs start prog =
  let tinit    = BST.ThreadInit { BST.name       = name
                                , BST.program    = prog
                                , BST.liftedState = LS.mkLiftedState name tid
                                }
      (bst, inittid) = BST.addThread tinit (BST.initBraidState gs tid start)
  in bst { BST.current = inittid }

-- Create a braid and run it to completion
-- This makes a global thread
mkBraid :: ( Null ls, Show ls, Show gs ) =>
    String          -> -- Name of first program
    gs              -> -- Initial value for global state
    Int             -> -- Initial value for lifted ticker
    Scheduler gs ls -> -- The scheduling algorithm
    Program gs ls   -> -- Program for first thread
    IO (BraidSt gs ls) -- An IO action to return the initial braid state
mkBraid name gs start sched prog =
  let bst = mkBraidInitializer name TID.firstUnlifted gs start prog
  in completeStateM ( weave sched bst ) bst

----------------------------------------------------------------------
--  State the lift / separate property of braid threads.
----------------------------------------------------------------------

type ThreadFilter = [TID.ThreadId]

{-P: {-< Filter functions for the separation property >-}

-- A scheduler can be subjected to a filter, so that it never selects
-- any of the threadIds in the exclude list
filterScheduler :: [TID.ThreadId] -> Scheduler gs ls -> Scheduler gs ls
filterScheduler filt sched =
  do { tid <- sched
     ; if isNothing tid || elem (fromJust tid) filt
       then filterScheduler filt sched
       else return tid
     }

-- Step a single tid within a braid
runTid :: TID.ThreadId -> BraidSt gs ls -> Braid gs ls ()
runTid tid bst =
  do { rn <-braidRunM Run { state = bst, cont  = BST.getThreadProgram tid bst }
     ; updateCurrent (cont rn)
     }

-- Step a single tid within a braid
runTidObserve ::
    (BraidSt gs ls, [P.Observation ls]) -> -- Init braid, st, and obs
    TID.ThreadId                        -> -- Tid to run for 1 pause
    -- Final braid, st, and observations
    Braid gs ls (BraidSt gs ls, [P.Observation ls])
runTidObserve (bst, obs) tid =
  do { rn <- braidRunM Run { state = bst
                           , cont  = BST.getThreadProgram tid bst
                           }
     ; let bst'  = BST.updateProg tid (cont rn) (state rn)
           ob    = P.Observation { P.oid = tid
                                 , P.ols = BST.liftedState tid (state rn)
                                 }
       in return ( bst', ob:obs ) -- Append in reverse order
     }

-- Run the braid according to a schedule
runScheduled ::
    P.Schedule             -> -- Schedule of threads to run
    BraidSt gs ls          -> -- The initial internal state of the braid
    Braid gs ls ( BraidSt gs ls,  [P.Observation ls ] )
runScheduled sched bst = foldM runTidObserve (bst, []) sched

-- Produce the list of lifted states corresponding to a schedule
observations ::
    P.Schedule    -> -- Schedult to run
    BraidSt gs ls -> -- BraidSt to start the run
    Braid gs ls [P.Observation ls] -- Result of the un
observations sched bst =
  do { ( _bst, obs ) <- runScheduled sched bst
     ; return obs
     }

-- Reduce the outputs by filtering out those outputs resulting
-- from steps taken by threads in the filter set. This corresponds
-- to the transitive purge of Rushby. It is called "transitive"
-- because it relates to the "influence" relation, which is
-- transitive.
-- This function corresponds to the "purge" of Rushby, except
-- that this function assumes a null communication policy
-- (complete separation of the threads)
filterObservationsTransitive ::
    P.Schedule         -> -- The schedule to filtert
    ThreadFilter       -> -- The filtering function
    [P.Observation ls] -> -- The observations to filter
    [P.Observation ls]    -- The filtered observation
filterObservationsTransitive sched filt obs =
  foldl (filterSingle filt) obs sched
-- Helper function
filterSingle ::
    ThreadFilter       -> -- The filtering functions
    [P.Observation ls] -> -- The observation to filter
    TID.ThreadId       -> -- The thread id to filter out
    [P.Observation ls]    -- The filtered observations
filterSingle filt obs' tid =
  if tid `elem` filt
  then tail obs'
  else obs'

-- Filter out the filter set of threads from the schedule.
filterScheduleTransitive :: P.Schedule -> ThreadFilter -> P.Schedule
filterScheduleTransitive sched filt = filter (`elem` filt) sched

-- Reduce the inputs by filtering out those resulting from steps
-- taken by threads that could have influenced the target tids
-- under the "directly influence" relation, which is an intransitive
-- relation.
-- This function does not assume a null policy, it is well defined in
-- terms of any valid (reflexive) communication policy.
filterScheduleIntransitive ::
    P.Schedule -> P.Policy -> TID.ThreadId -> P.Schedule
filterScheduleIntransitive = P.intransitiveInputPurge

-- Reduce the outputs by filtering out those outputs resulting
-- from steps taken by threads that were purged owing to the
-- intranstive purge of the inputs.
filterObservationsIntransitive ::
    P.Policy           -> -- The communication policy
    TID.ThreadId       -> -- The target thread
    [P.Observation ls] -> -- The observations of the unrestricted system
    [P.Observation ls]    -- The reduced set of observations
filterObservationsIntransitive = P.intransitiveOutputPurge
 -}

{-P: {-< Braid internal lift/separate property >-}
-- The lift and separate property, for a transitive null policy
assert LiftAndSeparateTransitive =
    All sched  :: P.Schedule.
    All filt   :: ThreadFilter.
    All bst    :: BraidSt tid gs ls.
    All tinits :: [BST.ThreadInit (LT.Thread ls ()) ls].
        { filterObservationsTransitive
            sched filt (observations sched (lifts bst tinits))
        } ===
        { observations (filterScheduleTransitive sched filt)
                       (lifts bst tinits)
        }

----------------------------------------------------------------------
-- Defining what is meant by non-interference in a braid consisting
-- of some (not necessarily lifted) threads.
-- The first set of definitions uses the observations function.
----------------------------------------------------------------------

property NonInterference1 =
    {| tids :: [tid]
     , ths  :: [BST.ThreadInit (UnliftedThread gs ls) ls]
     |
        All filt   :: ThreadFilter.
        All sched  :: P.Schedule.
        All bst    :: BraidSt gs ls.
        { filterObservationsTransitive
            sched                     -- The schedule
            filt                      -- The tids to filter out
            ( observations
                sched                 -- Unfiltered scheduler
                (braid bst tids ths)  -- Braid with filtered tids
            )                         -- Lifted state observations
        } ===
        { observations
            (filterScheduleTransitive sched filt) -- The filtered schedule
            (braid bst tids ths)        -- Braid without filtered tids
        }
     |}

-- A special case for two non interfering, not necessarily lifted threads
property NonInterference1a =
    {| tid1 :: tid
     , th1  :: BST.ThreadInit (UnliftedThread gs ls) ls
     , tid2 :: tid
     , th2  :: BST.ThreadInit (UnliftedThread gs ls) ls
     |  All filt   :: ThreadFilter.
        All sched  :: P.Schedule.
        All bst    :: BraidSt gs ls.
        { filterObservationsTransitive
            sched filt (observations sched (braid bst [tid1, tid2] [th1, th2]))
        } ===
        { observations
            (filterScheduleTransitive sched filt)
            (braid bst [tid1, tid2] [th1, th2])
        }
     |}

-- Now construct an interference property
property Interference1 =
    {| tid1 :: tid
     , th1  :: BST.ThreadInit SingleThread ls
     , tid2 :: tid
     , th2  :: BST.ThreadInit SingleThread ls
     |
        -/ (NonInterference1a tid1 th1 tid2 th2)
     |}

----------------------------------------------------------------------
-- The next set of non-interference definitions uses the thread weaver.
----------------------------------------------------------------------
-- Non interference for a collection of not necessarily lifted threads.
-- I like this one.
property NonInterference2 =
    {| ths :: [BST.ThreadInit SingleThread ls]
     |
       All bst    :: BraidSt tid gs ls.
       All sched  :: Scheduler tid ls.
       All tids   :: [tid].
       { weave sched bst >> clobbers tids } ===
       { weave (filterScheduler tids sched) bst }
     |}

-- Isolating the causes of interference: If we run two threads, where
-- one is a general thread (not lifted), and one is a lifted thread,
-- and the first (unlifted) thread affects the second thread,
-- then the first (unlifted) thread must at some point affect the
-- state information about the second thread
assert InterferenceCause =
    All bst    :: BraidSt tid gs ls.
    All tid1   :: tid.
    All th1    :: BST.ThreadInit SingleThread ls.
    All tid2   :: tid.
    All th2    :: BST.ThreadInit SingleThread ls.
    Interference1 tid1 th1 tid2 th2 ==>
    (-/ ( { runTid tid1 bst >> getTidState tid2 } ===
          { getTidState tid2 }
        )
    )
 -}

----------------------------------------------------------------------
-- Now for intransitive separation, per Rushby
----------------------------------------------------------------------
{-P: {-< The intransitive lift and separate property.>-}
assert LiftAndSeparateIntransitive =
    All sched  :: P.Schedule.
    All policy :: P.Policy.
    All target :: tid.
    All bst    :: BraidSt tid gs ls.
    All tinits :: [BST.ThreadInit (Thread ())].
        { filterObservationsIntransitive
            policy target (observations sched (lifts bst tinits))
        } ===
        { observations (filterScheduleIntransitive sched policy target)
                       (lifts bst tinits)
        }
 -}

----------------------------------------------------------------------
-- Support of a layered braid, i.e. executing in the higher (e.g.
-- platform) braid, but adding a thread to a braid state in the
-- lower (e.g. domain) braid
----------------------------------------------------------------------

-- Here is yet another version
-- (hilod is Ilongo for "twist")
-- They say that not to decide is to decide. By not deciding how to
-- lose the "ms" parameter, we have decided how to lose it. The
-- decision is deferred by passing it to the function recursively.
-- hilod :: GlobalRun ls ms -> IO (LT.Run ls ())
-- hilod r = runM r >>= hilod
twist :: BraidSt ls ms -> UnliftedThread ls ms -> LT.Thread ls ()
twist bst prog =
  do { bst1 <- lower bst
     ; (bst2, prog', done) <- LT.lift (T.runM bst1 (threads prog))
     ; bst3 <- raise bst2
     ; if done then return () else twist bst3 (Braid prog')
     }

-- Convert a braid to a lifted thread at the higher level, and then
-- make this a braid level thread at the higher level.
-- This used to be called "encapsulate"
bundle :: ( Null ls, Null ms, Show ms, Show ls ) =>
    String                -> -- Name of the bundled lower braid
    Braid ls ms ()        -> -- The lower braid to bundle
    Braid gs ls TID.ThreadId -- Thread in the higher braid
bundle name b =
  do { tid   <- genLifted
     ; putStrLn ( ">>> bundle: " ++ show tid ++
                  " // " ++ show (TID.firstSub tid) ++ 
                  " // " ++ show (TID.firstUnliftedSub tid) )
     ; start <- getElapsed
     ; let bst = mkBraidInitializer
                    name                        -- Name of the braid
-- ***** Do I want this, or just tid ??????
                    (TID.firstUnliftedSub tid)  -- Local tid of the braid
                    mkNull                      -- Init global state
                    start                       -- Starting clock tick
                    (BST.UnliftedProgram b)     -- First thread program (hub)
     ; forkWithTid name
                   tid
                   (BST.mkLiftedProg (twist bst (weave scheduleAndUpdate bst)))
     }

----------------------------------------------------------------------
-- Raising MVar ops (part of globalization)
--
-- When a braid is twisted into a lifted thread at a higher level,
-- the non-lifted MVar operations become lifted MVar operations in the
-- lifted thread belonging to the higher level braid.
----------------------------------------------------------------------

-- Raise MVar requests and responses from an encapsulate braid to
-- the lifted thread into which it has be twisted.
raise :: BraidSt ls ms -> LT.Thread ls (BraidSt ls ms)
raise bst =
  do { -- when (L.hasUpward (BST.locale bst))
--             ( LT.putStrLn ( "... BI.raise / not null: " ++ show (BST.local bst) ++
--                             "\n\t" ++ show (BST.locale bst ) ) )
     ; LT.raise (BST.locale bst)
     ; return (MVP.nullUpwardRelay bst)
     }

-- A lifted thread version of lower
lower :: BraidSt ls ms -> LT.Thread ls (BraidSt ls ms)
lower bst =
  do { locale' <- LT.lower (BST.locale bst)
     ; return (MVP.setLocale locale' bst)
     }

----------------------------------------------------------------------
--  In support of MVar Ops
----------------------------------------------------------------------
-- See if there are no MVarOps
-- isUpwardReq :: Braid gs ls Bool
-- isUpwardReq = observe MVP.isUpwardReq

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE liftThread          #-}
{-# INLINE liftSt              #-}
{-# INLINE lifta               #-}
{-# INLINE observe             #-}
{-# INLINE scheduleAndUpdate   #-}
{-# INLINE weave               #-}
{-# INLINE updateWeave         #-}
{-# INLINE prepareWeave        #-}
{-# INLINE bundle              #-}
{-# INLINE twist               #-}
{-# INLINE lower               #-}
{-# INLINE raise               #-}
{-# INLINE forkWithTid         #-}
{-# INLINE fork                #-}
{-# INLINE forkLift            #-}
{-# INLINE prepareThread       #-}
{-# INLINE updateThread        #-}
{-# INLINE localize            #-}
{-# INLINE threadDelay         #-}
{-# INLINE completeStateM      #-}
{-# INLINE lift                #-}
{-# INLINE liftma              #-}
{-# INLINE myThreadId          #-}
{-# INLINE pauseInState        #-}
{-# INLINE pauseSt             #-}
{-# INLINE killThread          #-}
{-# INLINE takeMVar            #-}
{-# INLINE putMVar             #-}
{-# INLINE newMVar             #-}
{-# INLINE deleteMVar          #-}
{-# INLINE dereferenceMVar     #-}
{-# INLINE genLifted           #-}
{-# INLINE genGlobal           #-}
{-# INLINE addThreadWithTid    #-}
{-# INLINE lRunM               #-}
{-# INLINE runM                #-}
{-# INLINE store               #-}
{-# INLINE fetch               #-}
{-# INLINE nullPause           #-}
{-# INLINE getElapsed          #-}
{-# INLINE getExpired          #-}
{-# INLINE mkBraid             #-}
{-# INLINE mkBraidInitializer  #-}
