-- Copyright (c) Peter Duncan White, 2003 -- Copyright (c) OHSU, 2003
module ThreadInfo
    ( ThreadState (..)     -- Type for thread state
    , isPausible           -- Is state valid to be in Paused list?
    , ThreadInfo (..)      -- Information stored about the thread
    , ProgramType (..)     -- Is thread lifted or Unlifted
    , isLifted             -- Determine if thread info is lifted
    , progType             -- Determine if thread info is lifted / unlifted
    , isLiftedP            -- Determine if program is lifted
    , Transformer          -- Transformer of the thread information
    , Observer             -- Observer of the thread information
    , Transerver           -- Transform thread and observe a value
    , getUnliftedProg      -- Get the Unlifted program
    , getLiftedProg        -- Get the lifted program
    , getWaitProg          -- Get a program waiting on a value
    , getLiftedState       -- Get the lifted state out of a thread info
    , updateLiftedState    -- Update lifted state in a thread info
    , updateLifted         -- Update lifted state and program
    , updateTid            -- Update thread state and program
    , transformLifted      -- Transform the lifted state
    , getProgram           -- Get program out of a thread info
    , getName              -- Get the program out of a thread info
    , updateProgram        -- Update program in a thread info
    , updateState          -- Update the thread state of the thread
    , getState             -- Get the state out of a thread info
    , getId                -- Get the thread id out of a thread info
    , getCatchers          -- Get the thread catcher out of a thread info
    , takeFirstCatcher     -- Take the first exception catcher
    , updateCatchers       -- Update the thread catcher of a thread info
    , addCatcher           -- Add a thread catcher
    , Mutable (..)         -- Mutable thread information
    , initMutable          -- Initialize thread mutable structure
    , Create (..)          -- Statically created part of thread info
    , initCreate           -- Initialize a thread create structure
    , Catcher              -- Type of thread catchers
    , Program (..)         -- Type of the thread program
    , mkUnliftedProg       -- Constructor for a Unlifted program
    , mkLiftedProg         -- Constructor for a lifted program
    ) where

----------------------------------------------------------------------
-- An abstract kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Dynamic (Dynamic)
import List
-- Utility imports
import Null
import Unsafe
-- Resumption imports
import qualified Ex as E
-- Braid imports
import qualified BraidMVar as MV
import qualified LiftedState as LS
import qualified LiftedThread as LT
import ThreadId (ThreadId)

data ThreadState
    = Running -- Currently not used
    | Paused  -- Paused, per the underlying resumption monad
      -- A wait completed, and produced a value
    | Completed       { value :: Dynamic }
    | Delayed    -- Delayed, for a threadDelay
      -- Waiting because the MVar is full
    | WaitingCuzFull  { wmvid :: MV.MVarId }
      -- Waiting because the MVar is empty
    | WaitingCuzEmpty { wmvid :: MV.MVarId }

instance Null ThreadState where
    mkNull = Paused

instance Eq ThreadState where
    Running               == Running               = True
    Paused                == Paused                = True
    Completed _           == Completed _           = True
    Delayed               == Delayed               = True
    WaitingCuzFull mvid1  == WaitingCuzFull mvid2  = mvid1 == mvid2
    WaitingCuzEmpty mvid1 == WaitingCuzEmpty mvid2 = mvid1 == mvid2
    _ == _ = False

instance Show ThreadState where
    show Running               = "Running"
    show Paused                = "Paused"
    show (Completed v)         = "Completed, v = " ++ show v
    show Delayed               = "Delayed"
    show (WaitingCuzFull mid)  = "WaitingCuzFull (" ++ show mid ++ ")"
    show (WaitingCuzEmpty mid) = "WaitingCuzEmpty (" ++ show mid ++ ")"

-- Determine if a state is pausible
isPausible :: ThreadState -> Bool
isPausible Paused        = True
isPausible (Completed _) = True
isPausible _             = False

type Catcher t ls = E.Exception -> Program t ls

-- An exception catcher, having a Unlifted or a lifted program
data Program t ls
    = UnliftedProgram t
    | LiftedProgram (LT.Program ls)
    | WaitProgram (Dynamic -> t)

-- Get the Unlifted program out of the Program type
getUnliftedProg :: Program t ls -> t
getUnliftedProg (UnliftedProgram t) = t
getUnliftedProg (LiftedProgram _)   = error "getUnliftedProg/Lifted"
getUnliftedProg (WaitProgram _)     = error "getUnliftedProg/Wait"

-- Get a program waiting on a value
getWaitProg :: Program t ls -> (Dynamic -> t)
getWaitProg (UnliftedProgram _) = error "getWaitProgram/Unlifted"
getWaitProg (LiftedProgram _)   = error "getWaitProgram/Lifted"
getWaitProg (WaitProgram p)     = p

-- Make a Unlifted program
mkUnliftedProg :: t -> Program t ls
mkUnliftedProg  = UnliftedProgram

-- Make a lifted program
mkLiftedProg :: LT.Program ls -> Program t ls
mkLiftedProg  = LiftedProgram

-- Get the lifted program out of the Program type
getLiftedProg :: Program t ls -> LT.Program ls
getLiftedProg (UnliftedProgram _) = error "getLiftedProg / Unlifted"
getLiftedProg (LiftedProgram t)  = t
getLiftedProg (WaitProgram _)   = error "getLiftedProg / Wait"

-- Get the program type
isLiftedP :: Program t ls -> ProgramType
isLiftedP (LiftedProgram _)   = Lifted
isLiftedP (UnliftedProgram _) = Unlifted
isLiftedP (WaitProgram _)     = Wait
-- Determine if program is lifted
liftedP :: Program t ls -> Bool
liftedP (LiftedProgram _)   = True
liftedP (UnliftedProgram _) = False
liftedP (WaitProgram _)     = False

instance (Null t) => Null (Program t ls) where
    mkNull = UnliftedProgram mkNull

----------------------------------------------------------------------
-- The information stored by a braid about a thread.
--
-- At this level we do not commit to a particular kind of
-- thread program to run.
-- We currently permit only a single exception catcher
-- per thread, no nesting of catchers (yet). Furthermore, the
-- return type of the catcher must be (), i.e. the unit type
----------------------------------------------------------------------

-- First the mutable information about the thread, i.e. information
-- that can be changed by the thread as a result of legitimate
-- operations by the thread itself.
-- The type of the lifted state is a parameter to the thread
-- mutable state
data Mutable t ls =
    Mutable
    { state       :: ThreadState       -- Execution state of the thread
    , program     :: Program t ls      -- Program to execute in thread
    , catchers    :: [ Catcher t ls ]  -- Exception catcher for thread
    , liftedState :: LT.LiftedSt ls    -- Lifted state of the thread
    }

instance (Null t, Null ls) => Null (Mutable t ls) where
      mkNull = Mutable { state       = mkNull
                       , program     = mkNull
                       , catchers    = mkNull
                       , liftedState = mkNull
                       }

instance Show (Mutable t ls) where
    show m = show (length (catchers m))

-- Initialize a thread mutable structure
initMutable :: Program t ls -> LT.LiftedSt ls -> Mutable t ls
initMutable prog ls = Mutable { state       = Paused
                              , program     = prog
                              , catchers    = []
                              , liftedState = ls
                              }

-- Now the information that cannot be changed by the thread, but
-- is affected by another thread that forks the thread, i.e.
-- information that is set when the thread is created and then
-- never changed by any legitimate thread operation after the
-- thread is created.
data Create = Create { tid     :: ThreadId -- Stored here for convenience
                     , name    :: String   -- For debugging only
                     }

instance Null Create where
    mkNull = Create { tid   = mkNull, name = mkNull }

-- Initialize a thread mutable structure
initCreate :: ThreadId -> String -> Create
initCreate = Create

-- All the thread information (creation and mutable) in one bundle.
-- A thread can be lifted (created through lift), or Unlifted (created
-- via a fork)
data ThreadInfo t ls
    = ThreadInfo { create  :: Create       -- Changed during thread creation
                 , mutable :: Mutable t ls -- Changed by thread ops
                 }

instance (Null t, Null ls) => Null (ThreadInfo t ls) where
    mkNull = ThreadInfo { mutable = mkNull, create   = mkNull }

-- A data type for program identification
data ProgramType = Lifted | Unlifted | Wait deriving (Eq, Show)

instance Null ProgramType where
    mkNull = Unlifted

instance Show (ThreadInfo t ls) where
    show tinfo  = "/TI " ++ show (mutable tinfo) ++ " TI/"

-- A transformer of the thread information
type Transformer t ls   = ThreadInfo t ls -> ThreadInfo t ls
type Observer t ls a    = ThreadInfo t ls -> a
type Transerver t ls a  = ThreadInfo t ls -> (ThreadInfo t ls, a)

-- Determine if thread info is lifted or Unlifted
isLifted :: ThreadInfo t ls -> Bool
isLifted = liftedP . program . mutable

-- Get the program type of the thread information
progType :: ThreadInfo t ls -> ProgramType
progType = isLiftedP . program . mutable

----------------------------------------------------------------------
-- Some accessors for ThreadInfo
----------------------------------------------------------------------

-- Get the lifted state
getLiftedState :: ThreadInfo t ls -> LT.LiftedSt ls
getLiftedState = liftedState . mutable

-- Update the thread program for a thread info
updateLiftedState :: LT.LiftedSt ls -> Transformer t ls
updateLiftedState ls tinfo@(ThreadInfo { mutable = m } ) =
  tinfo { mutable = m { liftedState = ls } }

-- Update the state and the program
updateLifted :: LT.LiftedSt ls -> LT.Thread ls () -> Transformer t ls
updateLifted ls prog tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { liftedState = ls, program    = LiftedProgram prog } }

-- Transform the lifted state
transformLifted :: LT.Transformer ls -> Transformer t ls
transformLifted f tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { liftedState = f (liftedState m) } }

-- Update the thread state
updateState :: ThreadState -> Transformer t ls
updateState s tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { state = s }}

-- Update the thread state and the program
updateTid :: ThreadState -> Program t ls -> Transformer t ls
updateTid s prog tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { state = s, program = prog } }

-- Get the program out of a thread info
getName :: Observer t ls String
getName = name . create

-- Get the program out of a thread info
getProgram :: Observer t ls (Program t ls)
getProgram  = program . mutable

-- Update the thread program for a thread info
updateProgram :: Program t ls -> Transformer t ls
updateProgram t tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { program=t } }

-- Get the state from a thread info
getState :: Observer t ls ThreadState
getState = state . mutable

-- Get the thread Id out of a threadInfo
getId :: Observer t ls ThreadId
getId = tid . create

-- Get the thread cather out of a thread info
getCatchers :: ThreadInfo t ls -> [ Catcher t ls ]
getCatchers = catchers . mutable

-- Get the first thread catcher
-- This also updates the program to the catcher. This is always
-- part of the exception processing, in support of the throw
-- primitives.
-- In the unlifted case, we must execute the program retrieved as
-- the catcher, as well as store it in the program portion of the
-- state.
-- In the lifted case, we need only store the new program in the state,
-- and this program will be retrieved before the next step is executed.
takeFirstCatcher :: E.Exception -> Transerver t ls (Maybe (Program t ls))
takeFirstCatcher e tinfo@(ThreadInfo { mutable = m }) =
  if isLifted tinfo
  then let ls              = getLiftedState tinfo
           (ls', mcatcher) = LS.takeFirstCatcher ls
       in case mcatcher of
            Nothing       -> ( tinfo, Nothing )
            Just lcatcher ->
              ( tinfo { mutable = m { liftedState = ls'
                                    , program = LiftedProgram (lcatcher e)
                                    }
                      }
              , Nothing
              )
  else if null (catchers m)
       then ( unsafeRet ( "... TI.takeFirstCatcher.1" ) tinfo
            , Nothing
            )
       else let newProg = head (catchers m) e
                tinfo' = tinfo { mutable = m { catchers = tail (catchers m)
                                             , program  = newProg
                                             }
                               }
            in ( unsafeRet ( "... TI.takeFirstCatcher.2: " ++ show tinfo ++ show tinfo' )
                         tinfo'
               , Just newProg
               )

-- Update the thread catcher for a thread info
updateCatchers :: [ Catcher t ls ] -> Transformer t ls
updateCatchers cs tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { catchers = cs } }

-- Update the thread catcher for a thread info
addCatcher :: Catcher t ls -> Transformer t ls
addCatcher c tinfo@(ThreadInfo { mutable = m }) =
  tinfo { mutable = m { catchers = c:(catchers m) } }

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE getUnliftedProg     #-}
{-# INLINE mkUnliftedProg      #-}
{-# INLINE mkLiftedProg        #-}
{-# INLINE getLiftedProg       #-}
{-# INLINE getWaitProg         #-}
{-# INLINE isLiftedP           #-}
{-# INLINE liftedP             #-}
{-# INLINE initMutable         #-}
{-# INLINE initCreate          #-}
{-# INLINE isLifted            #-}
{-# INLINE progType            #-}
{-# INLINE getLiftedState      #-}
{-# INLINE updateLiftedState   #-}
{-# INLINE updateLifted        #-}
{-# INLINE transformLifted     #-}
{-# INLINE updateState         #-}
{-# INLINE getName             #-}
{-# INLINE getProgram          #-}
{-# INLINE updateProgram       #-}
{-# INLINE getState            #-}
{-# INLINE getId               #-}
{-# INLINE getCatchers         #-}
{-# INLINE updateCatchers      #-}
{-# INLINE addCatcher          #-}
