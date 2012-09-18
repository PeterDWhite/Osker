-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Threads
    ( ThreadInit (..)      -- Evertyting needed to initialize a thread
    , progType             -- Is thread init for lifted or global thread
    , Threads              -- Map thread Id to thread information
    , delThread            -- Remove a thread from the collection
    , addThread            -- Add a new thread to the collection
    , isLifted             -- Determine if thread is lifted or global
    , lengthThreads        -- Find out how many threads there are
    , threadSet            -- Get the thread Ids from collection of threads
    , listify              -- Get the list of threads
    , mapThreads           -- Map a function over the threads, return a list
    , filterThreads        -- Filter a thread set by a condition
    , size                 -- Size of a thread set
    , lookup               -- Get information about a thread (assumes live)
    , update               -- Update information about a thread Id
    , updateThreadState    -- Update only the thread state
    , mkNull               -- Make null constructor visible
    , Null                 -- Make null class visible
    , TI.Catcher           -- Type of thread catchers
    , outThreadsTight      -- Nice print out for threads
    , initThreadInfo       -- Initialize a thread info
      -- Re-export threadinfo stuff
    , TI.ThreadInfo        -- Information about a thread
    , TI.ProgramType (..)  -- Is thread lifted or global
    ) where

----------------------------------------------------------------------
-- An abstract kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (lookup)
import FiniteMap ( FiniteMap, emptyFM, eltsFM, keysFM, addToFM
                 , delFromFM, lookupFM, filterFM, sizeFM )
import SimpleQueue (fromList)
import Maybe
import List hiding (lookup)
-- Utility imports
import Null
-- Braid imports
import qualified ThreadId as TID
import qualified ThreadInfo as TI
import qualified LiftedState as LS
import qualified LiftedThread as LT

-- Map thread Id to thread information
data Threads t ls =
    Threads { proj :: FiniteMap (TID.ThreadId) (TI.ThreadInfo t ls)}

instance Show (Threads t ls) where
    show ts = outThreadsTight ts

instance Null (Threads t ls) where
    mkNull = Threads emptyFM

-- Compact output for threads
outThreadsTight :: Threads t ls -> String
outThreadsTight ts =
  let elts = eltsFM (proj ts)
      outElems :: [TI.ThreadInfo t ls] -> String
      outElems [] = ""
      outElems [e] = outElem e
      outElems (e:es) = outElem e ++ "\n\t" ++ outElems es
      outElem :: TI.ThreadInfo t ls -> String
      outElem tinfo =
        let tid   = TI.getId tinfo
            state = TI.getState tinfo
            ls    = TI.getLiftedState tinfo
            cs    = TI.getCatchers tinfo
        in "/" ++ TI.getName tinfo ++ "=" ++ show tid ++
           " -> " ++ show state ++ " / " ++ show ls ++
           " / " ++ show (length cs) ++ " /"
  in "\n\t" ++ outElems elts

-- Lookup thread information.
lookup :: Threads t ls -> TID.ThreadId -> Maybe (TI.ThreadInfo t ls)
lookup (Threads threads) tid = lookupFM threads tid

-- Lookup thread information, do not accept no for an answer
lookupHard :: Threads t ls -> TID.ThreadId -> TI.ThreadInfo t ls
lookupHard (Threads threads) tid =
  case lookupFM threads tid of
    Nothing    -> error ("lookupHard: " ++ show tid)
    Just tinfo -> tinfo

-- Determine if thread is lifted or not
isLifted :: Threads t ls -> TID.ThreadId -> Bool
isLifted threads tid = TI.isLifted (lookupHard threads tid)

-- Everything needed to initialize a thread information structure
data ThreadInit t ls = ThreadInit { name        :: String
                                  , program     :: TI.Program t ls
                                  , liftedState :: LT.LiftedSt ls
                                  }

-- Initialize a lifted thread info structure
initThreadInfo :: TID.ThreadId -> ThreadInit t ls -> TI.ThreadInfo t ls
initThreadInfo tid (ThreadInit {name = n, program = p, liftedState = s}) =
  TI.ThreadInfo { TI.mutable = TI.initMutable p s
                , TI.create  = TI.initCreate tid n
                }

-- Determine if thread init is for lifted or global thread
progType :: ThreadInit t ls -> TI.ProgramType
progType = TI.isLiftedP . program

-- Get the threadIds from a collection of threads
threadSet :: Threads t ls -> TID.ThreadSet
threadSet = TID.ThreadSet . fromList . keysFM . proj

-- Update the thread information
update ::
    Threads t ls       -> -- Initial collection of threads
    TID.ThreadId       -> -- Id of thread to update
    TI.ThreadInfo t ls -> -- New info for the thread
    Threads t ls          -- Final collection of threads
update (Threads threads) tid tinfo = Threads ( addToFM threads tid tinfo )

-- Update the only the thread state
updateThreadState ::
    Threads t ls   -> -- Initial collection of threads
    TID.ThreadId   -> -- Thread id of thread to update
    TI.ThreadState -> -- New state of the thread
    Threads t ls      -- Final collection of threads
updateThreadState ts@(Threads _threads) tid state =
  let tinfo = lookupHard ts tid
  in seq tinfo (update ts tid (TI.updateState state tinfo))

-- Get the list of threads
listify :: Threads t ls -> [TI.ThreadInfo t ls]
listify = eltsFM . proj

-- Remove a thread from the collection of threads
delThread :: Threads t ls -> TID.ThreadId -> Threads t ls
delThread (Threads threads) tid = Threads ( delFromFM threads tid )

-- Add a thread to the collection, or update a thread in the collection
addThread ::
    Threads t ls     -> -- The initial collection of threads
    TID.ThreadId     -> -- The thread id of thread to add
    ThreadInit t ls  -> -- Init info for the thread to add
    Threads t ls        -- The final collection of threads
addThread (Threads threads) tid tinit =
  Threads (addToFM threads tid (initThreadInfo tid tinit))

-- Determine the length of the collection of threads
lengthThreads :: Threads t ls -> Int
lengthThreads = length . keysFM . proj

-- Map a function over a list of threads
mapThreads :: (TI.ThreadInfo t ls -> a) -> Threads t ls -> [a]
mapThreads f (Threads threads) = map f (eltsFM threads)

-- NC: Filter a thread set
filterThreads ::
    (TID.ThreadId -> TI.ThreadInfo t ls -> Bool) -> -- filter
    Threads t ls                                 -> -- filtrand
    Threads t ls                                    -- filtrate
filterThreads f (Threads threads) = Threads (filterFM f threads)

-- NC: Size of a threadset
size :: Threads t ls -> Int
size (Threads ts) = sizeFM ts

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE lookup            #-}
{-# INLINE lookupHard        #-}
{-# INLINE isLifted          #-}
{-# INLINE initThreadInfo    #-}
{-# INLINE progType          #-}
{-# INLINE threadSet         #-}
{-# INLINE update            #-}
{-# INLINE updateThreadState #-}
{-# INLINE listify           #-}
{-# INLINE delThread         #-}
{-# INLINE addThread         #-}
{-# INLINE lengthThreads     #-}
{-# INLINE mapThreads        #-}
{-# INLINE filterThreads     #-}
{-# INLINE size              #-}
