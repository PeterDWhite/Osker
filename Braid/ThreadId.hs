-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module ThreadId
    ( ThreadId (..)    -- Data type for thread Id **** TEMP TEMP
    , Locality         -- Locality of a thread
    , locality         -- Determine locality of thread wrt a braid
    , mkLifted         -- Constructor for ThreadId
    , mkUnlifted       -- Constructor for ThreadId
    , isLifted         -- Check if thread id is local
    , noTid            -- Check if tid is noTid
    , firstUnlifted    -- First global tid of a thread
    , firstLifted      -- First local tid of a thread
    , firstUnliftedSub -- First global sub tid of a thread
    , firstLiftedSub   -- First local sub tid of a thread
    , firstSub         -- First sub tid of a tid
    , bumpTid          -- Get next local or global tid
    , ThreadSet (..)   -- A collection of thread Ids
    , addThreadSet     -- Add thread id to set (at the end), only if not null
    , delThreadSet     -- Delete a thread from the thread set
    , fromList         -- Convert list of tids to thread set
    , toList           -- Convert a threadset to a list
    , size             -- Get the size of a thread set
    , nullTS           -- Determine if thread set is null
    , headTS           -- Get the head of a thread set
    , tailTS           -- Get the tail of a thread set
    , elemTS           -- Determine if tid is a member
    , atLevel          -- Determine immediate subordinate relationship
    , isSub            -- Determine subordinate relationship
    , isStrictSub      -- Determine strict subordinate relationship
    , level            -- Compare the level of a thread to level of a braid
    ) where

-- Haskell imports
import List
import qualified SimpleQueue as SQ
-- Utility imports
import qualified PartialOrder as PO
import Null
-- Graph imports
import qualified NodeName as NN

----------------------------------------------------------------------
-- A thread Id is a renaming of the class NodeName, from the Graph
-- library.
----------------------------------------------------------------------

-- A thread Id can name a lifted or a unlifted thread
data ThreadId = NoTid | Lid [Int] | Uid [Int] deriving (Eq, Ord, Show)

instance Null ThreadId where
    mkNull = NoTid

-- A data type for local and global identification
-- A lifted thread is at the level of the braid, generated by liftThread
-- A non-lifted thread is at the level of the braid,
-- not generated by liftThread
-- A non-local thread is not at the level of the braid
data Locality = Lifted | Unlifted | NonLocal deriving (Eq, Show)

-- Determine the locality of a thread with respect to a braid
locality :: ThreadId -> ThreadId -> Locality
locality thread braid =
  if atLevel braid thread
  then case thread of
         Lid _ -> Lifted
         Uid _ -> Unlifted
         NoTid -> error ( "TID.locality")
  else NonLocal

instance Null Locality where
    mkNull = Unlifted

-- Get the length of the thread Id
longth :: ThreadId -> Int
longth NoTid = 0
longth (Lid ns) = length ns
longth (Uid ns) = length ns

-- Increment the Id portion of a Thread Id
bumpId :: [Int] -> [Int]
bumpId []          = [mkNull]
bumpId [tid]       = [tid + 1]
bumpId (tid:tids') = tid:bumpId tids'

-- Increment the id in a thread id
bumpTid :: ThreadId -> ThreadId
bumpTid NoTid = NoTid
bumpTid (Lid tids) = Lid (bumpId tids)
bumpTid (Uid tids) = Uid (bumpId tids)

-- Put a function inside the thread id constructor
in1 :: ([Int] -> [Int]) -> ThreadId -> ThreadId
in1 _f NoTid = NoTid
in1 f (Lid ns) = Lid (f ns)
in1 f (Uid ns) = Uid (f ns)

-- Get the first sub id of an id.
firstSub :: ThreadId -> ThreadId
firstSub = in1 NN.nnFirstSub

-- Convert thread Id to local
toLifted :: ThreadId -> ThreadId
toLifted = Lid . proj

-- Convert thread Id to local
toUnlifted :: ThreadId -> ThreadId
toUnlifted = Uid . proj

-- Get the first local sub thread id
firstLiftedSub :: ThreadId -> ThreadId
firstLiftedSub = toLifted . firstSub

-- Get the first local sub thread id
firstUnliftedSub :: ThreadId -> ThreadId
firstUnliftedSub = toUnlifted . firstSub

-- Get the first local id
firstLifted :: ThreadId
firstLifted = Lid NN.nnFirst

-- Get the first global id
firstUnlifted :: ThreadId
firstUnlifted = Uid NN.nnFirst

-- Constructors
mkLifted :: [Int] -> ThreadId
mkLifted = Lid
mkUnlifted :: [Int] -> ThreadId
mkUnlifted = Uid

-- Project out the tid
proj :: ThreadId -> [Int]
proj NoTid = []
proj (Lid ns) = ns
proj (Uid ns) = ns

-- See if tid is null
isNull :: ThreadId -> Bool
isNull = null . proj

-- See if tid is NoTid
noTid :: ThreadId -> Bool
noTid NoTid = True
noTid _     = False

-- See if tid is local
isLifted :: ThreadId -> Bool
isLifted NoTid     = False
isLifted (Lid _ns) = True
isLifted (Uid _ns) = False

-- Determine if the second thread id is for an immediate subordinate of
-- the second first id. A thread (say Uid [0,0] or Lid [0,1] is at the
-- level of a braid (say Uid [0]) when the prefix of the thread is
-- the same as the braid Id, and the threadId has one additional
-- component, i.e. the thread Id is the braid Id with another number
-- tacked onto the end.
atLevel :: ThreadId -> ThreadId -> Bool
atLevel x y = y PO.< x && longth x + 1== longth y

-- Determine if the second thread id is a subordinate of the first
isSub :: ThreadId -> ThreadId -> Bool
isSub x y = y PO.<= x

-- Determine if the second thread id is a subordinate of the first
isStrictSub :: ThreadId -> ThreadId -> Bool
isStrictSub x y = y PO.< x

-- x <= y when y is a prefix of x
instance PO.PartialOrder ThreadId where
    compare x y =
      if x == y
      then PO.EQ
      else if isPrefixOf (proj y) (proj x)
           then PO.LT
           else if isPrefixOf (proj x) (proj y)
                then PO.GT
                else PO.NC

-- Compare the level of a thread to the level of a braid
-- The thread is at the same level as the braid when it has the
-- same prefix, with exactly one more component added on.
level :: ThreadId -> ThreadId -> PO.Comparison
level braid thread =
  if braid PO.>= thread
  then if longth thread == longth braid + 1
       then PO.EQ -- Thread is at same level as the braid
       else PO.LT -- Thread is less than the braid
  else if braid PO.< thread
       then PO.GT -- Thread is greater than the braid
       else PO.NC -- Thread is non comparable to the braid

-- For comparisons such as a finite map, where any order will do
--instance (Eq tid, Null tid) => Ord ThreadId where
--    x <= y = isPrefixOf (proj x) (proj y)

-- A collection of threads
data ThreadSet = ThreadSet (SQ.Seq ThreadId)

-- Project out from the thread set
projTS :: ThreadSet -> SQ.Seq ThreadId
projTS (ThreadSet ts) = ts

instance Null ThreadSet where
    mkNull = ThreadSet SQ.empty

instance Show ThreadSet where
    show ts = show (toList ts)

-- Convert list to thread set
fromList :: [ThreadId] -> ThreadSet
fromList = ThreadSet . SQ.fromList

-- Convert a thread set to a list
toList :: ThreadSet -> [ThreadId]
toList = SQ.toList . projTS

-- Get the size of a thread set
size :: ThreadSet -> Int
size = SQ.size . projTS

-- Determine if the thread set is null
nullTS :: ThreadSet -> Bool
nullTS = SQ.null . projTS

-- Get the head of a thread set
headTS :: ThreadSet -> ThreadId
headTS = SQ.lhead . projTS

-- Get the head of a thread set
tailTS :: ThreadSet -> ThreadSet
tailTS = ThreadSet . SQ.ltail . projTS

-- Check if element of a thread set
elemTS :: ThreadId -> ThreadSet -> Bool
elemTS tid (ThreadSet ts) = SQ.size (SQ.filter ((==) tid) ts) >= 1

seqOr :: SQ.Seq Bool -> Bool
seqOr = SQ.foldr (||) False

seqAny :: (a -> Bool) -> SQ.Seq a -> Bool
seqAny p = seqOr . SQ.map p

seqElem :: (Eq a) => a -> SQ.Seq a -> Bool
seqElem = seqAny . (==)

-- Add a thread id to a thread set, only if not null
-- Adds to the end
addThreadSet :: ThreadSet -> ThreadId -> ThreadSet
addThreadSet (ThreadSet ts) tid =
  if isNull tid
  then ThreadSet ts
  else if seqElem tid ts
       then ThreadSet ts
       else ThreadSet (SQ.snoc ts tid)

-- Delete a thread from a thread set
-- Looks like a bad choice was made in using simple queue, may
-- want to change it back to a list.
delThreadSet :: ThreadSet -> ThreadId -> ThreadSet
delThreadSet (ThreadSet ts) tid =
  let (_remove, leave) = SQ.partition (\t -> t == tid) ts
  in ThreadSet leave

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkLifted         #-}
{-# INLINE mkUnlifted       #-}
{-# INLINE isLifted         #-}
{-# INLINE proj             #-}
{-# INLINE isNull           #-}
{-# INLINE projTS           #-}
{-# INLINE fromList         #-}
{-# INLINE toList           #-}
{-# INLINE size             #-}
{-# INLINE nullTS           #-}
{-# INLINE headTS           #-}
{-# INLINE tailTS           #-}
{-# INLINE elemTS           #-}
{-# INLINE seqOr            #-}
{-# INLINE seqAny           #-}
{-# INLINE seqElem          #-}
{-# INLINE addThreadSet     #-}
{-# INLINE delThreadSet     #-}
{-# INLINE toLifted         #-}
{-# INLINE toUnlifted       #-}
{-# INLINE firstLiftedSub   #-}
{-# INLINE firstUnliftedSub #-}
{-# INLINE noTid            #-}
{-# INLINE level            #-}
{-# INLINE isSub            #-}
{-# INLINE isStrictSub      #-}
{-# INLINE atLevel          #-}
