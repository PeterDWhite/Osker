-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Policy
    ( Policy                  -- A relation on threadIds
    , Schedule                -- An order of threads to schedule
    , intransitiveInputPurge  -- Intransitive purge on the input side
    , intransitiveOutputPurge -- Intransitive purge on the output side
    , Observation (..)        -- An observation of a system step
    ) where

----------------------------------------------------------------------
--  Definition of a communication policy
----------------------------------------------------------------------

-- Haskell imports
import List
-- Utility imports
-- Local imports
import qualified Relation as R
import qualified LiftedThread as LT
import qualified ThreadId as TID

-- A policy determines which threads can interfere with which.
-- It is represented as a relation on thread Ids.
data Policy = Policy { proj :: R.Relation TID.ThreadId }

instance Show Policy where
    show (Policy _tid) = "Policy*"

{-P: {-< Communication policy properties >-}
-- Communication policies must be reflexive
property IsValidPolicy =
  {| sp :: Policy | All d :: Domain. { sp d d } === True |}
 -}

-- A schedule is a potentially infinite list of thread Ids
type Schedule = [TID.ThreadId] -- Potentially infinite

-- Our sources function (Rushby, p24)
-- Note that our function sometimes returns bottom!
-- [Rushby]: When v is in (sources alpha u), it means either that
-- v = u, or that there is a subsequence of alpha consisting of
-- actions performed by domains w1, ... wn such that w1 interferes
-- with w2, w2 interferes with w3, .... all the way to u = wn.
-- "Interfere" means "directly interfere".
-- The definition of sources assumes that policies are valid, i.e.
-- that they are reflexive. Without this assumption, the first
-- clause of the definition falls over.
sources :: Schedule -> Policy -> TID.ThreadId -> Schedule
sources [] _policy tid = [tid]
sources (a:as) policy tid =
  if any (\v -> elem v (sources as policy tid) && ((proj policy) a v))
         (sources as policy tid)
  then sources as policy tid `union` [tid]
  else sources as policy tid

-- Define the intransitive purge function (Rushby p25).
-- This is an intransitive purge on the inputs
intransitiveInputPurge :: Schedule -> Policy -> TID.ThreadId -> Schedule
intransitiveInputPurge [] _policy _tid = []
intransitiveInputPurge (a:as) policy tid =
  if elem tid (sources (a:as) policy tid)
  then a:intransitiveInputPurge as policy tid
  else intransitiveInputPurge as policy tid

-- An observation is a thread Id paired with the lifted state of
-- that thread Id in the braid.
data Observation ls =
    Observation { oid :: TID.ThreadId
                , ols :: LT.LiftedSt ls
                }

-- Define the intransitive purge function (Rushby p25).
-- This is an intransitive purge on the inputs
intransitiveOutputPurge ::
    Policy           -> -- Purge according to this policy
    TID.ThreadId     -> -- The thread id to purge
    [Observation ls] -> -- The observations to purge from
    [Observation ls]    -- The purged observations
intransitiveOutputPurge _policy _tid [] = []
intransitiveOutputPurge policy tid obs@(o:os) =
  let tids = map oid obs
  in if elem tid (sources tids policy tid)
     then o:intransitiveOutputPurge policy tid os
     else intransitiveOutputPurge policy tid os
