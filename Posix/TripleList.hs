-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module TripleList
    ( TripleList      -- A triple of lists
    , mkTripleList    -- Constructor for triple list
    , first           -- The first component of a triple
    , second          -- The second component of a triple
    , third           -- The third component of a triple
    , append          -- Append two triple lists
    , nullTripleList  -- Null value for a triple list
    , updateFirst     -- Update the first list component
    , updateSecond    -- Update the second list component
    , updateThird     -- Update the third list component
    ) where

-- Local imports
import qualified Triple as T
import qualified Null as N

----------------------------------------------------------------------
-- Utilities regarding a triple of lists
----------------------------------------------------------------------

data TripleList a b c = TripleList (T.Triple [a] [b] [c])

mkTripleList :: [a] -> [b] -> [c] -> TripleList a b c
mkTripleList as bs cs = TripleList (T.Triple as bs cs)

proj :: TripleList a b c -> T.Triple [a] [b] [c]
proj (TripleList x) = x

first :: TripleList a b c -> [a]
first = T.first . proj
second :: TripleList a b c -> [b]
second = T.second . proj
third :: TripleList a b c -> [c]
third = T.third . proj
updateFirst :: TripleList a b c -> [d] -> TripleList d b c
updateFirst tl ds =  TripleList (T.updateFirst (proj tl) ds)
updateSecond :: TripleList a b c -> [d] -> TripleList a d c
updateSecond tl ds =  TripleList (T.updateSecond (proj tl) ds)
updateThird :: TripleList a b c -> [d] -> TripleList a b d
updateThird tl ds =  TripleList (T.updateThird (proj tl) ds)

-- append two triple lists
append :: TripleList a b c -> TripleList a b c -> TripleList a b c
append t1 t2 =
  TripleList (T.Triple (first t1 ++ first t2)
                       (second t1 ++ second t2)
                       (third t1 ++ third t2))

-- A null value for a triple list
nullTripleList :: TripleList a b c
nullTripleList = TripleList (T.Triple [] [] [])

instance N.Null (TripleList a b c) where
    mkNull = nullTripleList
