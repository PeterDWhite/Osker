-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ThreeSum
    ( ThreeSum (..)  -- A sum of three summands (in type theory)
    , first          -- Get the item of the first summand (if any)
    , second         -- Get the item of the second summand (if any)
    , third          -- Get the item of the third summand (if any)
    ) where

----------------------------------------------------------------------
-- A three way sum (extending Either, which is only a two way sum.
----------------------------------------------------------------------

data ThreeSum a b c
    = First a
    | Second b
    | Third c
    deriving (Eq, Ord, Show)

-- Extract from the first summand
first:: ThreeSum a b c -> a
first (First a) = a
first _ = error "ThreeSum/First"
-- Extract from the second summand
second:: ThreeSum a b c -> b
second (Second b) = b
second _ = error "ThreeSum/Second"
-- Extract from the third summand
third:: ThreeSum a b c -> c
third (Third c) = c
third _ = error "ThreeSum/Third"
