-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module PartialOrder
    ( PartialOrder (..)   -- Partial order class
    , Comparison (..)     -- Comparison results
    ) where

import Prelude hiding (compare, Ordering (..) )

-- Less than (LT), Greater than (GT), Equal (EQ), Non Comparable (NC)
data Comparison = LT | GT | EQ | NC deriving (Eq, Show)

class PartialOrder a where
    compare :: a -> a -> Comparison
    (===)   :: a -> a -> Bool
    x === y = compare x y == EQ
    (<=)    :: a -> a -> Bool
    x <= y  = compare x y == LT || compare x y == EQ
    (>=)    :: a -> a -> Bool
    x >= y  = compare x y == GT || compare x y == EQ
    (>)     :: a -> a -> Bool
    x > y   = compare x y == GT
    (<)     :: a -> a -> Bool
    x < y   = compare x y == LT
    -- Non comparable
    (<>)    :: a -> a -> Bool
    x <> y  = compare x y == NC
