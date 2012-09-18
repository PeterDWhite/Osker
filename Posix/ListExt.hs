-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ListExt
    ( splitBy -- Split a list at the first element found with property
    , firstBy -- Find the first element with property
    ) where

----------------------------------------------------------------------
-- List extenstions for Osker
----------------------------------------------------------------------

-- split a list at the first element found having the specified property
-- The first element of the second list will be the element satisfying
-- the property. If there are no elements satisfying the property, the
-- first list will be the entire list, and the second list will be null.
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy _f [] = ([], [])
splitBy f (a:az) =
  if f a
  then ([], a:az)
  else let (pre, post) = splitBy f az
       in (a:pre, post)

-- Select the first in a list satsifying a predicate
firstBy :: (a -> Bool) -> [a] -> Maybe a
firstBy _p []     = Nothing
firstBy p (a:as)  = if p a then Just a else firstBy p as
