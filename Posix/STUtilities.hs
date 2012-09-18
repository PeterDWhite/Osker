-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module STUtilities
    ( listST
    , mapST
    ) where

----------------------------------------------------------------------
-- Utilities regarding the ST monad
----------------------------------------------------------------------

import ST

-- From John Launchbury's paper on state in Haskell
listST :: [ST s a] -> ST s [a]
listST sts = foldr conST nilST sts
  where
    nilST = return []
    conST m ms =
      do { r <- m
         ; rs <- ms
	 ; return (r:rs)
	 }

-- From John Launchbury's paper on state in Haskell
mapST :: (a -> ST s b) -> [a] -> ST s [b]
mapST f xs = listST (map f xs)
