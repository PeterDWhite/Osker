-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module NodeName ( NodeName (..) ) where

-- Haskell imports
import List
-- Utility imports
import qualified Null as N

class (Eq n, Ord n) => NodeName n where
    nnDeepen       :: n -> n
    nnFlatten      :: n -> n -> n
    -- Determine the greatest common prefix to two node names
    nnCommonPrefix :: n -> n -> n
    -- The first sub node name of the input node name
    nnFirstSub     :: n -> n
    -- The first useable node name
    nnFirst        :: n
    -- A null node name
    nnNull         :: n
    nnDrop         :: Int -> n -> n
    nnTake         :: Int -> n -> n
    nnInit         :: n -> n
    nnLast         :: n -> n
    nnConcat       :: n -> n -> n
    nnLength       :: n -> Int
    isNnNull       :: n -> Bool
    isNnNull n = n == nnNull
    nnIsPrefixOf   :: n -> n -> Bool
    nnTail         :: n -> n
    nnHead         :: n -> n

instance (Eq a, Ord a, N.Null a) => NodeName [a] where
    nnDeepen bs           = [last bs]
    nnFlatten context bs  = context ++ bs
    nnCommonPrefix _xs [] = []
    nnCommonPrefix [] _ys = []
    nnCommonPrefix (x:xs) (y:ys) =
      if x == y 
      then x:(nnCommonPrefix xs ys)
      else nnCommonPrefix xs ys
    nnFirstSub xs = xs ++ [N.mkNull]
    nnFirst       = [N.mkNull]
    nnNull        = []
    nnDrop        = drop
    nnTake        = take
    nnInit        = init
    nnLast l      = [last l]
    nnConcat      = (++)
    nnLength      = length
    nnIsPrefixOf  = isPrefixOf
    nnTail        = tail
    nnHead l      = [head l]

instance NodeName () where
    nnDeepen _         = ()
    nnFlatten _ _      = ()
    nnCommonPrefix _ _ = ()
    nnFirstSub _       = ()
    nnFirst            = ()
    nnNull             = ()
    nnDrop _ ()        = ()
    nnTake _ ()        = ()
    nnInit ()          = ()
    nnLast ()          = ()
    nnConcat () ()     = ()
    nnLength ()        = 0
    nnIsPrefixOf () () = True
    nnTail ()          = ()
    nnHead ()          = ()
