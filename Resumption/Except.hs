-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Except ( Exception (..) ) where

-- Haskell imports
import Dynamic as DYN

data Exception
    = Message
    | DynException DYN.Dynamic
    | NotImplemented
    | Error
    deriving (Show)
