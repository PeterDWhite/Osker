-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Name
    ( Name (..)      -- Data type for compound names
    , projName       -- Get the underlying components
    , projNameString -- Get the showable name
    ) where

----------------------------------------------------------------------
-- The POSIX file name specification.
-- Also used by other operations, such as POSIX.4 message queue.
----------------------------------------------------------------------

-- Haskell imports
import List

-- A generic name is a list of components, each component is a string
data Name = Name [String] deriving (Eq, Ord)

-- Get the underlying components
projName :: Name -> [String]
projName (Name s) = s

-- Get the underlying name
projNameString :: Name -> String
projNameString (Name s) = show s

instance Show Name where
    show (Name n) = "/" ++ concat (intersperse "/" n)
