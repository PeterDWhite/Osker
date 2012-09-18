-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Whence ( Whence (..) ) where -- Posix offset from whence

----------------------------------------------------------------------
-- Posix offset from whence, i.e. a base for an offset
----------------------------------------------------------------------

-- Local imports
data Whence
    = SeekSet -- Base is beginning of file
    | SeedEnd -- Set position to end of file
    | SeekCur -- Base is the current position
    deriving (Eq, Ord, Enum, Show)
