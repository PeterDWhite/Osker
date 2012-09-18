-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module BufferMode ( BufferMode (..) ) where

----------------------------------------------------------------------
-- Buffering modes, controlling IO devices such as the file system
----------------------------------------------------------------------

data BufferMode = IOFBF -- Full buffering
                | IOLBF -- Line buffering
                | IONBF -- No buffering
                deriving (Eq, Ord, Enum, Show)
