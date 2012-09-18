-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Size ( Size (..) ) where

----------------------------------------------------------------------
-- The size type is used by many Posix calls
----------------------------------------------------------------------

newtype Size = Size Int

instance Show Size where
    show (Size n) = "Size:" ++ show n
