-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module OskerException
    ( B.ThreadId        -- Re export Haskell stuff
    , B.Exception (..)  -- Re export Haskell stuff
    , B.throwTo         -- Our own version of throwing an exception
    , B.catch           -- Catch an exception
    ) where

----------------------------------------------------------------------
--  Enough stuff to serve as exceptions (real exceptions)
----------------------------------------------------------------------

import qualified BraidExternal as B
