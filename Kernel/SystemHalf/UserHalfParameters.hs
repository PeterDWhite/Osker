-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module UserHalfParameters
    (  UserHalfParameters (..)
    ) where

----------------------------------------------------------------------
-- The parameters to the system half IO shell that describe the user
-- half
----------------------------------------------------------------------

-- Posix imports
import qualified SystemCall as SC

data UserHalfParameters =
    UserHalfParameters
    { program :: SC.UserProgram  -- Program to run in process space
    , slice   :: Int             -- How many ticks per slice
    }
