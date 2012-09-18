-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module File ( File (..) ) where

----------------------------------------------------------------------
-- Implementing the Posix stream
----------------------------------------------------------------------

-- Local imports
import qualified FileName as FN

data File = File { name :: FN.FileName  -- Name of the file
                 , pos  :: Int          -- Current read / write position
                 } deriving (Show)
