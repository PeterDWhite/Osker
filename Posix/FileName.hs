-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module FileName
    ( FileName            -- Data type for file names
    , outFileName         -- Nicer print out for file names
    , outRelativeFileName -- Print out for relative file names
    , outRelativeList     -- Print out a list of relative file names
    , isCwd               -- Determine if directory is "."
    , isParent            -- Determine if directory is ".."
    , rootFileName        -- "/" file name
    , absorbDots          -- Remove leading "././." from file name
    ) where

----------------------------------------------------------------------
-- The POSIX file name specification.
-- Also used by other operations, such as POSIX.4 message queue.
----------------------------------------------------------------------

-- Haskell imports
import List

type FileName = [String]

-- Nicer print out for file names
outFileName :: FileName -> String
outFileName fn = "/" ++ outRelativeFileName fn

-- Print out for a relative file name
outRelativeFileName :: FileName -> String
outRelativeFileName fn = concat (intersperse "/" fn)

-- Print out a list of realtive file names
outRelativeList :: [FileName] -> String
outRelativeList fns =
  concat (intersperse "\n" (map outRelativeFileName fns))

-- Determine if directory is "."
isCwd :: FileName -> Bool
isCwd ["."] = True
isCwd _     = False

-- Determine if directory is ".."
isParent :: FileName -> Bool
isParent [".."] = True
isParent _      = False

-- Root file name
rootFileName :: FileName
rootFileName = ["/"]

-- Take any "././." off the front of a file name
absorbDots :: FileName -> FileName
absorbDots [] = []
absorbDots [s] = [s]
absorbDots (s:ss) = if s == "."
                    then absorbDots ss
                    else s:ss
