-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module DirectoryData
    ( DIR (..)       -- Data type for directory structure
--    , DirStream (..) -- The Osker directory stream type
    , bump           -- Bump the read count in a directory structure
    , done           -- Determine if a directory stream is done
    ) where

-- Haskell imports
import qualified Posix as P
-- Posix imports
import qualified FileName as FN

----------------------------------------------------------------------
-- The directory entry structure, visible to a user process
-- When we change to real IO, we will get rid of the cheating.
----------------------------------------------------------------------

--data DirStream
--    = Cheating P.DirStream  -- Uses the underlying Posix directory stream
--    | InCache  P.DirStream  -- The directory is in the cache

--instance Show DirStream where
--    show (Cheating _) = "Cheating"
--    show (InCache _)  = "InCache"

data DIR = DIR { stream    :: P.DirStream -- Directory stream
--               , inCache   :: Bool        -- Is this dir in cache?
               , dPath     :: FN.FileName -- Path to the directory
               , isDir     :: Bool        -- Is current entry a directory?
               , readCount :: Int         -- How many reads in the directory
               , dirCount  :: Int         -- How many files in the directory
               }

-- Bump the read count in a DIR
bump :: DIR -> DIR
bump dir = dir { readCount = readCount dir + 1 }

-- Check if directory stream is done
done :: DIR -> Bool
done dir = readCount dir == dirCount dir

instance Show DIR where
    show d = "DIR(> " ++ FN.outFileName (dPath d) ++
             ", " ++ show (isDir d) ++
             ", " ++ show (dirCount d) ++
             ", " ++ show (readCount d) ++ " <)"
