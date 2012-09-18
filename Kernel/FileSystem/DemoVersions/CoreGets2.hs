-- Copyright (c) Peter Duncan White, 2002
-- Copyright (c) OHSU, 2002
module CoreGets
    ( getDirectory -- Get a directory from the underlying file system
    , getFile      -- Get a file from the underlying file system
    ) where

----------------------------------------------------------------------
-- Functions to get files for the file system core
----------------------------------------------------------------------

-- Haskell imports
import qualified Posix as P
-- Posix imports
import qualified DirectoryData as DD
import qualified FileName as FN
-- Osker imports
import qualified FileSystemCoreBio as B
import qualified DirectoryCache as DC
import qualified CoreData as CD
--import qualified CoreOut as CO

-- Get directory information, depending on if the directory is
-- in the cache or not. The cache is updated when the directory
-- was not in the cache already
getDirectory ::
    B.FileSystemCoreBio -> -- Bounds on the io
    CD.FSData           -> -- Data maintained by the driver
    FN.FileName         -> -- The directory to get
    B.FileSystemCoreBIO (DD.DIR, CD.FSData)
getDirectory b fsData fn =
  let cache = CD.directoryCache fsData
      mnode = DC.dirDetectCache cache fn
  in case mnode of
       Nothing ->
         do { -- fsOut b ("Not In Cache: " ++ FN.outFileName fn)
            ; dir <- (B.openDir b) fn
            ; return (dir, fsData)
            }
       Just _freight -> error "No cache in this version"

-- Get a file (not a directory), taking into account if the directory
-- containing the file is cached or not.
-- We know that the directory is always in the cache, and that it
-- got rewound on this pass through the directory
getFile ::
    B.FileSystemCoreBio -> -- Bounds on the IO
    CD.FSData           -> -- Core file system state
    DD.DIR              -> -- Directory structure
    B.FileSystemCoreBIO (CD.FSData, FN.FileName, DD.DIR, Bool)
getFile b fsData dir =
  let cache = CD.directoryCache fsData
      dirname = DD.dPath dir
      mnode = DC.lookupDirectory cache dirname
  in case mnode of
       Nothing ->
         do { fname <- (B.readDir b) dir
            ; let fullname = dirname ++ [fname]
            ; fstat <- (B.getFileStatus b) fullname
--            ; CO.fsOut b ( "read in incomplete cache: " ++ fname ++ ", " ++
--                           show (P.isDirectory fstat) ++ "\n\t" ++ show dir )
            ; let isDir = P.isDirectory fstat
            ; return ( fsData      -- New cache information
                     , fullname     -- Next name
                     , DD.bump dir  -- Read count bumped
                     , isDir        -- Is it a directory
                     )
            }
       Just _node -> error "Should be no cache"
