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
            ; let cache' = DC.dirInsertCache
                              cache
                              fn
                              (DD.isDir dir)
                              (DD.dirCount dir)
                              (Just (DD.stream dir))
                  fsData' = fsData { CD.directoryCache = cache' }
            ; return (dir, fsData')
            }
       Just freight ->
         do {
--              fsOut b ("In Cache: " ++ FN.outFileName fn)
              -- Already in cache, so rewind it for this time through
            ; case DC.stream freight of
                Nothing ->
                  -- Directory placed here by read, now fill it in
                  do { 
--                       fsOut b ("In Cache / fill in: " ++ FN.outFileName fn)
                     ; dir <- (B.openDir b) fn
                     ; let freight' = freight
                                      { DC.stream = Just (DD.stream dir)
                                      , DC.dirCount = DD.dirCount dir
                                      }
                           cache' = DC.updateDir cache fn freight'
                           fsData' = fsData { CD.directoryCache = cache' }
                     ; return ( dir, fsData' )
                     }
                Just dirStream ->
                  do { (B.rewindDir b) dirStream
                     ; return ( DD.DIR { DD.stream    = dirStream
                                       , DD.dPath     = fn
                                       , DD.isDir     = DC.isDirectory
                                                          freight
                                       , DD.dirCount  = DC.dirCount
                                                          freight
                                       , DD.readCount = 0
                                       }
                              , fsData
                              )
                    }
            }

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
       Nothing -> error ("getFile: dir not cached: " ++ show dir)
       Just node ->
         let freight = DC.getFileInfo node
         in if DC.dirComplete freight
            then -- Get the file from the cache
                 let mrNode = DC.getNthFile (DD.readCount dir) node
                 in case mrNode of
                      Nothing -> error ("getFile: bad cache\n\t" ++
                                        show dir ++ "\n\t" ++ show freight)
                      Just rNode ->
                        let fname = DC.fileName (DC.getFileInfo rNode)
                            dir'  = DD.bump dir
                        in do {
--                                fsOut b ( "read in complete cache " ++
--                                          FN.outFileName fname ++
--                                          ", " ++ show (DC.isDirectory freight)
--                                        )
                              ; return ( fsData  -- No change
                                       , fname   -- Next file name
                                       , dir'    -- Read count bumped
                                       , DC.isDirectory (DC.getFileInfo rNode)
                                       )
                              }
            else -- Go to the file system to get the next file
                 do { fname <- (B.readDir b) dir
                    ; let fullname = dirname ++ [fname]
                    ; fstat <- (B.getFileStatus b) fullname
--                    ; fsOut b ( "read in incomplete cache: " ++
--                                fname ++ ", " ++
--                                show (P.isDirectory fstat) ++
--                                "\n\t" ++ show dir )
                    ; let isDir = P.isDirectory fstat
                          cache' = if fname == "." || fname == ".."
                                   then cache
                                   else DC.bumpDir cache dirname
                          cache'' = if fname == "." || fname == ".."
                                    then cache'
                                    else DC.dirInsertCache
                                           cache'
                                           fullname
                                           isDir
                                           0
                                           Nothing
                          fsData' = fsData { CD.directoryCache = cache'' }
                    ; return ( fsData'      -- New cache information
                             , fullname     -- Next name
                             , DD.bump dir  -- Read count bumped
                             , isDir        -- Is it a directory
                             )
                    }
