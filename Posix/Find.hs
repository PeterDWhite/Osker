-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Find ( find ) where

----------------------------------------------------------------------
-- A rough approximation to the unix find command
----------------------------------------------------------------------

-- Haskell imports
import Monad
import Maybe
-- Posix imports
import qualified DirectoryIF as DIF
import qualified DirectoryData as DD
import qualified FileName as FN
import qualified SystemCall as SC

-- This function is equivalent to the Unix command "find . -print"
find ::
    FN.FileName   ->     -- Path name
    SC.U [FN.FileName]   -- List of directories
find path =
  do { fns <- find' path
     ; return (["."]:fns) -- Add "." at top level only
     }

-- The helper to find, that does not return any "." entries
find' ::
    FN.FileName   ->     -- Path name
    SC.U [FN.FileName]   -- List of directories
find' path =
  do { ret <- DIF.opendir (FN.outFileName path)
     ; when (isJust (SC.srErrno ret))
            (error ( "find': Bad opendir rsp: " ++
                     FN.outFileName path ++ " ... " ++ show ret))
     ; case SC.srSpecific ret of
          SC.OpenDirRsp { SC.openDirRsp = mdir } ->
            case mdir of
              Nothing -> error ("find': open failed: " ++ show ret)
              Just dir -> oneDir path dir
          _otherwise -> error ("find': bad response to open: " ++ show ret)
     }

-- Process one directory of the find
oneDir ::
    FN.FileName   ->   -- Path name
    DIF.DIR       ->   -- Directory entry structure
    SC.U [FN.FileName] -- List of directories
oneDir path dir =
  do { (first, mdir) <- readOne path dir
     ; case mdir of
         Nothing -> -- Current was cwd or parent
           do { rest <- oneDir path dir
              ; return rest
              }
         Just newdir ->
           if DD.done newdir
           then return first
           else  do { rest <- oneDir path newdir
                    ; return (first ++ rest)
                    }
     }

-- Read a directory entry, recursing if it is a directory itself
readOne ::
    FN.FileName   ->   -- Current path for directory searcdh
    DIF.DIR       ->   -- Current directory stream
    SC.U ([FN.FileName], Maybe DIF.DIR)
readOne path dir =
  do { ret <- DIF.readdir dir
     ; when (isJust (SC.srErrno ret))
            (error ( "oneDir: Bad readdir: " ++
                     FN.outFileName path ++ " ... " ++ show ret))
--     ; putStrLn ("RRRRRRead response: " ++ show ret)
     ; case SC.srSpecific ret of
              SC.ReadDirRsp { SC.readDirRsp   = mdir
                            , SC.readDirName  = fname
                            , SC.readDirIsDir = isDir
                            } ->
                let current = path ++ [fname]
                in case mdir of
                     Nothing -> error ("readOne: readdir failed: " ++ show ret)
                     Just dirent ->
                       if FN.isCwd [fname] || FN.isParent [fname]
                       then return ([], Nothing)
                       else if isDir
                            then do { recurse <- find' current
                                    ; return (current:recurse, Just dirent)
                                    }
                            else return ([current], Just dirent)
              _otherwise ->
                error ("readOne: bad response to open: " ++ show ret)
     }
