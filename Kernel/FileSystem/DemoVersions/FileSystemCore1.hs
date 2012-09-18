-- Copyright (c) Peter Duncan White, 2002
-- Copyright (c) OHSU, 2002
module FileSystemCore ( fsCore ) where

----------------------------------------------------------------------
-- The file system core device driver
----------------------------------------------------------------------

-- Posix imports
import qualified DirectoryData as DD
-- Osker imports
import qualified OskerMessage as SM
import qualified FileSystemCoreBio as B
import qualified CoreData as CD
import qualified CoreGets as CG

--------------------------------- -------------------------------------
-- The standard input output device driver is completely event driven
-- There is only one input channel
----------------------------------------------------------------------
fsCore ::
    B.FileSystemCoreBio -> -- Bounds on the IO
    String              -> -- Path to file system
    B.FileSystemCoreBIO ()
fsCore b fsName =
  do { fsOut b "Started"
     ; fsCore' b (CD.initFSData fsName)
     }

fsCore' ::
    B.FileSystemCoreBio ->  -- Bounds on the io
    CD.FSData           ->  -- Data maintained by the driver
    B.FileSystemCoreBIO ()  -- A bounded resumption IO action
fsCore' b fsData =
 do { sm <- B.getFromChan b
    ; fsData' <- processInput b fsData sm
    ; B.yield b
    ; fsCore' b fsData'
    }

-- Process a single input
processInput ::
    B.FileSystemCoreBio ->  -- Bounds on the io
    CD.FSData           ->  -- Data maintained by the driver
    SM.OskerMessage     ->  -- The input message
    B.FileSystemCoreBIO CD.FSData  -- A bounded resumption IO action
processInput b fsData sm =
  let sp = SM.getPay sm
  in do { case sp of
            SM.ToFSCore fscorereq ->
              case fscorereq of
                SM.OpenDirReq { SM.odPName = _pn
                              , SM.odDir   = fn
                              } ->
                  do { -- Open the directory stream in the underlying
                       -- Posix / Linux file system (cheating)
                     ; (dir, fsData') <- CG.getDirectory b fsData fn
--                     ; fsOut b ("Opendir.2: " ++ show dir)
                       -- Send the response to the file system
                     ; let rspMsg = SM.FromFSCore
                                        ( SM.OpenDirRsp
                                          { SM.odRspDir = dir
                                          , SM.odRsp    = True
                                          }
                                        )
                     ; (B.respond b) sm rspMsg
                     ; return fsData'
                     }
                SM.ReadDirReq { SM.rdPName = _pn
                              , SM.rdDir   = dir
                              } ->
                  do { (fsData', fname, dir', isDir) <-
                          CG.getFile b fsData dir
                     ; let rspMsg = SM.FromFSCore
                                        ( SM.ReadDirRsp
                                          { SM.rdRspDir = dir'
                                          , SM.rdName   = last fname
                                          , SM.rdIsDir  = isDir
                                          }
                                        )
                     ; (B.respond b) sm rspMsg
                     ; return fsData'
                     }
            _otherwise -> error ("processInput" ++ show sp)
        }

-- Print outs from file system core
fsOut ::
    B.FileSystemCoreBio -> -- Bounds on the io
    String ->              -- String to print
    B.FileSystemCoreBIO () -- This is an IO action
fsOut b s = (B.putStrLn b) ("***[FS.Core]...\t\t" ++ s)
