-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileSystemCore ( fsCore ) where

----------------------------------------------------------------------
-- The file system core device driver
----------------------------------------------------------------------

-- Posix imports
import qualified DirectoryData as DD
-- Braid imports
import qualified DomainBraid as DB
-- Osker imports
import qualified OskerMessage as OM
import qualified FileSystemCoreBd as B
import qualified CoreData as CD
import qualified CoreGets as CG
import qualified CoreOut as CO
--import qualified SystemHalfActor as SHA

--------------------------------- -------------------------------------
-- The standard input output device driver is completely event driven
-- There is only one input channel
----------------------------------------------------------------------
fsCore ::
    B.FileSystemCoreBdC -> -- Bounds on the IO
    String              -> -- Path to file system
    B.FileSystemCoreBd DB.DomainRet
fsCore b fsName =
  do { tid <- B.myThreadId b
     ; CO.fsOut b ("Started (" ++ show tid ++ ")")
     ; fsCore' b (CD.initFSData fsName)
     }

fsCore' ::
    B.FileSystemCoreBdC ->  -- Bounds on the io
    CD.FSData           ->  -- Data maintained by the driver
    B.FileSystemCoreBd DB.DomainRet -- A bounded resumption IO action
fsCore' b fsData =
 do { -----------------------------------------------------------------
      -- First some code that is supposed to be here
      -----------------------------------------------------------------
      sm <- B.getFromChan b
    ; fsData' <- processInput b fsData sm
      -----------------------------------------------------------------
      -- Attempt to send to the kernel core, borrowing a function for
      -- that purpose from the system half.
      --
      -- If you can't get it to compile with "undefined" parameters,
      -- then you can't get it to compile with defined parameters.
      --
      -- We *would* be able to do these things if the system half and
      -- the file system core were *not* in their own bounded IO
      -- monads.
      -----------------------------------------------------------------
--    ; SHA.sysRsp undefined undefined
      -----------------------------------------------------------------
      -- Now attempt to access a system half state element. Again, if
      -- you can't get it to compile with an "undefined" parameter,
      -- then you can't get it to compile with any other parameter.
      -----------------------------------------------------------------
--    ; stolen <- SHA.getVanillaElt undefined
      -----------------------------------------------------------------
      -- We now resume our regular programming, using the only
      -- communication primitives that are allowed in the bounded IO
      -- monad for the file system core.
      -----------------------------------------------------------------
    ; B.yield b
    ; fsCore' b fsData'
    }

-- Process a single input
processInput ::
    B.FileSystemCoreBdC ->  -- Bounds on the io
    CD.FSData           ->  -- Data maintained by the driver
    OM.OskerRspMsg      ->  -- The input message
    B.FileSystemCoreBd CD.FSData  -- A bounded resumption IO action
processInput b fsData sm =
  let sp = OM.payOfM sm
  in do { case sp of
            OM.ToFSCore fscorereq ->
              case fscorereq of
                OM.OpenDirReq { OM.odPName = _pn
                              , OM.odDir   = fn
                              } ->
                  do { -- Open the directory stream in the underlying
                       -- Posix / Linux file system (cheating)
                     ; (dir, fsData') <- CG.getDirectory b fsData fn
--                     ; CO.fsOut b ("Opendir.2: " ++ show dir)
                       -- Send the response to the file system
                     ; let rspMsg = OM.FromFSCore
                                        ( OM.OpenDirRsp
                                          { OM.odRspDir = dir
                                          , OM.odRsp    = True
                                          }
                                        )
                     ; (B.respond b) sm rspMsg
                     ; return fsData'
                     }
                OM.ReadDirReq { OM.rdPName = _pn
                              , OM.rdDir   = dir
                              } ->
                  do { (fsData', fname, dir', isDir) <-
                          CG.getFile b fsData dir
                     ; let rspMsg = OM.FromFSCore
                                        ( OM.ReadDirRsp
                                          { OM.rdRspDir = dir'
                                          , OM.rdName   = last fname
                                          , OM.rdIsDir  = isDir
                                          }
                                        )
                     ; (B.respond b) sm rspMsg
                     ; return fsData'
                     }
            _otherwise -> error ("processInput" ++ show sp)
        }
