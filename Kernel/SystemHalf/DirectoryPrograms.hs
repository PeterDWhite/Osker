-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module DirectoryPrograms
    ( closedirProgram   -- Actor for Posix closedir()
    , opendirProgram    -- Actor for Posix opendir()
    , readdirProgram    -- Actor for Posix readdir()
    , rewinddirProgram  -- Actor for Posix rewinddir()
    ) where

----------------------------------------------------------------------
-- The POSIX directory calls
----------------------------------------------------------------------

-- POSIX imports
import qualified SystemCall as SC
-- Osker imports
import qualified SystemHalfActor as SHA
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The closedir system call:
-- Close an open directory stream.
----------------------------------------------------------------------
closedirProgram ::
    SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
closedirProgram b pay =
  do { (SHA.putStrLn b) ("closedirProgram: " ++ show pay)
     ; SHA.sysRsp SC.noError (SC.CloseDirRsp SC.passFlag)
     }

----------------------------------------------------------------------
-- The opendir system call:
-- Open a directory stream.
----------------------------------------------------------------------
opendirProgram ::
    SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
opendirProgram b pay =
  do { (SHA.toFileSystem b) (OM.IOReqFS (OM.getSysReq pay))
     ; fsrsp <- SHA.getFromChan b
     ; case fsrsp of
         OM.FromFileSystemDD iorsp ->
            case iorsp of
              OM.IORspFS { OM.rspFS = rsp } ->
                SHA.sysRsp (SC.srErrno rsp) (SC.srSpecific rsp)
         _otherwise -> error ( "opendirProg.2 " ++ show fsrsp )
     }

----------------------------------------------------------------------
-- The readdir system call:
-- Read from a directory stream.
----------------------------------------------------------------------
readdirProgram :: SHA.SystemHalfActorC -> OM.OskerPay -> SHA.SystemHalfProgram
readdirProgram b pay =
  do { (SHA.toFileSystem b) (OM.IOReqFS (OM.getSysReq pay))
     ; fsrsp <- SHA.getFromChan b
     ; case fsrsp of
         OM.FromFileSystemDD iorsp ->
            case iorsp of
              OM.IORspFS { OM.rspFS = rsp } ->
                SHA.sysRsp (SC.srErrno rsp) (SC.srSpecific rsp)
         _otherwise -> error ( "readdir.2: " ++ show fsrsp )
     }

----------------------------------------------------------------------
-- The rewinddir system call:
-- Reset a directory stream to the beginning of the current directory.
----------------------------------------------------------------------
rewinddirProgram ::
    SHA.SystemHalfActorC -> OM.OskerMsg -> SHA.SystemHalfProgram
rewinddirProgram b msg =
  do { (SHA.putStrLn b) ("rewinddirProgram: " ++ show (OM.pay msg))
     ; SHA.sysRsp SC.noError SC.RewindDirRsp
     }
