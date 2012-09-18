-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileSystemPid ( fsPid ) where

----------------------------------------------------------------------
-- The file system component corresponding to a given system half.
-- Each process has a kernel thread representing it in the file
-- system.
----------------------------------------------------------------------

-- Haskell imports
import List
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID
import qualified FileName as FN
import qualified SystemCall as SC
-- Domain imports
import qualified ParseFileName as PFN
-- Braid imports
import qualified DomainBraid as DB
-- Osker imports
import qualified OskerMessage as OM
-- Local imports
import qualified FileSystemPidBd as B

-- The data maintained by the file system core device driver
data FSPidData =
    FSPidData { processName :: PN.ProcessName
              , processId   :: PID.ProcessId
              , processCwd  :: FN.FileName
                -- Work that has been sent to the file system core
              , pendingWork :: [OM.OskerRspMsg]
              }

initFSPidData ::
    PN.ProcessName -> -- Name of process represented by FS Pid thread
    PID.ProcessId ->  -- Process Id of process represented
    FN.FileName ->    -- Current working directory
    FSPidData
initFSPidData pname pid cwd =
    FSPidData { processName = pname
              , processId   = pid
              , processCwd  = cwd
              , pendingWork = []
              }

--------------------------------- -------------------------------------
-- The standard input output device driver is completely event driven
-- There is only one input channel
----------------------------------------------------------------------
fsPid ::
    B.FileSystemPidBdC -> -- Bounds on the IO
    PN.ProcessName     -> -- Name of the process represented
    PID.ProcessId      -> -- Id of the process represented
    FN.FileName        -> -- Current working directory of process
    B.FileSystemPidBd DB.DomainRet
fsPid b pname pid cwd =
  do { fsPidOut b pname "Started"
     ; fsPid' b (initFSPidData pname pid cwd)
     }

fsPid' ::
    B.FileSystemPidBdC -> -- Bounds on the IO
    FSPidData          -> -- Data maintained by the driver
    B.FileSystemPidBd DB.DomainRet -- A bounded resumption IO action
fsPid' b fsData =
 do { sm <- B.getFromChan b
    ; fsPidOut b (processName fsData) ("Got: " ++ show sm)
    ; fsData' <- processSystemHalfInput b fsData sm
    ; B.yield b
    ; fsPid' b fsData'
    }

-- Process a message from the system half
processSystemHalfInput ::
    B.FileSystemPidBdC -> -- Bounds on the IO
    FSPidData          -> -- Data maintained by the driver
    OM.OskerRspMsg     -> -- Message to process
    B.FileSystemPidBd FSPidData -- A bounded resumption IO action
processSystemHalfInput b fsData sm =
  let pay = OM.payOfM sm
  in case pay of
       OM.ToFileSystemDD ( OM.IOReqFS { OM.reqFS = sysreq } ) ->
          case sysreq of
            SC.OpenDirReq { SC.openPath = p } ->
              let openDir = referencedDirectory fsData (PFN.stringToFileName p)
                  fsCorePay = OM.ToFSCore
                                OM.OpenDirReq { OM.odPName = processName fsData
                                              , OM.odDir   = openDir
                                              }
                  fsData' = fsData { pendingWork = sm: (pendingWork fsData) }
              in do { (B.forwardToCore b) sm fsCorePay
                    ; return fsData'
                    }
            SC.ReadDirReq { SC.readDir = dir } ->
              let fsCorePay = OM.ToFSCore
                                OM.ReadDirReq { OM.rdPName = processName fsData
                                              , OM.rdDir   = dir
                                              }
                  fsData' = fsData { pendingWork = sm: (pendingWork fsData) }
              in do { (B.forwardToCore b) sm fsCorePay
                    ; return fsData'
                    }
            _else -> error ("processSystemHalfInput.1: " ++ show sysreq)
       OM.FromFSCore fsCoreRsp ->
         case fsCoreRsp of
           OM.OpenDirRsp { OM.odRspDir = dir
                         , OM.odRsp    = _rsp
                         } ->
             let (fsData', sm') = matchCoreRsp sm fsData
                 sysrsp = SC.SystemResponse
                          { SC.srErrno = Nothing
                          , SC.srSpecific =
                              SC.OpenDirRsp { SC.openDirRsp = Just dir }
                          }
                 sp     = OM.FromFileSystemDD
                             ( OM.IORspFS { OM.rspFS = sysrsp } )
             in do { (B.respond b) sm' sp
                   ; return fsData'
                   }
           OM.ReadDirRsp { OM.rdRspDir = dir
                         , OM.rdName   = fname
                         , OM.rdIsDir  = isDir
                         } ->
             let (fsData', sm') = matchCoreRsp sm fsData
                 sysrsp = SC.SystemResponse
                          { SC.srErrno = Nothing
                          , SC.srSpecific =
                              SC.ReadDirRsp
                              { SC.readDirRsp   = Just dir
                              , SC.readDirName  = fname
                              , SC.readDirIsDir = isDir
                              }
                          }
                 sp     = OM.FromFileSystemDD
                             ( OM.IORspFS { OM.rspFS = sysrsp } )
             in do { --fsPidOut b (processName fsData)
                     --           ("ReadDirRsp: " ++ show sysrsp)
                   ; (B.respond b) sm' sp
                   ; return fsData'
                   }
--           _else -> error ("processSystemHalfInput.3: " ++ show fsCoreRsp)
       _otherwise -> error ("processSystemHalfInput.2: " ++ show pay)

-- Match a response from the file system core with pending work.
-- The matching message, together with the updated state, is returned
matchCoreRsp :: OM.OskerRspMsg -> FSPidData -> (FSPidData, OM.OskerRspMsg)
matchCoreRsp sm fsData =
  let (matching, notMatching) =
         partition (OM.matchMessages sm) (pendingWork fsData)
  in case matching of
       [] -> error ("matchCoreRsp: " ++ show sm ++ "\n\t" ++ show notMatching)
       [sm'] -> let fsData' = fsData { pendingWork = notMatching }
                in (fsData', sm')
       (sm':sms') -> let fsData' = fsData { pendingWork = sms' ++ notMatching }
                     in (fsData', sm')

-- Compute the directory referenced
referencedDirectory :: FSPidData -> FN.FileName -> FN.FileName
referencedDirectory fsData fn =
  let cwd = processCwd fsData
  in if FN.isCwd fn
     then cwd
     else if FN.isParent fn
          then if length cwd >= 1
               then init cwd
               else FN.rootFileName
          else -- Relative path name
               cwd ++ FN.absorbDots fn

-- Print outs from file system device drivers
fsPidOut ::
    B.FileSystemPidBdC -> -- Bounds on the IO
    PN.ProcessName     -> -- Name of process represented
    String             -> -- String to print
    B.FileSystemPidBd ()  -- This is an IO action
fsPidOut b pn s =
  (B.putStrLn b) ("***[FSDD." ++ PN.outProcName pn ++ "]...\t\t" ++ s)
