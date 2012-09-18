-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ProcessControl
    ( updateProcess         -- Actor update to process as part of fork
    , finishProcess         -- Finish process control work
    , forkProcess           -- Actor part of a fork
    , exitProcess           -- Actor part of an exit
    , addProcessPCInternal  -- IO shell part of a fork
    , addProcessGroup       -- Actor part of adding a process group
    , addSession            -- Actor part of adding a session
      -- In support of the layered braid
    , addProcessDomain      -- Add a process to a domain from platform braid
    ) where

----------------------------------------------------------------------
-- Requests for the kernel core portion of process control actions
-- such as fork and exit.
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
import Monad
import Maybe
-- Utility imports
import qualified IntGen as IG
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID
import qualified Errno as ER
import qualified FileName as FN
-- Braid imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import qualified BraidUtilities as BU
import qualified OskerConcurrent as C
import qualified DomainBraid as DB
import qualified PlatformBraid as PlB
-- Debugging imports
import qualified DomainOut as DO
import qualified PlatformOut as PO
-- Domain model imports
import qualified DomainGraph as DOMG
-- Osker imports
import qualified SystemHalfBdM as SHB
import qualified SystemHalfActor as SHA
import qualified DeviceId as DID
import qualified SystemHalfBounds as BD
import qualified LocalPartitionedState as LPS
import qualified KernelCoreActor as KCA
import qualified KernelCoreActions as KCACT
import qualified UnsafeAS as UAS
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified ExecutiveParameters as EP
import qualified OskerMessage as OM
import qualified Process as P
import qualified SystemHalf as SH
import qualified SystemHalfCallTable as SHCT
import qualified SystemHalfIOShell as IOS
import qualified IOSOut as IOO
import qualified SystemHalfMessage as SHM
import qualified JobControlData as JCD
import qualified DeviceMap as DM
import qualified FileSystemPidBd as FSPB
import qualified FileSystemPid as FSP
import qualified UserHalfParameters as UHP

----------------------------------------------------------------------
-- Add a process: The process control portion, run at the Braid level
----------------------------------------------------------------------

addProcessPC ::
    OM.OskerChannel ->             -- Channel to kernel core
    OM.OskerChannel ->             -- Channel to the file system core
    DM.DeviceMap    ->             -- Device channel information
    FN.FileName     ->             -- Current working directory
    OM.OskerRspPay  ->             -- The message from the actor
    DB.DomainBraid (Maybe OM.OskerRspPay) -- Return msg for further work
addProcessPC kcChan fsCoreChan devmap cwd pay =
  let pcr = OM.getToProcessControlReq pay
  in case pcr of
       OM.AddProcessPCR { OM.apProcessTags  = processTags
                        , OM.apProcessNode  = procNode
                        , OM.forkingProcess = forking
                        } ->
         do { DO.domOut (">>> addProcessPC")
            ; procinfo <- addProcessPCInternal
                            processTags
                            procNode
                            kcChan
                            fsCoreChan
                            devmap
                            cwd
            ; DO.domOut ("<<< addProcessPCInternal")
              -- Generate a message to route back to an actor action
            ; return (Just (OM.FromProcessControl
                             ( OM.UpdateProcessPCR
                               { OM.forkedProcess  = procinfo
                               , OM.forkingProcess = forking
                               }
                             )))
            }
       _otherwise ->
         error ("Bad message routed to AddProcess: " ++ show pay)

-- Braid level work to actuall start the kernel threads for the process
startProcessPC ::
    OM.OskerPay        ->              -- The message from the actor
    DB.DomainBraid (Maybe OM.OskerPay) -- Return msg for further work
startProcessPC pay =
  case OM.getToProcessControlReq pay of
    OM.StartProcessPCR { OM.spForked  = forked
                       , OM.spForking = forking
                       } ->
      do { DO.domOut (">>> startProcessPC")
         ; P.startProcess forked
         ; DO.domOut ("<<< startProcessPC")
         ; return (Just ( OM.FromProcessControl
                            OM.FinishProcessPCR
                            { OM.fpForked  = P.prProcessTags forked
                            , OM.fpForking = forking
                            }
                        ) )
         }
    _otherwise -> error "startProcessPC: not a start process message"

-- Braid shell level processing for adding a process
addProcessPCInternal ::
    P.ProcessTags        ->   -- Various Ids about the process
    DOMG.ProcessNodeData ->   -- Process / domain model info
    OM.OskerChannel      ->   -- Channel to kernel core
    OM.OskerChannel      ->   -- Channel to the file system core
    DM.DeviceMap         ->   -- Device map
    FN.FileName          ->   -- Current working directory
    DB.DomainBraid ( P.Process OM.OskerRspMsg ) -- Information about process
addProcessPCInternal processTags procNode kcChan fsCoreChan devmap cwd =
  do { DO.domOut (">>> addProcessPCInternal")
       -- Get the process name and process Id
     ; let pname = P.prProcessName processTags
           pid   = P.prProcessId processTags
     -- Create the in channel to the system half
     ; inChan <- DB.lift ( C.newChan ((PN.outProcessName pname) ++ ".CHAN") )
     -- Create the in channel to the file system "Pid" process
     ; fsChan <- DB.lift ( C.newChan ((PN.outProcessName pname) ++ ".fsPid") )
     -- Set up the system half parameters
     ; (shbio, bounds, systemHalfParameters, userHalfParameters) <-
          DB.lift
            ( mkSystemHalf processTags procNode kcChan devmap
                           cwd inChan fsChan
            )
     -- Create the IO shell
     ; ioShell <-
         ( DB.lift
            ( BU.oskerFork
              (PN.outProcessName pname ++ ".Sys/2")
              inChan      -- Input channel to exec
                ( DB.proj
                  ( SHB.toM
                      bounds
                      ( IOS.systemHalfIoShell
                                  shbio systemHalfParameters userHalfParameters
                      )
                  )
                )
            )
         )
     ; DB.lift ( C.yield )
     -- Create the file system "Pid" thread
     ; let fsBounds = FSPB.FileSystemPidBounds
                      { FSPB.inputChan  = OM.OskerChannel fsChan
                      , FSPB.fsCoreChan = fsCoreChan
                      , FSPB.fsSys2Chan = OM.OskerChannel inChan
                      , FSPB.localName  = pname
                      }
           fsBio    = FSPB.mkFileSystemPidBdC fsBounds
           fsDD     = FSP.fsPid fsBio pname pid cwd
     ; fsThreadInfo <-
         DB.lift
           ( BU.oskerFork
              (PN.outProcessName pname ++ ".fsPid")
              fsChan
              (DB.proj (FSPB.toM fsBounds fsDD))
           )
     -- Format the return message, which tells the actor
     -- to update its tables with the information generated
     -- here at the Braid level. To build the message, we will
     -- clone the input message
     ; return P.Process
                { P.prIoShell        = ioShell
                , P.prFileSysThread  = fsThreadInfo
                , P.prProcessNode    = procNode
                , P.prProcessTags    = processTags
                }
     }

----------------------------------------------------------------------
-- Update a process: The actor portion, after process control
-- has already executed, adds the information generated at the Braid
-- level into the fractured state. The fork prepares an 
----------------------------------------------------------------------
updateProcess :: KCA.KernelCoreSegment r
updateProcess pay =
  do { UAS.unsafeASIO (">>> updateProcess: " ++ show pay)
     ; let pcr =OM.getFromProcessControlReq pay
     ; let OM.UpdateProcessPCR { OM.forkedProcess  = process
                               , OM.forkingProcess = forking
                               } = pcr
           processName    = P.prProcessName (P.prProcessTags process)
           sys2Thread     = P.prIoShell process
     -- Associate the user half tid with the process name in the
     -- thread map.
     ; KCACT.updateThreadMap (BU.tiTid sys2Thread) processName
       -- Next, update the process map
     ; KCACT.updateProcessMap processName process
       -- Braid level work to start the process
     ; KCA.defer ( OM.StartProcessPCR 
                        { OM.spForked  = process
                        , OM.spForking = forking
                        }
                 )
                 startProcessPC
     }

-- Finish the processing of the fork
finishProcess :: KCA.KernelCoreSegment r
finishProcess pay =
  let pcr = OM.getFromProcessControlReq pay
  in case pcr of
       OM.FinishProcessPCR { OM.fpForked  = forked
                           , OM.fpForking = forking
                           } ->
         do { UAS.unsafeASIO (">>> finishProcess: " ++ show pay)
            ; KCA.shRsp (OM.ForkResponse (Just (P.prProcessId forked)))
                        (P.prProcessName forking)
            }
       _otherwise -> error ("finishProcess")

----------------------------------------------------------------------
--
-- Fork a process
--
-- The process fork is triggered by the arrival of a ForkProcess
-- kernel request. This message is processed by the forkProcess
-- actor program. forkProcess generates an AddProcess
-- message, which is routed to the IO shell. The AddProcess
-- message is processed at the Braid level, by the function
-- addProcessPC. addProcessPC calls addProcessPCInternal, which
-- is also used during Osker initialization. The kernel core
-- completes its processing with forkProcessFinish, which is
-- triggered by the output message of addProcessPC.
-- forkProcessFinish sends the response message back to
-- the system half of the process that requested the fork.
--
----------------------------------------------------------------------
forkProcess :: KCA.KernelCoreSegment r
forkProcess pay =
 do { UAS.unsafeASIO (">>> forkProcess: " ++ show pay)
      -- Bust out the fields of the kernel core request:
    ; let OM.ForkProcess { OM.fpTags = processTags
                         , OM.fpNode = processNode
                         } = OM.getKernelCoreReq pay
    -- Access the kernel core thread Id
    ; GPS.GfseKernelCoreChan kcChan <-
        KCACT.getGlobalStateElement
          (GPSK.GPSK (Right GPSK.GPSKKernelCoreChan))
    -- Access the file system core channel
    ; GPS.GfseFSCoreChan fsCoreChan <-
        KCACT.getGlobalStateElement
          (GPSK.GPSK (Right GPSK.GPSKFSCoreChan))
    -- Access the file system root. For now, this is the initial current
    -- working directory of every process
    ; GPS.GfseFSRoot fsRoot <-
        KCACT.getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKFSRoot))
    -- Access the job control data
    ; GPS.GfseJobControlData jcd <-
        KCACT.getGlobalStateElement
          (GPSK.GPSK (Right GPSK.GPSKJobControlData))
    -- Access the PID generator
    ; GPS.GfsePidGen pidGen <-
        KCACT.getGlobalStateElement (GPSK.GPSK (Right GPSK.GPSKPidGen))
    -- Access the Osker global var
    ; GPS.GfseDeviceMap deviceMap <-
        KCACT.getGlobalStateElement
          (GPSK.GPSK (Right GPSK.GPSKDeviceMap))
    -- Convert the tid into a process name
    ; let -- The new process name is the base name (all but the last
          -- last) of the process name, plus the newly generated PID
          pname = P.prProcessName processTags
          basename = PN.processGroupName pname
          (pidGen', newPid) = IG.genNew pidGen
          newProcessName = basename ++ [show newPid]
    ; KCA.updateVanillaElt
        (GPSK.GPSK (Right GPSK.GPSKPidGen))
        (GPS.GfsePidGen pidGen')
      -- Get the response context
    ; let newProcessTags =
            P.ProcessTags
            { P.prProcessName = newProcessName
            , P.prProcessId   = newPid
              -- The process group id is unchanged
            , P.prProcessGroupId = P.prProcessGroupId processTags
              -- The new parent pid is the pid of the forking process
            , P.prParentPid      = P.prProcessId processTags
            }
      -- Tell process control to create the new process
    ; UAS.unsafeASIO "forkProcess: done updating"
    ; KCA.defer
        ( OM.AddProcessPCR { OM.apProcessTags  = newProcessTags
                           , OM.apProcessNode  = processNode
                           , OM.forkingProcess = processTags
                           } )
        ( addProcessPC kcChan fsCoreChan deviceMap fsRoot )
    }

----------------------------------------------------------------------
-- Exit a process:
--
--   Remove it from the process / domain model
--   Terminate the io shell, executive, and user half of the process.
----------------------------------------------------------------------
exitProcess :: KCA.KernelCoreSegment r
exitProcess pay =
  do { UAS.unsafeASIO ("exitProcess: " ++ show pay)
       -- Access the job control data
     ; GPS.GfseJobControlData jcd <-
         KCACT.getGlobalStateElement
           (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let OM.ExitProcess pname = OM.getKernelCoreReq pay
           mproc = FM.lookupFM (JCD.jcdProcessMap jcd) pname
       in case mproc of
            Nothing -> KCA.endActor []
            Just proc ->
              -- Bust apart the process data, in preparation to send
              -- a delete process message to the process control at
              -- the Braid level of the kernel shell.
              let ioshellInfo = P.prIoShell proc
                  processTags = P.prProcessTags proc
              in do { -- Now remove the element from the process map
                    ; UAS.unsafeASIO ("Exit: got proc")
                    ; KCACT.reduceProcessMap pname
                    -- Also remove it from the pid map
                    ; KCACT.reducePidMap (P.prProcessId processTags)
                    -- Also remove it from the thread map
                    ; KCACT.reduceThreadMap (BU.tiTid ioshellInfo)
                    -- Send the work up to the Braid level
                    ; KCA.defer
                        ( OM.DeleteProcessPCR
                          { OM.dpProcessName = pname
                          , OM.dpSystemHalfTid = BU.tiTid ioshellInfo
                          }
                        )
                        ( delProcessPCInternal )
                    }
     }

-- The Braid level action to complete the work of the exit() system
-- call. This Braid level action kills the threads involved in the process
delProcessPCInternal ::
    OM.OskerPay             ->            -- The message from the actor
    DB.DomainBraid (Maybe OM.OskerPay)    -- Return msg for further work
delProcessPCInternal pay =
  let pcr = OM.getToProcessControlReq pay
  in case pcr of
       OM.DeleteProcessPCR { OM.dpProcessName   = pname
                           , OM.dpSystemHalfTid = ioshelltid
                           } ->
         do { IOO.iosOut pname "IO Shell delete process"
            ; DB.lift ( C.killThread ioshelltid )
            -- Update the trapHandler on the delete process
            ; return Nothing
            }
       _otherwise ->
         error "delProcessPCInternal: Not a delete process message"

----------------------------------------------------------------------
-- Add a process group:
----------------------------------------------------------------------
addProcessGroup :: KCA.KernelCoreSegment r
addProcessGroup pay =
  do { let OM.AddProcessGroup pid pgid tags = OM.getKernelCoreReq pay
           pn = P.prProcessName tags
     ; UAS.unsafeASIO ("pid: " ++ show pid)
     -- Access the job control data
     ; let pid' = if pid == PID.nullPID
                  then P.prProcessId tags
                  else pid
     ; UAS.unsafeASIO ("pid': " ++ show pid')
       -- Access the job control data
     ; GPS.GfseJobControlData jcd <-
         KCACT.getGlobalStateElement
           (GPSK.GPSK (Right GPSK.GPSKJobControlData))
       -- First check the condition that pgid is less than zero, this
       -- calls for its own error code
     ; if pgid < 0
       then KCA.shRsp (OM.AddProcessGroupResponse ER.EINVAL) pn
       else if pgid == PID.nullPID
            then do { -- Update the job control data
                    ; if JCD.searchPid jcd pid'
                      then let mjcd' = JCD.newProcessGroupByPid jcd pid'
                           in case mjcd' of
                                Nothing ->
                                  KCA.shRsp
                                    (OM.AddProcessGroupResponse ER.EPERM) pn
                                Just jcd' ->
                                  do { KCACT.updateJobControlData jcd'
                                       -- Respond to the calling system half
                                     ; KCA.shRsp
                                        (OM.AddProcessGroupResponse ER.NOERROR)
                                        pn
                                     }
                      else -- Could not find pid
                           KCA.shRsp (OM.AddProcessGroupResponse ER.ESRCH) pn
                    }
            else if JCD.searchProcessGroupId jcd pgid
                 then let mjcd' = JCD.moveProcess jcd pid' pgid
                      in case mjcd' of
                           Nothing ->
                             KCA.shRsp
                               (OM.AddProcessGroupResponse ER.EPERM) pn
                           Just jcd' ->
                             do { KCACT.updateJobControlData jcd'
                                  -- Respond to the calling system half
                                ; KCA.shRsp
                                   (OM.AddProcessGroupResponse ER.NOERROR) pn
                                }
                 else KCA.shRsp (OM.AddProcessGroupResponse ER.ESRCH) pn
     }

----------------------------------------------------------------------
-- Add a session group:
----------------------------------------------------------------------
addSession :: KCA.KernelCoreSegment r
addSession pay =
  do { let OM.AddSession tags = OM.getKernelCoreReq pay
           pn = P.prProcessName tags
       -- Access the job control data
     ; GPS.GfseJobControlData jcd <-
         KCACT.getGlobalStateElement
           (GPSK.GPSK (Right GPSK.GPSKJobControlData))
     ; let mjcd' = JCD.newSession jcd (P.prProcessId tags)
       in case mjcd' of
            Nothing ->
              KCA.shRsp (OM.AddSessionResponse ER.EPERM) pn
            Just jcd' ->
              do { KCACT.updateJobControlData jcd'
                   -- Respond to the calling system half
                 ; KCA.shRsp (OM.AddSessionResponse { OM.asrEr = ER.NOERROR } ) pn
                 }
     }

----------------------------------------------------------------------
-- Support for process creation
----------------------------------------------------------------------

mkSystemHalf ::
    P.ProcessTags         ->   -- Various Ids about the process
    DOMG.ProcessNodeData  ->   -- Process / domain model info
    OM.OskerChannel       ->   -- Channel to kernel core
    DM.DeviceMap          ->   -- Device map
    FN.FileName           ->   -- Current working directory
    C.Chan OM.OskerRspMsg ->   -- Input channel
    C.Chan OM.OskerRspMsg ->   -- File system pid input channel
    -- Arbitrary braid type, so this can be used at platform or
    -- domain level.
    B.Braid hs ls r ( SHB.SystemHalfBdMC
                    , SHB.SystemHalfBounds
                    , SHA.SystemHalfParameters OM.OskerChannel
                    , UHP.UserHalfParameters
                    )
mkSystemHalf processTags procNode kcChan devmap cwd inChan fsChan =
  let pname = P.prProcessName processTags
      pid   = P.prProcessId processTags
      systemHalfParameters =
        EP.ExecutiveParameters
          { EP.epProcessName    = pname
          , EP.epProcessId      = pid
          , EP.epLocalState     = SH.initSystemHalfPartitionedState
                                    procNode processTags cwd
          , EP.epCallTable      = SHCT.systemHalfCallTable
          , EP.epScreen         = SH.systemHalfScreen
          , EP.epReject         = EP.RejectFunction SH.systemHalfReject
          , EP.epCallTableIndex = SHM.getCallTableIndex
          , EP.epProcessNode    = procNode
          }
      mtimerdevinfo = FM.lookupFM devmap (fromEnum DID.TimerDeviceId)
      timerdevinfo  = fromJust mtimerdevinfo
      standarddevinfo =
        fromJust (FM.lookupFM devmap (fromEnum DID.StandardInOut))
      bounds = BD.SystemHalfBounds
               { BD.inputChan      = OM.OskerChannel inChan
               , BD.kernelCoreChan = kcChan
               , BD.timerChan      = OM.OskerChannel
                                       (BU.tiChan timerdevinfo)
               , BD.standardChan   = OM.OskerChannel
                                       (BU.tiChan standarddevinfo)
               , BD.fsChan         = OM.OskerChannel fsChan
               , BD.localName      = pname
               }
      shbio = SHB.mkSystemHalfBdMC bounds
      userHalfParameters = UHP.UserHalfParameters
                           { UHP.program = DOMG.pndUserHalf procNode
                           , UHP.slice   = 20000 -- **** Config later
                           }
  in return (shbio, bounds, systemHalfParameters, userHalfParameters)

-- Add process to a domain braid from a platform braid
addProcessDomain ::
    P.ProcessTags        ->   -- Various Ids about the process
    DOMG.ProcessNodeData ->   -- Process / domain model info
    OM.OskerChannel      ->   -- Channel to kernel core
    OM.OskerChannel      ->   -- Channel to the file system core
    DM.DeviceMap         ->   -- Device map
    FN.FileName          ->   -- Current working directory
    DB.DomainSt          ->   -- Input domain state
    PlB.PlatformBraid ( DB.DomainSt              -- Output domain state
                      , P.Process OM.OskerRspMsg -- Information about process
                      )
addProcessDomain processTags procNode kcChan fsCoreChan devmap cwd domSt0 =
  do { PO.platOut (">>> addProcessDomain")
       -- Get the process name and process Id
     ; let pname = P.prProcessName processTags
           pid   = P.prProcessId processTags
     -- Create the in channel to the system half
     ; (domSt1, inChan) <-
         PlB.lift (C.newChanLower ((PN.outProcessName pname)++".CHAN") domSt0)
     -- Create the in channel to the file system "Pid" process
     ; (domSt2, fsChan) <-
         PlB.lift (C.newChanLower ((PN.outProcessName pname)++".fsPid") domSt1)
     -- Set up the system half parameters
     ; (shbio, bounds, systemHalfParameters, userHalfParameters) <-
          PlB.lift
            ( mkSystemHalf processTags procNode kcChan devmap
                           cwd inChan fsChan
            )
     ; (domSt3, ioShell) <-
         ( PlB.lift
            ( BU.oskerForkLower
                (PN.outProcessName pname ++ ".Sys/2")
                inChan      -- Input channel to exec
                ( DB.proj
                  ( SHB.toM
                      bounds
                      ( IOS.systemHalfIoShell
                                  shbio systemHalfParameters userHalfParameters
                      )
                  )
                )
                domSt2
            )
         )
     -- Create the file system "Pid" thread
     ; let fsBounds = FSPB.FileSystemPidBounds
                      { FSPB.inputChan  = OM.OskerChannel fsChan
                      , FSPB.fsCoreChan = fsCoreChan
                      , FSPB.fsSys2Chan = OM.OskerChannel inChan
                      , FSPB.localName  = pname
                      }
           fsBio    = FSPB.mkFileSystemPidBdC fsBounds
           fsDD     = FSP.fsPid fsBio pname pid cwd
     ; (domSt4, fsThreadInfo) <-
         PlB.lift
           ( BU.oskerForkLower
              (PN.outProcessName pname ++ ".fsPid")
              fsChan
              (DB.proj (FSPB.toM fsBounds fsDD))
              domSt3
           )
     -- Return the process structure
     ; return ( domSt4
              , P.Process
                { P.prIoShell        = ioShell
                , P.prFileSysThread  = fsThreadInfo
                , P.prProcessNode    = procNode
                , P.prProcessTags    = processTags
                }
              )
     }
