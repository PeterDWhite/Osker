-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Osker
    ( initOsker     -- The normal Haskell IO portion of Osker
    , initOskerDemo -- Initialize Osker from the demo
    ) where

----------------------------------------------------------------------
-- Initialization of Osker
--
-- The maps that support Osker:
-- PidMap :: ProcessId -> ProcessName
-- ProcessMap :: ProcessName -> Process
--    Process:
--      prProcessName -- A compound string
--      prProcessId   -- An Int
--      prKernelGate
--      prIoShell     -- IO Shell ThreadInfo
--      prSystemHalf  -- System Half ThreadInfo
--      prUserHalf    -- User Half ThreadInfo
--    ThreadInfo ::
--      Thread Id (tiTid)
--      MVar (tiDone)
--    KernelGate:
--      synchGate :: Gate SC.SystemRequest SC.SystemResponse
--      asynchGate -- A threadId
--    Gate:
--      gateFrom :: MVar SC.SystemRequest
--      gateTo   :: MVar SC.SystemResponse
---- Map the user half thread Id to the process name
-- ThreadMap :: ThreadId -> ProcessName
---- Map the device id to the thread info of the device thread
-- DeviceMap ::  Int -> ThreadInfo -- The Int is the device id
---- Map user threadid to ioshell thread id
-- TrapMap   :: ThreadId -> ThreadId
--
---- The osker global variable is global to all the kernel threads
-- The osker global variable:
--  sgDeviceMap   :: DeviceMap
--  sgKernelCore  :: ThreadInfo
--  sgTrapHandler :: ThreadInfo
---- The kernel core global variable is global only to the kernel core
--
----------------------------------------------------------------------

-- Haskell imports
import qualified IO as EIO
import qualified Concurrent as C
import qualified FiniteMap as FM
import qualified List as L
import qualified Monad as M
import qualified Directory as DIR
import qualified Exception as E
-- Braid imports
import qualified BraidExternal as B
import qualified OskerConcurrent as OC
import qualified PlatformOut as PO
-- Utility imports
import qualified IntGen as IG
import qualified BraidUtilities as BU
import qualified ThreadId as TID
import qualified Null as N
-- Debugging imports
--import qualified Unsafe as U
-- Posix imports
import qualified ProcessName as PN
-- Graph imports
import qualified Deep as D
import qualified NodeName as NN
-- Domain imports
import qualified ParsePlatform as PP
import qualified Platform as PF
import qualified DomainGraph as DG
import qualified ParseFileName as PFN
-- Actor imports
import qualified PlatformBraid as PlB
-- Osker imports
import qualified OskerMessage as OM
import qualified ExecutiveParameters as EP
import qualified Devices as DEV
import qualified PartitionedState as PS
import qualified LocalPartitionedState as LPS
import qualified KernelCoreIOShell as IOS
import qualified OskerGlobal as OG
import qualified KernelCore as KC
import qualified KernelCoreOut as KCO
import qualified KernelCoreCallTable as KCCT
import qualified GlobalPartitionedState as GPS
import qualified GlobalPartitionedStateKey as GPSK
import qualified ProgramTable as PT
import qualified KernelCoreMessage as KCM
import qualified JobControl as JC
import qualified DemoIF as DIF
import qualified DeviceMap as DM
import qualified DeviceId as DID
import qualified TimerDevice as TDEV
import qualified StandardIODevice as SIO
import qualified FileSystemCore as FSC
import qualified StandardIOBd as SIOB
import qualified TimerBd as TB
import qualified FileSystemCoreBd as FSB

-- Update the user half with the program from the program table
updateUserHalf ::
    PT.ProgramTable -> -- Program table containing user half
    PN.ProcessName  -> -- Accumulated process name
    DG.ProcessNode  -> -- Node to update
    DG.ProcessNode     -- Updated node
updateUserHalf progTable accumPN procDomNode =
  case D.nodeFreight procDomNode of
    DG.PlatformNode _platformNode -> procDomNode
    DG.DomainNode _domNode -> procDomNode
    DG.SessionNode _sessNode -> procDomNode
    DG.ProcessGroupNode _pgNode -> procDomNode
    DG.ProcessNode procNode ->
      let procName  = D.nodeName procDomNode
          mUserHalf = FM.lookupFM progTable (accumPN ++ procName)
      in case mUserHalf of
           Nothing -> procDomNode
           Just userHalf ->
             let procNode' = procNode { DG.pndUserHalf = userHalf }
             in D.changeFreight
                   procDomNode
                   (DG.ProcessNode procNode')

-- Initialize the kernel, the Haskell IO portion
initOsker ::
    String                   -> -- The file with the process domain model
    PT.ProgramTable          -> -- Programs for the initial processes
    C.Chan (DIF.OskerInput)  -> -- External input channel
    C.Chan (DIF.OskerOutput) -> -- External output channel
    String                   -> -- Path name to the file system
    IO ()
initOsker procDomModelFile progTable pin pout fsName =
  (do {
      ; putStrLn "Initializing Osker"
      -- Clean up the debugging message trace file
      ; traceExist <- DIR.doesFileExist "messageTrace"
      ; M.when traceExist (DIR.removeFile "messageTrace")
      -- Parse in the process domain model
      ; platform <- PP.parseDomainFile procDomModelFile
      ; M.when (not (DG.validateDomainProcessModel
                       (PF.platformDom platform)
                       (PF.platformRLM platform)))
               (error "Invalid platform domain model")
      -- Now check that the program table has an entry for everything
      -- in the process / domain model.
      ; let -- Get the process names, global format, from the process /
            -- domain model
            procNames1 = DG.listProcessNames (PP.platformDom platform)
            -- Get the process names from the program table.
            procNames2 = PT.listProcessNames progTable
      ; M.when (L.sort procNames1 /= L.sort procNames2)
               (error ("Process names don't check\n\t" ++
                       show (L.sort procNames1) ++ "\n\t" ++
                       show (L.sort procNames2)))
        -- Create the real world trace file
      ; rwTraceFile <- EIO.openFile "rwTrace" EIO.AppendMode
        -- Add the user halves from the program table to the process
        -- domain model
      ; let updatedModel = D.updateLeafNodes
                              NN.nnNull
                              (PP.platformDom platform)
                              (updateUserHalf progTable)
            updatedPlatform = platform { PP.platformDom = updatedModel }
      ; initState <-
          B.initBraid
            N.mkNull
            "Braid Init Thread/"
            (PlB.proj
               (initOsker2 (PF.platformDom updatedPlatform) pin pout fsName))
      ; putStrLn ("Domain process model:\n" ++ show updatedPlatform)
      ; putStrLn (">>>Osker init state:\n");
      ; putStrLn (show initState)
      ; putStrLn ("<<<Osker init state\n");
      ; finalState <- B.completeStateM
                         N.mkNull -- Used to be initState
                         (B.weave B.scheduleAndUpdate initState)
      ; let outstring = show finalState
        in putStrLn ("Osker final state: " ++ take 200 outstring)
      ; putStrLn ("Done with Osker.")
      })
  `E.catch`
  (\e -> putStrLn ("Exception caught: " ++ show e))

-- Initialize Osker, from the demo
initOskerDemo ::
    DG.DomainGraph           -> -- Process domain model
    C.Chan (DIF.OskerInput)  -> -- External input channel
    C.Chan (DIF.OskerOutput) -> -- External output channel
    String                   -> -- Path to the file name
    IO ()                       -- A Haskell IO action
initOskerDemo domGraph pin pout fsName =
  (do { putStrLn "Initializing Osker"
      ; M.when (not (DG.validateDomainProcessModel
                       domGraph
                       (DG.getPlatformRLM domGraph)))
               (error ("Invalid platform domain model: " ++ show domGraph))
        -- Clean up the debugging message trace file
      ; traceExist <- DIR.doesFileExist "messageTrace"
      ; M.when traceExist (DIR.removeFile "messageTrace")
        -- Create the real world trace file
      ; rwTraceFile <- EIO.openFile "rwTrace" EIO.AppendMode
      ; putStrLn ("**Domain process model:\n" ++ show domGraph)
      ; initState <- B.initBraid
                        N.mkNull
                        "Braid init.2"
                        (PlB.proj (initOsker2 domGraph pin pout fsName))
      ; putStrLn ("Osker init state: " ++ show initState)
      ; finalState <- B.completeStateM
                           initState
                           (B.weave B.scheduleAndUpdate initState)
      ; let outstring = show finalState
        in putStrLn ("Osker final state: " ++ take 100 outstring)
      ; C.threadDelay 1000000
      ; putStrLn ("Done with Osker.")
      })
  `E.catch`
  (\e -> putStrLn ("Exception caught: " ++ show e))

-- Initialize the kernel, the Resumption IO portion
initOsker2 ::
    DG.DomainGraph           -> -- Process domain model
    C.Chan (DIF.OskerInput)  -> -- External input channel
    C.Chan (DIF.OskerOutput) -> -- External output channel
    String                   -> -- Path name to file system
    PlB.PlatformBraid PlB.PlatformRet -- A threader action
initOsker2 domGraph pin pout fsName =
  do { -- Parse in the file system root
     ; PO.platOut ">>> initOsker"
     ; let fsRoot = PFN.stringToFileName fsName
       -- Create the input channel for the kernel core
     ; kcChan     <- PlB.lift (OC.newChan "KCChan")
       -- Create a device map, with just standard devices
     ; timerChan  <- PlB.lift (OC.newChan "Timer")
     ; stdChan    <- PlB.lift (OC.newChan "Standard IO")
     ; fsCoreChan <- PlB.lift (OC.newChan "File System Core")
     ; let -- Set up the timer device driver
           timerBounds = TB.TimerBounds (OM.OskerChannel timerChan)
           timerBio    = TB.mkTimerBdC timerBounds
           timerDD     = TB.toM
                           timerBounds
                           (TDEV.timerDevice timerBio)
           timerConfig = DM.DeviceConfig
                         { DM.dcDeviceId     = DID.TimerDeviceId    
                         , DM.dcChannel      = OM.OskerChannel timerChan
                         , DM.dcDeviceDriver = timerDD
                         , DM.dcDeviceName   = "Timer Device"
                         }
           -- Set up the standard IO Device driver
           sioBounds   = SIOB.StandardIOBounds
                         { SIOB.inputChan = OM.OskerChannel stdChan
                         , SIOB.oskerOut  = pout
                         , SIOB.oskerIn   = pin
                         }
           sioBio      = SIOB.mkStandardIOBdC sioBounds
           sioDD       = SIOB.toM sioBounds (SIO.standardIODevice sioBio)
           sioConfig   = DM.DeviceConfig
                         { DM.dcDeviceId     = DID.StandardInOut
                         , DM.dcChannel      = OM.OskerChannel stdChan
                         , DM.dcDeviceDriver = sioDD
                         , DM.dcDeviceName   = "Standard IO Device"
                         }
           -- Set up the file device driver
           fsBounds    = FSB.mkFileSystemCoreBounds
                            (OM.OskerChannel kcChan)
                            (OM.OskerChannel fsCoreChan)
           fsBio       = FSB.mkFileSystemCoreBdC fsBounds
           fsDD        = FSB.toM fsBounds (FSC.fsCore fsBio fsName)
           fileConfig  = DM.DeviceConfig
                         { DM.dcDeviceId     = DID.FileSystemId
                         , DM.dcChannel      = OM.OskerChannel fsCoreChan
                         , DM.dcDeviceDriver = fsDD
                         , DM.dcDeviceName   = "File system"
                         }
           deviceConfigs = [ timerConfig
                           , sioConfig
                           , fileConfig
                           ]
     ; let devSt0 = B.initBraidState N.mkNull []
     ; (devSt1, deviceMap ) <-
          DEV.standardDevices
            FM.emptyFM    -- An empty device map
            devSt0        -- An empty device braid
            deviceConfigs -- Device configuration list
     ; oskerMVar <-
         PlB.lift
           ( OC.newMVar
              "OskerGlobal"
              ( OG.OskerGlobal deviceMap (error "OskerGlobal.temp") )
           )
     -- Set up the PID Generator, and generate enough PIDs for
     -- those processes in the domain process model at init time.
     -- Exclude the special nullPID from the new PID generation.
     ; let gen = IG.initGen 0
           -- Initialize the global fractured state
           gps = KC.initKernelCorePartitionedState
                   domGraph    -- Process / domain model
                   gen         -- Process Id Generator
                   JC.initJobControlData -- Domain, session, .. info
                   deviceMap   -- Device information table
                   (OM.OskerChannel kcChan)     -- Input channel to kernel core
                   (OM.OskerChannel fsCoreChan) -- Input chan to file sys core
                   fsRoot      -- File system root
     -- Initialize the domains, sessions, process groups, and
     -- processes, from the domain / process model
     ; (jcs, jcret) <-
         JC.initJobControl
           ( JC.JCState { JC.jcsGps = gps -- Global partitioned state
                        , JC.jcsJcd = JC.initJobControlData
                          -- Read only fields
                        , JC.jcsOskerMVar  = oskerMVar -- Osker globals
                        , JC.jcsKcChan     = OM.OskerChannel kcChan
                        , JC.jcsFsCoreChan = OM.OskerChannel fsCoreChan
                        , JC.jcsFsRoot     = fsRoot
                        }
           )
           domGraph        -- The model updated with user halves
     -- Set up the kernel core parameters
     ; let sys2Vars = JC.first jcret
           fsVars   = JC.second jcret
           kernelCoreParameters =
             EP.ExecutiveParameters
               { EP.epProcessName    = ["KernelCore"]
               , EP.epProcessId      = 0 -- Don't care about this one
               , EP.epLocalState     = JC.jcsGps jcs
               , EP.epCallTable      = KCCT.kernelCoreCallTable
               , EP.epScreen         = KC.kernelCoreScreen
               , EP.epReject         = EP.RejectFunction KC.kernelCoreReject
               , EP.epCallTableIndex = KCM.getCallTableIndex
               , EP.epProcessNode    = error "No proc node for KC"
               }
     -- Create the kernel core. It is another version of the actor
     -- with IO Shell and executive
     -- Create MVar for the process information
     ; procMVar <- PlB.lift ( OC.newEmptyMVar "Process Info" )
     ; PO.platOut "Creating kernel core thread"
     -- Create the IO shell for the kernel core
     ; kcIoshell <- BU.myFork
                      "Kernel Core"
                         (IOS.kernelCoreIoShell
                            (JC.getProcessMap jcs)
                            kernelCoreParameters  
                            (OM.OskerChannel kcChan)  -- Input channel to exec
                            KC.kernelCoreResponseAction)
     ; KCO.kernelCoreOut "Created kernel core thread"
     ; OC.yield
     -- Update the osker globals
     ; let oskerGlobals =  OG.OskerGlobal deviceMap kcIoshell
     ; OC.swapMVar oskerMVar oskerGlobals
     -- Now that the kernel has initialized, we can start up all the
     -- processes that were initialized out of the process / domain model
     ; _tid <- B.fork "startUp" (startUpThread sys2Vars fsVars)
     ; PO.platOut " <<< initOsker"
     ; return ( N.mkNull )
     }

-- A thread to start the other threads running
startUpThread :: [BU.OneShot] -> [BU.OneShot] -> PlB.PlatformBraid ()
startUpThread sys2Vars fsVars =
  do { PlB.lift (BU.shootManyOneShot sys2Vars 0)
     ; PlB.lift (BU.shootManyOneShot fsVars 0)
     }

----------------------------------------------------------------------
--
-- Thread States:
--
--   Running: Currently running thread, which is first thread
--   in the threads list in the real world. When it pauses, it will
--   become the last thread in the list.
--
--   Paused: Resumption IO Pause, ready to run again
--
--   WaitingEmptyMVar: Sets the thread state before the Pause. The
--   threadId is recorded in the MVar, for easy look up. When a value
--   is posted to the MVar, if there is a thread id recorded there,
--   the thread state of the thread indicated is changed to Paused.
--
--   WaitingFullMVar: The thread state is set before the Pause. The
--   threadId is recorded in the MVar, for easy look up. When the
--   thread is emptied, if there is a thread id indicated, then the
--   corresponding thread is looked up, and the state of that thread
--   is changed to Paused.
--
-- Scheduling:
--
-- Yielding the current thread.
--   The nullPause operator takes the
--   new state as argument, the state of the currently running
--   thread is changed to the specified value.
--
-- Getting the next thread:
--   The scheduler looks for the first Paused thread in the
--   threads list. This thread is removed from the threads list,
--   and placed at the end of the list, with the state Running.
--   In this manner, the scheduling is round robin amongst all
--   the paused threads.
--
----------------------------------------------------------------------
