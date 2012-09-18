-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SignalPrograms
    ( sigactionProgram    -- Actor for Posix sigaction()
    , sigprocmaskProgram  -- Actor for Posix sigprocmask()
    , sigpendingProgram   -- Actor for Posix sigpending()
    , killProgram         -- Actor for Posix kill()
    , pauseProgram        -- Actor for Posix pause()
    , sigsuspendProgram   -- Actor for Posix sigsuspend()
    , alarmProgram        -- Actor for Posix alarm()
    , sleepProgram        -- Actor for Posix sleep()
    , receiveProgram      -- Actor for Posix receive()
    ) where

----------------------------------------------------------------------
-- The POSIX signal system calls
----------------------------------------------------------------------

-- Posix imports
import qualified SystemCall as SC
import qualified ProcessName as PN
import qualified Errno as ER
-- Kernel globals
import qualified TimerControlBlock as TCB
import qualified ExceptionData as ED
-- Actor imports
import qualified ActorThread as AT
import qualified SignalData as SIGD
-- Osker imports
import qualified SystemHalfActor as SHA
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The pause system call:
-- Suspend the caller until a signal is received. If the signal
-- is caught, then the catcher is run. If the catcher returns,
-- then pause returns a -1. If the signal is not caught, the
-- caller is terminated.
----------------------------------------------------------------------
pauseProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    SC.SignalMap         -> -- Map signals to their actions
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
pauseProgram b _sigmap _pay =
  do { ( SHA.putStrLn b ) ("pauseProgram")
       -- Save the response message context away in the pause
       -- condition component of the local fractured state.
     ; return ( AT.PauseResponse )
     }

----------------------------------------------------------------------
-- sigaction system call
--
-- Used to set a catcher for a signal. When the sigaction parameter
-- is null, it is used to fetch the current setting for the signal.
----------------------------------------------------------------------
sigactionProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    SC.SignalMap         -> -- Map signals to their actions
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
sigactionProgram b sigmap pay =
  case OM.getSysReq pay of
    SC.SigActionReq signal sigaction ->
      case sigaction of
        SC.NullSigAction -> -- Return the old value of the signal action
          do { -- Dig up the action map
             ; ( SHA.putStrLn b ) ("sigaction/NullSigAction")
             ; let current = SC.getSigAction signal sigmap
             ; ( SHA.putStrLn b ) ("current = " ++ show current)
             ; SHA.sysRsp SC.noError (SC.SigActionRsp current)
             }
        SC.SigAction _handler _mask _flags -> -- Update the signal action map
          do { ( SHA.putStrLn b) ( "sigaction/SigAction: " ++ show signal ++
                                   "..>" ++ show sigaction )
             ; let current = SC.getSigAction signal sigmap
                   sigmap' = SC.updateSignalMap
                               sigmap signal (SC.Catch sigaction)
               in sigRsp sigmap' SC.noError (SC.SigActionRsp current)
             }
    _otherwise -> error ("sigaction, bad request: " ++ show pay)

----------------------------------------------------------------------
-- sigprocmask system call
-- Change the signal mask for the process
----------------------------------------------------------------------
sigprocmaskProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    SC.SignalMask        -> -- The current signal mask
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
sigprocmaskProgram _b sigmask pay =
  case OM.getSysReq pay of
    SC.SigProcMaskReq how sigparamset ->
      case sigparamset of
        SC.NullSignalSetParam ->
          SHA.sysRsp SC.noError (SC.SigProcMaskRsp sigmask)
        SC.SignalSetParam sigset ->
          case how of
            SC.SIG_BLOCK ->
              let newsigset = SC.sigunion sigmask sigset
              in sigProcRsp newsigset SC.noError (SC.SigProcMaskRsp sigmask)
            SC.SIG_UNBLOCK ->
              let newsigset = SC.sigintersect
                                sigmask (SC.sigcomplement sigset)
              in sigProcRsp newsigset SC.noError (SC.SigProcMaskRsp sigmask)
            SC.SIG_SETMASK ->
              sigProcRsp sigset SC.noError (SC.SigProcMaskRsp sigmask)
    _otherwise -> error "sigprocmask: Busted system half state"

----------------------------------------------------------------------
-- sigpending system call
-- Return the set of signals that have a signal pending
----------------------------------------------------------------------
sigpendingProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    SC.SignalSet         -> -- Pending signals
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
sigpendingProgram _b pending _pay =
  SHA.sysRsp SC.noError (SC.SigPendingRsp pending)

----------------------------------------------------------------------
-- kill() system call
----------------------------------------------------------------------
killProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
killProgram b pay =
  do { ( SHA.putStrLn b ) ("killProgram")
     ; let SC.KillReq pid signal = OM.getSysReq pay
       -- Forward the request on to the kernel core
     ; ( SHA.toKernelCore b ) ( OM.SignalProcess pid signal )
     ; kcrsp <- SHA.getFromChan b
     ; let OM.FromKernelCore (OM.SignalProcessResponse flag) = kcrsp
           ret = if flag then 0 else -1
     -- **** Need to set error cdoe 
     ; ( SHA.putStrLn b ) "Received response from kernel core"
     ; SHA.sysRsp SC.noError (SC.KillRsp ret)
     }

sigsuspendProgram :: SHA.SystemHalfProgram
sigsuspendProgram = error "sigsuspendProgram"

----------------------------------------------------------------------
-- alarm starts a timer, and gives a SIGALRM when the alarm goes off.
----------------------------------------------------------------------
alarmProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    Maybe TCB.TimerControlBlock -> -- Alarm TCB, when alarm is pending
    TCB.TimerControlBlock -> -- Obtain new resource before calling this
    SC.SignalMap         -> -- Map signals to their actions
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
alarmProgram b malarm tcb sigmap pay =
  do { let SC.AlarmReq t = OM.getSysReq pay
     ; pn <- SHA.processName
     ; if t == 0
       then -- Cancel existing timer
            do { case malarm of
                   Nothing ->
                     -- No alarm is currently scheduled
                     -- NOTE: The response value in this case is not
                     -- specified in POSIX.1.
                     SHA.sysRsp SC.noError (SC.AlarmRsp Nothing)
                   Just tcb' ->
                     -- There is an alarm, the parameter 0 means to
                     -- cancel the alarm.
                     do { ( SHA.toTimer b ) ( OM.CancelTimerRequest
                                              { OM.ctrTCB = tcb'
                                              , OM.ctrProcessName = pn
                                              }
                                            )
                        ; timrsp <- SHA.getFromChan b
                        ; SHA.sysRsp SC.noError (SC.AlarmRsp Nothing)
                        }
               }
       else -- Start a new timer
            do { case malarm of
                   Nothing -> -- No alarm is currently scheduled
                     startNewAlarm b t 0 pn tcb sigmap
                   Just tcb' ->
                     do { ( SHA.toTimer b ) ( OM.CancelTimerRequest
                                              { OM.ctrTCB = tcb'
                                              , OM.ctrProcessName = pn
                                              }
                                            )
                        ; timrsp <- SHA.getFromChan b
                        ; startNewAlarm
                            b t (TCB.tcbTickCount tcb) pn tcb' sigmap
                        ; SHA.sysRsp SC.noError (SC.AlarmRsp Nothing)
                        }
               }
     }

-- Warning: Currently only handles case where there is local TCB available.
startNewAlarm ::
    SHA.SystemHalfActorC  -> -- Bounds on the program
    Int                   -> -- Time of new alarm
    Int                   -> -- Time of existing alarm
    PN.ProcessName        -> -- Process name of containing process
    TCB.TimerControlBlock -> -- Obtain new resource before calling this
    SC.SignalMap          -> -- Map signals to their actions
    SHA.SystemHalfProgram    -- In the system half program monad
startNewAlarm b t existingT pname tcb sigmap =
  do { ( SHA.putStrLn b ) ("TCB! for " ++ PN.outProcessName pname)
     ; let tcb' = tcb { TCB.tcbProcessName = pname
                      , TCB.tcbTickCount   = t
                      }
       -- Send the TCB along to the timer device
     ; ( SHA.toTimer b )( OM.StartTimerRequest { OM.strTCB =  tcb' } )
     ; timrsp <- SHA.getFromChan b
     ; ( SHA.putStrLn b ) "Actor gets timer expiration"
     ; let sigaction = SC.getSigAction SC.SIGALRM sigmap
           -- And a delayed exception
           excData = ED.mkExceptionData SC.SIGALRM sigaction
           rsppay  = SC.SystemResponse SC.noError
                                       (SC.AlarmRsp (Just existingT))
     ; return ( AT.StartAlarmResponse { AT.rsp = rsppay
                                      , AT.tcb = Just tcb'
                                      , AT.ed  = excData
                                      }
              )
       -- How to handle the excpetion ???????
     }

----------------------------------------------------------------------
-- Sleep for the specified number of seconds
----------------------------------------------------------------------
sleepProgram :: SHA.SystemHalfProgram
sleepProgram = error "sleepProgram"

----------------------------------------------------------------------
-- receive a signal from another process
-- Unlike the other programs, this one is initiated by the kernel
-- core, via an activity initiated to another system half, by
-- another user process.
----------------------------------------------------------------------
receiveProgram ::
    SHA.SystemHalfActorC -> -- Bounds on the program
    SIGD.PauseCondition  -> -- Is there a pause condition?
    SC.SignalMap         -> -- Map signals to their actions
    OM.OskerPay          -> -- Payload with the system request
    SHA.SystemHalfProgram
receiveProgram b pause sigmap pay =
  do { ( SHA.putStrLn b ) ( "Receive Program: " ++ show pay )
     ; let OM.ReceiveSignal _pid signal = OM.getKernelCoreCommand pay
       -- Dig up the signal action map
     ; let signalAction = SC.getSigAction signal sigmap
           excData      = ED.mkExceptionData signal signalAction
     ; ( SHA.rspKernelCore b ) (OM.ReceiveSignalComply True)
     ; case signalAction of
         SC.NullSignalAction -> error "receiveProgram: NullSignalAction"
         SC.TerminateProcess -> error "TerminateProcess"
         SC.IgnoreSignal ->
            -- Ignoring the signal is easy, just do nothing
            -- But we still owe a response to the kernel core.
            do { ( SHA.rspKernelCore b ) ( OM.ReceiveSignalComply True )
               ; return ( AT.NullReturn )
               }
         SC.ContinueProcess ->
            -- Generate a return from the pause
            -- First check if there is a pause condition
            if SIGD.isPause pause
            then -- No pause condition, cannot continue where
                 -- there is no pause, so just ignore the
                 -- signal. But we still owe a response to the kernel core
                 do { ( SHA.rspKernelCore b ) ( OM.ReceiveSignalComply True )
                    ; return ( AT.NullReturn )
                    }
            else do { ( SHA.rspKernelCore b ) ( OM.ReceiveSignalComply True )
                    ; return ( AT.NullReturn )
                    }
         SC.Catch sigAction ->
            case sigAction of
              SC.NullSigAction -> error "receiveProgram: NullSigAction"
              SC.SigAction _handler _mask _flags ->
               do {  -- First check if there is a pause condition
                  ; if SIGD.isPause pause
                    then -- Need to stop the pause
                         return ( AT.ReceiveResponse { AT.ed = excData } )
                    else -- *** More here ****
                         return ( AT.ReceiveResponse { AT.ed = excData } )
                  }
     }

----------------------------------------------------------------------
-- Actions useful to the message queue programs
----------------------------------------------------------------------
sigRsp ::
    SC.SignalMap              -> -- Map signals to their actions
    Maybe SC.Errno            -> -- Error return, if any
    SC.SpecificSystemResponse -> -- Response specific to the request
    SHA.SystemHalfProgram        -- In the system half program monad
sigRsp sigmap merr specific =
  return ( AT.SigResponse
           { AT.sigmap = sigmap
           , AT.rsp    = ( SC.SystemResponse { SC.srErrno    = merr
                                             , SC.srSpecific = specific
                                             }
                         )
           }
         )

sigProcRsp ::
    SC.SignalSet              -> -- Collection of signals
    Maybe SC.Errno            -> -- Error return, if any
    SC.SpecificSystemResponse -> -- Response specific to the request
    SHA.SystemHalfProgram        -- In the system half program monad
sigProcRsp sigset merr specific =
  return ( AT.SigProcResponse
           { AT.sigset = sigset
           , AT.rsp    = ( SC.SystemResponse { SC.srErrno    = merr
                                             , SC.srSpecific = specific
                                             }
                         )
           }
         )
