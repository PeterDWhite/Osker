-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module TimerDevice ( timerDevice  ) where

----------------------------------------------------------------------
-- The timer device driver
-- Inputs to the timer device driver:
-- DyanmicException (ExecutiveMessage): timer start, cancel, or status
-- UserException "tick":                A timer tick
--
-- This version of the timer device uses RIO (Resumption IO), but
-- no bounded IO Monad around it. We will upgrage the device drivers
-- to use bounded IO soon....
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (putStrLn)
import Maybe
import Monad
-- Braid imports
import qualified DomainBraid as DB
-- Osker imports
import qualified OskerMessage as OM
import qualified TimerBd as TB

----------------------------------------------------------------------
-- Ths usual implementation, a list of TCBs, each one has a delta
-- time from the last.
----------------------------------------------------------------------
data DeltaTime =
    DeltaTime
    { dtDelta :: Int            -- Delta from previous timer
    , dtMsg   :: OM.OskerRspMsg -- Containing the TCB
    }

instance Show DeltaTime where
    show dt = "{" ++ show (dtDelta dt) ++ ", " ++ show (dtMsg dt) ++ "}"

type TimersRunning = [DeltaTime]

initTimersRunning :: TimersRunning
initTimersRunning = []

----------------------------------------------------------------------
-- Inserting a timer into the list. Each timer has a delta from
-- the previous timer in the list
----------------------------------------------------------------------
insertTimer' ::
    Int             ->      -- Tick count accumulator
    TimersRunning   ->      -- Structure to insert in
    OM.OskerRspMsg  ->      -- Containing the TCB
    TimersRunning           -- Updated structure
insertTimer' count [] sm = [DeltaTime count sm]
insertTimer' count (dt:dts) sm =
  if count <= dtDelta dt
  then let dt' = dt { dtDelta = dtDelta dt - count }
           newdt = DeltaTime { dtDelta = count
                             , dtMsg   = sm
                             }
       in newdt:dt':dts
  else dt:(insertTimer' (count - dtDelta dt) dts sm)

insertTimer ::
    TimersRunning   ->   -- Structure to insert in
    OM.OskerRspMsg  ->   -- Info for final response
    TimersRunning        -- Updated structure
insertTimer timers sm =
  let tcb = getTCB sm
  in insertTimer' (OM.tcbTickCount tcb) timers sm

----------------------------------------------------------------------
-- Deleting a timer from the list
----------------------------------------------------------------------
deleteTimer ::
    TimersRunning        ->  -- Structure to delete from
    OM.TimerControlBlock ->  -- TCB to delete
    TimersRunning            -- Updated structure
deleteTimer [] _tcb = []
deleteTimer (dt:dts) tcb =
  if OM.tcbProcessName tcb == OM.tcbProcessName (getTCB (dtMsg dt))
  then dts
  else dt:(deleteTimer dts tcb)

----------------------------------------------------------------------
-- The timer device driver is completely event driven
----------------------------------------------------------------------
timerDevice :: TB.TimerBdC -> TB.TimerBd DB.DomainRet
timerDevice b =
  do { tid <- TB.myThreadId b
     ; ddOut b "Timer" ("Starting (" ++ show tid ++ ")")
     ; timerDevice' b initTimersRunning
     }

timerDevice' ::
    TB.TimerBdC   ->        -- Bounds on the kernel thread
    TimersRunning ->        -- Local state info
    TB.TimerBd DB.DomainRet -- Bounded kernel thread action
timerDevice' b timers =
  do { timeReq <- TB.getFromChan b
     ; timers' <- processTimerRequest b timers timeReq
     ; timerDevice' b timers'
     }

-- Process many timer requests
processTimerRequest ::
    TB.TimerBdC     ->        -- Bounds on the thread
    TimersRunning   ->        -- Local state info
    OM.OskerRspMsg  ->        -- Current input message
    TB.TimerBd TimersRunning  -- Bounded kernel thread action
processTimerRequest b timers sm =
  let pay = OM.payOfM sm
  in case pay of
       OM.TimerTick ->
         do { -- Look at the timer on the front of the list, and
              -- decrement the count by one. If it expires, leap into
              -- action by sending the expiration back to the kernel
              -- thread that requested the timer.
            ; if null timers
              then return timers
              else let first = head timers
                       rest  = tail timers
                       delta = dtDelta first - 1
                       first' = first { dtDelta = delta }
                       timers' = first':rest
                   in if delta == 0
                      then do { -- Format the expiration message, and
                                -- send it back to client kernel thread.
                              ; ddOut b "timerExc" "Timer expiration"
                              ; let iorsp = OM.FromTimerDevice
                                             (OM.StartTimerResponse (Just 0))
                              ; (TB.respond b) (dtMsg first) iorsp
                              ; return timers'
                              }
                      else return timers'
         }
       OM.ToTimerDevice timerreq ->
         case timerreq of
           OM.StartTimerRequest { OM.strTCB = tcb } ->
             -- Add the new timer to the list
             let newtimers = insertTimer
                               timers
                               sm
             in do { ddOut b "Timer" ("Adding: " ++ show tcb ++
                                      "\n" ++ show newtimers ++
                                      "\n\t" ++ show sm)
                   ; return newtimers
                   }
           OM.CancelTimerRequest tcb _pname ->
             let newtimers = deleteTimer timers tcb
             in do { ddOut b "Timer" ("Deleting: " ++ show tcb ++
                                      "\n" ++ show newtimers)
                   ; return newtimers
                   }
       _otherwise ->
         error ("Bogus message to timer device: " ++ show sm)

-- Process many timer requests
processTimerRequests ::
    TB.TimerBdC       ->      -- Bounds on the kernel thread
    TimersRunning     ->      -- Local state info
    [OM.OskerRspMsg]  ->      -- Timer requests to process
    TB.TimerBd TimersRunning  -- Bounded kernel thread action
processTimerRequests _b _timers [] =
  error "processTimerRequests: Infinite lazy list"
processTimerRequests b timers (sm:sms) =
  do { processTimerRequest b timers sm
     ; processTimerRequests b timers sms
     }

-- Print outs from timer device drivers
ddOut ::
    TB.TimerBdC -> -- Bounds on the IO
    String      -> -- Name of device driver wanting print out
    String      -> -- String to print
    TB.TimerBd ()  -- This is a kernel thread action
ddOut b ddname s = (TB.putStrLn b) ("***[DD." ++ ddname ++ "]...\t\t" ++ s)

-- Break the TCB out of the Osker message
getTCB :: OM.OskerRspMsg -> OM.TimerControlBlock
getTCB sm =
  let pay = OM.payOfM sm
  in case pay of
       OM.ToTimerDevice timerreq ->
         case timerreq of
           OM.StartTimerRequest { OM.strTCB = tcb } -> tcb
           _else -> error ("getTCB: Bad msg payload: " ++ show sm)
       _otherwise -> error ("getTCB: Bad Osker msg: " ++ show sm)
