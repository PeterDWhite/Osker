-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SignalIF
    ( -- Signal data structures
    , SC.Signal (..)           -- Posix signals
    , SC.SignalMask            -- Set of masked signals
    , SC.SigAction (..)        -- Implementing action for when signal arrives
    , SC.SignalSet             -- A set of signals
    , SC.SignalMap             -- Map from signal to signal action
    , SC.SignalAction (..)     -- Possible signal actions
    , SC.hasHandler            -- Check if signal action has a handler
    -- Library signal interfaces
    , SC.sigemptyset           -- An empty signal set
    , SC.sigfillset            -- A set with all signals in it
    , SC.sigaddset             -- Add an element to a signal set
    , SC.sigdelset             -- Delete an element from a signal set
    , SC.sigismember           -- Determine if a signal is in a set
    -- Osker signal interfaces
    , sigaction                -- Update a signal action
    , SC.SigProcMaskParam (..) -- How to change the signal mask
    , SC.SignalSetParam (..)   -- The set of signals to update
    , sigprocmask              -- Change the signal mask for a process
    , sigpending               -- Determine pending signal set
    , kill                     -- Send a signal
    , pause                    -- Halt until a signal comes in
    , sigsuspend               -- Halt until a signal comes in
    , alarm                    -- Start a timer
    , sleep                    -- Sleep for specified number of seconds
    ) where

----------------------------------------------------------------------
-- The signal interfaces implemented in the Osker kernel.
--
-- Osker Notes:
-- We cannot yet emulate the behaviour of interrupting the user
-- process at any point, running an interrupt handler, and later
-- returning to the point of interruption. Later, we will experiment
-- with continuation passing style to emulate this in Haskell. When
-- we build the C language interfaces to Osker, emulating this
-- should be no problem. For Haskell user processes, there is a
-- problem. For Haskell user processes, we will run the specified
-- IO action in response to a signal. It will be the responsibility
-- of the IO action to pick up the pieces and continue, if desired.
--
-- When a signal is delivered to a process, and the process is
-- currently executing a system call, the system call should return
-- -1 and set errno to EINTR. We will not emulate this behaviour
-- until we do the C interface, and we have an instance of Osker
-- in which all system calls are blocking.
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (IO)
-- Posix imports
import qualified SystemCall as SC
import qualified ProcessId as PID

----------------------------------------------------------------------
-- sigaction updates the signal action for the specified signal.
-- If the input sigaction is Null, then the old sigaction is returned.
-- Otherwise the output is the previous sigaction for the signal.
-- Because Posix does not define what to return when there is no
-- catcher defined, we have expanded the return value to include
-- these cases. This return value can be converted into the Posix
-- standard, using perhaps a zero pointer when there is no catcher.
----------------------------------------------------------------------
sigaction ::
    SC.Signal    ->       -- The signal for which action is specified
    SC.SigAction ->       -- The new signal action
    SC.U SC.SystemResponse -- Sometimes returns the existing sigaction
sigaction sig action = SC.osker (SC.SigActionReq sig action)

----------------------------------------------------------------------
-- sigprocmask changes the signal mask for the process. How it is
-- changed is specified by the SigProcMaskParam.
--
-- The action that is in force at the time the signal is delivered
-- is the one that is used. So if sigaction changes the action
-- during the blocked time, the new action (set by sigaction) will
-- be used.
--
-- Exception 1:
-- If the signal is blocked, but the action for the signal is to
-- ignore, then the signal will be discarded (this exception is
-- allowed by Posix)
-- If the action for a signal is changed to IGNORE, and there is
-- a signal pending, the pending signal is discarded.
----------------------------------------------------------------------
sigprocmask ::
    SC.SigProcMaskParam ->  -- How to change the signal mask
    SC.SignalSetParam   ->  -- The new signal set
    SC.U SC.SystemResponse   -- The old signal set
sigprocmask sigprocmaskparam sigsetparam =
  SC.osker (SC.SigProcMaskReq sigprocmaskparam sigsetparam)

----------------------------------------------------------------------
-- sigpending returns the set of signals for which there are
-- pending signals that are now blocked.
----------------------------------------------------------------------
sigpending :: SC.U SC.SystemResponse -- The old signal set is 
sigpending = SC.osker SC.SigPendingReq

----------------------------------------------------------------------
-- kill is the badly named function in Posix to send a signal
-- kill gets its name because the most common purpose for sending
-- a signal is to terminate another process.
--
--   If the pid is positive, the signal is sent to the process with
--   that pid.
--
--   If the pid is zero, the signal is sent to all processes with
--   the same process group Id as the caller. **** NOT IMPLEMENTED
--   YET ***** (Waiting for process control)
--
--   If pid is -1, then Posix does not specify the behaviour. Osker
--   will reject this argument with an error code.
--
--   If pid is negative and not -1, then the absolute value of the
--   pid is treated as a process group Id, and the signal is sent
--   to all members of that process group for which the caller has
--   permission to send a signal. ***** NOT IMPLEMENTED YET *****
--   (Waiting for process control implementation)
--
-- The signal will be sent if the sender has permission to send
-- to the receiver. Under Posix, the permission is granted if the
-- real or effective user ID of the sender is equal to the real or
-- effective user ID of the receiver.  (**** NOT IMPLEMENTED YET
-- The implementation of the permissions checking will await the
-- implementation of process control for Osker *****
--
-- If the signal value is zero, then the permissions are checked,
-- but the signal is not actually sent. This is a way of finding
-- out if a process with the specified PID exists.
--
-- ***** Posix.1 implementations are free to impose further security
-- restrictions on the sending of signals *****
--
----------------------------------------------------------------------
kill ::
    PID.ProcessId ->       -- Who to signal
    SC.Signal     ->       -- What to signal
    SC.U SC.SystemResponse -- Set to -1 if there was an error
kill pid signal = SC.osker (SC.KillReq pid signal)

----------------------------------------------------------------------
-- The pause interface halts the process until a signal occurs.
--
-- If the signal is caught (a catcher has been established by
-- the sigatction() call), and if the signal catcher function
-- returns, then a -1 is returned by pause(), and errno is set
-- to EINTR. If the signal is not caught, then the caller is
-- terminated, therefore pause() does not return in this case.
----------------------------------------------------------------------
pause :: SC.U SC.SystemResponse -- No return value is required
pause = SC.osker SC.PauseReq

----------------------------------------------------------------------
-- The sigsuspend interface halts the process until a signal occurs,
-- with a specified mask in place during the wait.
-- This function is an extension to the pause() function.
----------------------------------------------------------------------
sigsuspend ::
    SC.SignalSet ->        -- Signals to be masked during wait
    SC.U SC.SystemResponse -- Error indication
sigsuspend sigset = SC.osker (SC.SigSuspendReq sigset)

----------------------------------------------------------------------
-- The alarm () interface starts a timer for the specified number
-- of seconds. It will generate a SIGALRM signal after the specified
-- number of seconds has elapsed.
-- A parameter of zero cancels any outstanding alarm.
-- This is implemented for now as a blocking system call, meaning
-- that the user half will block until the timer is scheduled.
-- The return value of alarm is the number of seconds left on any
-- previously scheduled alarm.
-- Only one alarm is remembered, no stacking.
-- An argument of zero seconds reports the number of seconds
-- left on the current alarm.
----------------------------------------------------------------------
alarm ::
    Int           ->       -- Number of seconds
    SC.U SC.SystemResponse -- Number of seconds remaining, if successful
alarm t = SC.osker (SC.AlarmReq t)

----------------------------------------------------------------------
-- The sleep call is a pause with a timeout
----------------------------------------------------------------------
sleep ::
    Int           ->       -- Number of seconds
    SC.U SC.SystemResponse -- Error indication
sleep t = SC.osker (SC.SleepReq t)
