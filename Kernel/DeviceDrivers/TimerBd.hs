-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module TimerBd
    ( TimerBd         -- Abstract data type
    , TimerBdC         -- Structure of constructors for FileSystemBIO
    , mkTimerBdC       -- Constructor for FileSystemBio
    , respond          -- Respond to an incoming message
    , getFromChan      -- Get input from input channel
    , isChanEmpty      -- Determine if input channel is empty
    , yield            -- Take a break
    , myThreadId       -- Get running thread Id
    , getProcessTimes  -- Get information about timing
    , putStrLn         -- For debugging
    , putStr           -- For debugging
    , TimerBounds (..) -- Kernel IO bounds
    , BM.toM           -- Convert bounded m to plain m
    ) where

----------------------------------------------------------------------
--  The standard bounds for a kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (IO, putStrLn, putStr)
import qualified PosixProcEnv as P
-- Braid imports
import qualified BoundedMonad as BM
import qualified ProcessThread as PT
import qualified LocalChan as C
-- Osker imports
import qualified OskerMessage as OM
import qualified ThreadId as TID

-- The bounds on the standard IO thread.
-- The standard IO thread can receive messages and respond
-- to them.
data TimerBounds = TimerBounds { inputChan :: OM.OskerChannel }

instance Show TimerBounds where
    show _tb = "TimerBounds*"

-- The bounded IO Monad for the file system "Pid" threads
type TimerBd a = BM.BdM PT.ProcessThread TimerBounds a

----------------------------------------------------------------------
-- Package the constructors for the kernel thread bounded IO for
-- the standardIO kernel thread
----------------------------------------------------------------------
data TimerBdC =
    TimerBdC
    { -- A response to a file system "Pid" thread
      respond         :: OM.OskerRspMsg -> OM.OskerRspPay -> TimerBd ()
      -- Read the input channel
    , getFromChan     :: TimerBd OM.OskerRspMsg
      -- Determine if the input channel is empty
    , isChanEmpty     :: TimerBd Bool
    , yield           :: TimerBd ()
    , myThreadId      :: TimerBd TID.ThreadId
      -- Timing information
    , getProcessTimes :: TimerBd P.ProcessTimes
      -- Debugging interfaces
    , putStrLn        :: String -> TimerBd ()
    , putStr          :: String -> TimerBd ()
    }

-- Get the contents of the system half input channel
timGetProcessTimes :: TimerBd P.ProcessTimes
timGetProcessTimes = PT.liftIO P.getProcessTimes

-- Get the contents of the system half input channel
timGetFromChan :: TimerBounds -> TimerBd OM.OskerRspMsg
timGetFromChan bounds = BM.mTo ( PT.readChan ( OM.projPC (inputChan bounds) ) )

-- Determine if the channel is empty
timIsChanEmpty :: TimerBounds -> TimerBd Bool
timIsChanEmpty bounds =
  BM.mTo ( PT.isEmptyChan ( OM.projPC (inputChan bounds) ) )

-- Response to system half "Pid" thread
timRespond :: OM.OskerRspMsg -> OM.OskerRspPay -> TimerBd ()
timRespond sm sp = BM.mTo ( OM.respondMessage sm sp )

-- Yield the processor for a while
timYield :: TimerBd ()
timYield = BM.mTo ( PT.yield )

-- Get running thread Id
timMyThreadId :: TimerBd (TID.ThreadId)
timMyThreadId = BM.mTo ( PT.myThreadId )

-- Write a line on standard output (for debugging only)
timPutStrLn :: String -> TimerBd ()
timPutStrLn s = BM.mTo ( PT.putStrLn s )

-- Write a line on standard output (for debugging only)
timPutStr :: String -> TimerBd ()
timPutStr s = BM.mTo ( PT.putStr s )

-- Construct a bounded IO thread from the bounds given
mkTimerBdC ::
    TimerBounds -> -- Bounds on the thread IO
    TimerBdC       -- The bounded IO thread
mkTimerBdC bounds =
  TimerBdC
  { respond           = timRespond
  , getFromChan       = timGetFromChan bounds
  , isChanEmpty       = timIsChanEmpty bounds
  , yield             = timYield
  , myThreadId        = timMyThreadId
  , getProcessTimes   = timGetProcessTimes
  , putStrLn          = timPutStrLn
  , putStr            = timPutStr
  }
