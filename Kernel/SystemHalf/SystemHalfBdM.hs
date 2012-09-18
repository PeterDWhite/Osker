-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module SystemHalfBdM
    ( SystemHalfBdM       -- Abstract data type
    , BdM.mTo             -- Convert IO to SystemHalfBIO
    , BdM.toM             -- Convert SystemHalfBIO to IO
    , SystemHalfBdMC      -- Structure of constructors for SystemHalfBIO
    , mkSystemHalfBdMC    -- Constructor for SystemHalfBdMC
    , getFromChan         -- Get input from input channel
    , writeSelf           -- Send to self
    , toKernelCore        -- Send to kernel core
    , toUser              -- System response to user
    , toTimer             -- Send to timer device
    , toStandard          -- Send to standard IO device
    , toFileSystem        -- Send to file system "Pid" thread
    , userHalfException   -- Generate exception to user half
    , putStrLn            -- For debugging
    , putStr              -- For debugging
    , yield               -- Take a break
    , myThreadId          -- Get local thread Id
    , BD.SystemHalfBounds -- Kernel IO bounds
    ) where

----------------------------------------------------------------------
--  The standard bounds for a kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (IO, putStrLn, putStr)
import qualified ThreadId as TID
-- Braid imports
import qualified BoundedMonad as BdM
import qualified ProcessThread as PT
import qualified OskerException as E
-- Actor imports
import qualified ActorThread as AT
-- Posix imports
import qualified SystemCall as SC
-- Message imports
import qualified OskerMessage as OM
-- Local imports
import qualified SystemHalfBounds as BD

-- The system half actor bounded Monad
type SystemHalfBdM a = BdM.BdM AT.ActorThread BD.SystemHalfBounds a

----------------------------------------------------------------------
-- Package the constructors for the kernel thread bounded monad
----------------------------------------------------------------------
data SystemHalfBdMC =
    SystemHalfBdMC
    { writeSelf         :: OM.OskerMsg -> SystemHalfBdM ()
    , getFromChan       :: SystemHalfBdM OM.OskerMsg
    , toKernelCore      :: OM.OskerMsg -> SystemHalfBdM ()
    , toUser            :: SC.UserState      ->
                           SC.SystemResponse ->
                           SystemHalfBdM SC.UserState
    , toTimer           :: OM.OskerMsg -> SystemHalfBdM ()
    , toStandard        :: OM.OskerMsg -> SystemHalfBdM ()
    , toFileSystem      :: OM.OskerMsg -> SystemHalfBdM ()
    , userHalfException :: SC.UserState     ->
                           SC.UserException ->
                           SystemHalfBdM SC.UserState
    , yield             :: SystemHalfBdM ()
    , myThreadId        :: SystemHalfBdM TID.ThreadId
      -- Debugging interfaces
    , putStrLn          :: String -> SystemHalfBdM ()
    , putStr            :: String -> SystemHalfBdM ()
    }

-- Get the local kernel thread Id
sys2MyThreadId :: SystemHalfBdM TID.ThreadId
sys2MyThreadId = BdM.mTo PT.myThreadId

-- Get the contents of the system half input channel
sys2GetFromChan ::
    BD.SystemHalfBounds ->       -- The bounds on the IO
    SystemHalfBdM OM.OskerMsg -- Resulting stream of messages
sys2GetFromChan bounds =
  BdM.mTo ( PT.readChan (BD.inputChan bounds) )

-- Response to user
sys2ToUser ::
    SC.UserState        -> -- Current user state
    SC.SystemResponse   -> -- Response to inject into user state
    SystemHalfBdM SC.UserState
sys2ToUser us sr = return ( us { SC.response = sr } )

-- Write to yourself
sys2WriteSelf :: BD.SystemHalfBounds -> OM.OskerMsg -> SystemHalfBdM ()
sys2WriteSelf bounds sm = BdM.mTo ( PT.writeChan (BD.inputChan bounds) sm)

-- Yield the processor for a while
sys2Yield :: SystemHalfBdM ()
sys2Yield = BdM.mTo PT.yield

-- Write a line on standard output (for debugging only)
sys2PutStrLn :: String -> SystemHalfBdM ()
sys2PutStrLn s = BdM.mTo (PT.putStrLn s)

-- Write to the timer device
sys2ToTimer :: BD.SystemHalfBounds -> OM.OskerMsg -> SystemHalfBdM ()
sys2ToTimer bounds sm = BdM.mTo ( PT.writeChan (BD.timerChan bounds) sm )

-- Write to the kernel core
sys2ToKernelCore :: BD.SystemHalfBounds -> OM.OskerMsg -> SystemHalfBdM ()
sys2ToKernelCore bounds sm =
  BdM.mTo ( PT.writeChan (BD.kernelCoreChan bounds) sm )

-- Write to the standard IO device
sys2ToStandard :: BD.SystemHalfBounds -> OM.OskerMsg -> SystemHalfBdM ()
sys2ToStandard bounds sm = BdM.mTo ( PT.writeChan (BD.standardChan bounds) sm )

-- Write to the standard IO device
sys2ToFileSystem :: BD.SystemHalfBounds -> OM.OskerMsg -> SystemHalfBdM ()
sys2ToFileSystem bounds sm = BdM.mTo ( PT.writeChan (BD.fsChan bounds) sm )

sys2UserHalfException ::
    SC.UserState -> SC.UserException -> SystemHalfBdM SC.UserState
sys2UserHalfException _bounds _uhe = error "sys2UserHalfException"

-- Write a line on standard output (for debugging only)
sys2OutStr :: String -> SystemHalfBdM ()
sys2OutStr s = BdM.mTo (PT.putStr s)

-- Construct a bounded IO thread from the bounds given
mkSystemHalfBdMC :: BD.SystemHalfBounds -> SystemHalfBdMC
mkSystemHalfBdMC bounds =
  SystemHalfBdMC
  { getFromChan       = sys2GetFromChan       bounds
  , writeSelf         = sys2WriteSelf         bounds
  , toKernelCore      = sys2ToKernelCore      bounds
  , toTimer           = sys2ToTimer           bounds
  , toStandard        = sys2ToStandard        bounds
  , toFileSystem      = sys2ToFileSystem      bounds
  , userHalfException = sys2UserHalfException
  , toUser            = sys2ToUser
  , yield             = sys2Yield
  , myThreadId        = sys2MyThreadId
  , putStrLn          = sys2PutStrLn
  , putStr            = sys2OutStr
  }
