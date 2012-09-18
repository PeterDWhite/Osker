-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SystemHalfActor
    ( SystemHalfActor            -- System half version of actor
    , SystemHalfProgram          -- Complete system half program
    , SystemHalfActorState       -- Local state information
    , initSystemHalfActorState   -- Init local state
    , SystemHalfActorC           -- Packaged constructors for system half actor
    , mkSystemHalfActorC         -- Constructor for SystemHalfActorC
      -- Methods available to system half actors
    , putStrLn                   -- Write to screen
    , getFromChan                -- Get an input
    , toFileSystem               -- Send a message to the file system
    , toStandard                 -- Send to standard IO driver
    , toKernelCore               -- Send to the kernel core
    , rspKernelCore              -- Respond to the kernel core
    , toTimer                    -- Send to the timer device
      -- Action useful to system half programs
    , sysRsp                     -- Produce a response to a system call
    , processName                -- Get the process name from the state
    , AT.ActorRet (..)           -- Return value of the complete program
    ) where

-- Haskell imports
import Prelude hiding ( putStr, putStrLn )
-- Resumption imports
import qualified BoundedMonad as BdM
-- Braid imports
import qualified LocalChan as LC
-- Posix imports
import qualified SystemCall as SC
import qualified ProcessName as PN
-- Actor imports
import qualified ActorThread as AT
-- Message imports
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- Specialize the actor concept to the system half actor
----------------------------------------------------------------------

----------------------------------------------------------------------
-- The bounds on the system half actor is an explicit list of things
-- that the system half can manipulate, that may have effects outside
-- the scope of the system half kernel thread.
----------------------------------------------------------------------
data SystemHalfActorBounds =
    SystemHalfActorBounds
    { -- Input to the system half actor
      inputChan      :: LC.Chan OM.OskerPay
      -- Messages to the kernel core
    , kernelCoreChan :: LC.Chan OM.OskerPay
      -- Messages to the timer device driver
    , timerChan      :: LC.Chan OM.OskerPay
      -- Messages to the standard device driver
    , standardChan   :: LC.Chan OM.OskerPay
      -- Messages to the file system "Pid" thread
    , fsChan         :: LC.Chan OM.OskerPay
    }

instance Show SystemHalfActorBounds where
    show (SystemHalfActorBounds _ _ _ _ _) = "SystemHalfActorBounds *"

-- The special state for system half actors
type SystemHalfActorState = () -- For now

initSystemHalfActorState :: SystemHalfActorState
initSystemHalfActorState = ()

-- The system half actor bounded Monad
type SystemHalfActor a =
    BdM.BdM (AT.ActorThread SystemHalfActorState) SystemHalfActorBounds a

-- A complete system half actor program
type SystemHalfProgram = SystemHalfActor AT.ActorRet

----------------------------------------------------------------------
-- Package the constructors for the system half actor thread
----------------------------------------------------------------------
data SystemHalfActorC =
    SystemHalfActorC
    { writeSelf         :: OM.OskerPay -> SystemHalfActor ()
    , getFromChan       :: SystemHalfActor OM.OskerPay
    , toKernelCore      :: OM.KernelCoreRequest -> SystemHalfActor ()
    , rspKernelCore     :: OM.KernelCoreComply -> SystemHalfActor ()
    , toUser            :: SC.UserState      ->
                           SC.SystemResponse ->
                           SystemHalfActor SC.UserState
    , toTimer           :: OM.IOReqTimer -> SystemHalfActor ()
    , toStandard        :: OM.IOReqSIO -> SystemHalfActor ()
    , toFileSystem      :: OM.IOReqFS -> SystemHalfActor ()
    , userHalfException :: SC.UserState     ->
                           SC.UserException ->
                           SystemHalfActor SC.UserState
    , yield             :: SystemHalfActor ()
    , myThreadId        :: SystemHalfActor PN.ProcessName
      -- Debugging interfaces
    , putStrLn          :: String -> SystemHalfActor ()
    , putStr            :: String -> SystemHalfActor ()
    }

-- Get the local kernel thread Id (specialized to process name)
sys2MyThreadId :: SystemHalfActor PN.ProcessName
sys2MyThreadId = BdM.mTo AT.myThreadId

-- Get the contents of the system half input channel
sys2GetFromChan :: SystemHalfActorBounds -> SystemHalfActor OM.OskerPay
sys2GetFromChan bounds = BdM.mTo ( AT.readChan (inputChan bounds) )

-- Response to user
sys2ToUser ::
    SC.UserState        -> -- Current user state
    SC.SystemResponse   -> -- Response to inject into user state
    SystemHalfActor SC.UserState
sys2ToUser us sr = return ( us { SC.response = sr } )

-- Write to yourself
sys2WriteSelf :: SystemHalfActorBounds -> OM.OskerPay -> SystemHalfActor ()
sys2WriteSelf bounds sm = BdM.mTo ( AT.writeChan (inputChan bounds) sm)

-- Yield the processor for a while
sys2Yield :: SystemHalfActor ()
sys2Yield = BdM.mTo AT.yield

-- Write a line on standard output (for debugging only)
sys2PutStrLn :: String -> SystemHalfActor ()
sys2PutStrLn s = BdM.mTo ( AT.putStrLn s )

-- Write to the timer device
sys2ToTimer :: SystemHalfActorBounds -> OM.IOReqTimer -> SystemHalfActor ()
sys2ToTimer bounds ioreq =
  BdM.mTo ( AT.writeChan (timerChan bounds) ( OM.ToTimerDevice ioreq ) )

-- Write to the kernel core
sys2ToKernelCore ::
    SystemHalfActorBounds -> OM.KernelCoreRequest -> SystemHalfActor ()
sys2ToKernelCore bounds kcr =
    BdM.mTo ( AT.writeChan (kernelCoreChan bounds) (OM.ToKernelCore kcr) )

-- Respond to the kernel core
sys2KernelCoreRsp ::
    SystemHalfActorBounds -> OM.KernelCoreComply -> SystemHalfActor ()
sys2KernelCoreRsp bounds kcc =
    BdM.mTo ( AT.writeChan (kernelCoreChan bounds) (OM.FromSystemHalf kcc) )

-- Write to the standard IO device
sys2ToStandard :: SystemHalfActorBounds -> OM.IOReqSIO -> SystemHalfActor ()
sys2ToStandard bounds ioreq =
  BdM.mTo ( AT.writeChan (standardChan bounds) (OM.ToStandardIO ioreq ) )

-- Write to the standard IO device
sys2ToFileSystem :: SystemHalfActorBounds -> OM.IOReqFS -> SystemHalfActor ()
sys2ToFileSystem bounds ioreq =
  BdM.mTo ( AT.writeChan (fsChan bounds) (OM.ToFileSystemDD ioreq) )

sys2UserHalfException ::
    SC.UserState -> SC.UserException -> SystemHalfActor SC.UserState
sys2UserHalfException _bounds _uhe = error "sys2UserHalfException"

-- Write a line on standard output (for debugging only)
sys2OutStr :: String -> SystemHalfActor ()
sys2OutStr s = BdM.mTo ( AT.putStr s )

-- Construct a bounded IO thread from the bounds given
mkSystemHalfActorC :: SystemHalfActorBounds -> SystemHalfActorC
mkSystemHalfActorC bounds =
  SystemHalfActorC
  { getFromChan       = sys2GetFromChan       bounds
  , writeSelf         = sys2WriteSelf         bounds
  , toKernelCore      = sys2ToKernelCore      bounds
  , rspKernelCore     = sys2KernelCoreRsp     bounds
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

----------------------------------------------------------------------
-- Actions useful to the system half
----------------------------------------------------------------------
sysRsp :: Maybe SC.Errno -> SC.SpecificSystemResponse -> SystemHalfProgram
sysRsp merr specific =
  return ( AT.Response
           { AT.rsp = SC.SystemResponse { SC.srErrno    = merr
                                        , SC.srSpecific = specific
                                        }
           }
         )

-- Get the process name out of the internal state
processName :: SystemHalfActor PN.ProcessName
processName = AT.liftActor AT.actorId
