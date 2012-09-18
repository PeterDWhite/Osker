-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module StandardIOBd
    ( StandardIOBd          -- Abstract data type
    , StandardIOBdC         -- Structure of constructors for FileSystemBIO
    , mkStandardIOBdC       -- Constructor for FileSystemBio
    , respond               -- Respond to an incoming message
    , getFromChan           -- Get input from input channel
    , isChanEmpty           -- Determine if input channel is empty
    , toExternal            -- Write to external world
    , isExtEmpty            -- Is external input channel empty
    , fromExternal          -- Read from external input channel
    , checkExternal         -- Return external input, if there is one
    , yield                 -- Take a break
    , putStrLn              -- For debugging
    , putStr                -- For debugging
    , getOskerIn            -- Temporary only
    , StandardIOBounds (..) -- Kernel IO bounds
    , BM.toM                -- Convert bounded m to plain m
    , checkExternal'        -- TEMP TEMP TEMP EXPORT
    ) where

----------------------------------------------------------------------
--  The standard bounds for a kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding ( putStr, putStrLn )
import qualified Concurrent as C
-- Braid imports
import qualified ProcessThread as PT
import qualified BoundedMonad as BM
import qualified LocalChan as LC
-- Posix imports
import qualified ProcessName as PN
import qualified SystemCall as SC
-- Osker imports
import qualified OskerMessage as OM
import qualified DemoIF as DIF

-- The bounds on the standard IO thread.
-- The standard IO thread can receive messages and respond
-- to them.
data StandardIOBounds =
    StandardIOBounds
    { inputChan :: OM.OskerChannel          -- Channel open to system halves
    , oskerOut  :: C.Chan (DIF.OskerOutput) -- External output channel
    , oskerIn   :: C.Chan (DIF.OskerInput)  -- Extern input channel
    }

instance Show StandardIOBounds where
    show _siob = "StandardIOBounds*"

-- The bounded IO Monad for the file system "Pid" threads
type StandardIOBd a = BM.BdM PT.ProcessThread StandardIOBounds a

----------------------------------------------------------------------
-- Package the constructors for the kernel thread bounded IO for
-- the standardIO kernel thread
----------------------------------------------------------------------
data StandardIOBdC =
    StandardIOBdC
    { -- A response to a file system "Pid" thread
      respond        :: OM.OskerRspMsg -> OM.OskerRspPay -> StandardIOBd ()
      -- Read the input channel
    , getFromChan    :: StandardIOBd OM.OskerRspMsg
      -- Determine if the input channel is empty
    , isChanEmpty    :: StandardIOBd Bool
    , toExternal     :: PN.ProcessName -> SC.SystemResponse -> StandardIOBd ()
    , isExtEmpty     :: StandardIOBd Bool
    , fromExternal   :: StandardIOBd DIF.OskerInput
    , checkExternal  :: StandardIOBd (Maybe DIF.OskerInput)
    , yield          :: StandardIOBd ()
      -- Debugging interfaces
    , putStrLn       :: String -> StandardIOBd ()
    , putStr         :: String -> StandardIOBd ()
      -- Temporary only
    , getOskerIn     :: C.Chan (DIF.OskerInput)
    , checkExternal' :: IO (Maybe DIF.OskerInput)
    }

-- Get the contents of the system half input channel
sioGetFromChan ::
    StandardIOBounds ->           -- The bounds on the IO
    StandardIOBd OM.OskerRspMsg -- Resulting stream of messages
sioGetFromChan bounds = BM.mTo ( PT.readChan ( OM.projPC (inputChan bounds) ) )

-- Determine if the channel is empty
sioIsChanEmpty :: StandardIOBounds -> StandardIOBd Bool
sioIsChanEmpty bounds =
  BM.mTo ( PT.inPT ( LC.isEmptyChan (OM.projPC (inputChan bounds) ) ) )

-- Write to external channel
sioToExternal ::
    StandardIOBounds  ->   -- The bounds on the IO
    PN.ProcessName    ->   -- Source of the response
    SC.SystemResponse ->   -- The response
    StandardIOBd ()        -- Action in the bounded IO monad
sioToExternal bounds pn sysrsp =
  let po = DIF.OskerOutput { DIF.source = pn
                           , DIF.output = sysrsp
                           }
  in PT.liftIO (C.writeChan (oskerOut bounds) po)

-- Determine if external input is empty
sioIsExtEmpty ::
    StandardIOBounds ->   -- The bounds on the IO
    StandardIOBd Bool     -- Action in the bounded IO monad
sioIsExtEmpty bounds = PT.liftIO (C.isEmptyChan $! (oskerIn bounds))

-- Read from the external input
sioFromExternal ::
    StandardIOBounds ->          -- The bounds on the IO
    StandardIOBd DIF.OskerInput -- Action in the bounded IO monad
sioFromExternal bounds = PT.liftIO (C.readChan $! (oskerIn bounds))

sioCheckExternal' :: StandardIOBounds -> IO (Maybe DIF.OskerInput)
sioCheckExternal' bounds =
  do { emptyq <- C.isEmptyChan (oskerIn bounds)
     ; if emptyq
       then return Nothing
       else do { pin <- C.readChan (oskerIn bounds)
               ; return (Just pin)
               }
     }

-- Check the external input, and read it if it is not empty.
sioCheckExternal :: 
    StandardIOBounds ->                  -- The bounds on the IO
    StandardIOBd (Maybe DIF.OskerInput) -- Action in the bounded IO monad
sioCheckExternal bounds = PT.liftIO ( sioCheckExternal' bounds )
--  return ( IOE.unsafePerformIO ( sioCheckExternal' bounds ) )

-- Response to system half "Pid" thread
sioRespond :: OM.OskerRspMsg -> OM.OskerRspPay -> StandardIOBd ()
sioRespond sm sp = BM.mTo ( OM.respondMessage sm sp )

-- Yield the processor for a while
sioYield :: StandardIOBd ()
sioYield = BM.mTo ( PT.yield )

-- Write a line on standard output (for debugging only)
sioPutStrLn :: String -> StandardIOBd ()
sioPutStrLn s = BM.mTo ( PT.putStrLn s )

-- Write a line on standard output (for debugging only)
sioPutStr :: String -> StandardIOBd ()
sioPutStr s = BM.mTo ( PT.putStr s )

-- Construct a bounded IO thread from the bounds given
mkStandardIOBdC ::
    StandardIOBounds -> -- Bounds on the thread IO
    StandardIOBdC       -- The bounded IO thread
mkStandardIOBdC bounds =
  StandardIOBdC
  { respond           = sioRespond
  , getFromChan       = sioGetFromChan    bounds
  , isChanEmpty       = sioIsChanEmpty $! bounds
  , toExternal        = sioToExternal     bounds
  , isExtEmpty        = sioIsExtEmpty     bounds
  , fromExternal      = sioFromExternal   bounds
  , checkExternal     = sioCheckExternal  bounds
  , yield             = sioYield
  , putStrLn          = sioPutStrLn
  , putStr            = sioPutStr
    -- Temporary only
  , getOskerIn        = oskerIn bounds
  , checkExternal'    = sioCheckExternal' bounds
  }
