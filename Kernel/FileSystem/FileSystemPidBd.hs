-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileSystemPidBd
    ( FileSystemPidBd          -- Abstract data type
    , FileSystemPidBdC         -- Structure of constructors for FileSystemBd
    , mkFileSystemPidBdC       -- Constructor for FileSystemBdC
    , toSystemHalf             -- Send to the corresponding system half
    , toCore                   -- Send to file system core
    , forwardToCore            -- Forward a message to file system core
    , getFromChan              -- Get input from input channel
    , respond                  -- Respond to system half
    , yield                    -- Take a break
    , putStrLn                 -- For debugging
    , putStr                   -- For debugging
    , FileSystemPidBounds (..) -- Kernel IO bounds
    , B.toM                    -- Convert bounded m to m
    ) where

----------------------------------------------------------------------
--  The standard bounds for a kernel thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding ( putStr, putStrLn )
-- Braid imports
import qualified ProcessThread as PT
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified OskerMessage as OM
import qualified BoundedMonad as B

-- The bounds on the file system Pid is an explicit list of things
-- that the system half can manipulate, that may have effects
-- outside the scope of the file system Pid kernel thread.

data FileSystemPidBounds =
    FileSystemPidBounds
    { -- Input to the system half
      inputChan      :: OM.OskerChannel
      -- Messages to the file system core
    , fsCoreChan     :: OM.OskerChannel
      -- For messages to the corresponding system half
    , fsSys2Chan     :: OM.OskerChannel
      -- The process name on behalf of which this thread operates
      -- This is useful for debugging purposes
    , localName      :: PN.ProcessName
    }

instance Show FileSystemPidBounds where
    show fspb = "FileSystemBounds: " ++ (PN.outProcessName (localName fspb))

-- The bounded IO Monad for the file system "Pid" threads
type FileSystemPidBd a = B.BdM PT.ProcessThread FileSystemPidBounds a

----------------------------------------------------------------------
-- Package the constructors for the kernel thread bounded IO for
-- the file system "Pid" kernel thread
----------------------------------------------------------------------
data FileSystemPidBdC =
    FileSystemPidBdC
    { -- A write to the corresponding system half
      toSystemHalf      :: OM.OskerRspMsg -> FileSystemPidBd ()
      -- A write to the file system core
    , toCore            :: OM.OskerRspMsg -> FileSystemPidBd ()
      -- Forward a message to the file system core
    , forwardToCore     :: OM.OskerRspMsg ->
                           OM.OskerRspPay ->
                           FileSystemPidBd ()
      -- Read the input channel
    , getFromChan       :: FileSystemPidBd OM.OskerRspMsg
      -- Respond to the system half
    , respond           :: OM.OskerRspMsg ->
                           OM.OskerRspPay ->
                           FileSystemPidBd ()
    , yield             :: FileSystemPidBd ()
      -- Debugging interfaces
    , putStrLn          :: String -> FileSystemPidBd ()
    , putStr            :: String -> FileSystemPidBd ()
    }

-- Get the contents of the system half input channel
fsPidGetFromChan ::
    FileSystemPidBounds ->           -- The bounds on the IO
    FileSystemPidBd OM.OskerRspMsg -- Resulting stream of messages
fsPidGetFromChan bounds = B.mTo ( PT.readChan (OM.projPC (inputChan bounds)) )

-- Write to the kernel dore
fsPidToCore ::
    FileSystemPidBounds -> -- The bounds on the IO
    OM.OskerRspMsg ->     -- Message to write
    FileSystemPidBd ()    -- A bounded IO action
fsPidToCore bounds sm =
  B.mTo ( PT.writeChan (OM.projPC (fsCoreChan bounds)) sm )

-- Make a new message to forward to the kernel core
-- The old message is left unscathed
fsPidForwardToCore ::
    FileSystemPidBounds -> -- The bounds on the IO
    OM.OskerRspMsg ->     -- Message to write
    OM.OskerRspPay ->     -- New payload to use
    FileSystemPidBd ()    -- A bounded IO action
fsPidForwardToCore bounds sm sp =
  let sm' = OM.mkForwardMsg sm sp (Just (inputChan bounds))
  in fsPidToCore bounds sm'

-- Write to the corresponding system hafl
fsPidToSystemHalf ::
    FileSystemPidBounds -> -- The bounds on the IO
    OM.OskerRspMsg ->     -- Message to write
    FileSystemPidBd ()
fsPidToSystemHalf bounds sm =
  B.mTo ( PT.writeChan (OM.projPC (fsSys2Chan bounds)) sm )

-- Respond to system half
fsPidRespond ::
    FileSystemPidBounds -> -- The bounds on the IO
    OM.OskerRspMsg ->      -- Message to respond to
    OM.OskerRspPay ->      -- New payload to send
    FileSystemPidBd ()
fsPidRespond _bounds sm sp = B.mTo (OM.respondMessage sm sp)

-- Yield the processor for a while
fsPidYield :: FileSystemPidBd ()
fsPidYield = B.mTo ( PT.yield )

-- Write a line on standard output (for debugging only)
fsPidPutStrLn :: String -> FileSystemPidBd ()
fsPidPutStrLn s = B.mTo ( PT.putStrLn s )

-- Write a line on standard output (for debugging only)
fsPidPutStr :: String -> FileSystemPidBd ()
fsPidPutStr s = B.mTo ( PT.putStr s )

-- Construct a bounded IO thread from the bounds given
mkFileSystemPidBdC ::
    FileSystemPidBounds -> -- Bounds on the thread IO
    FileSystemPidBdC       -- The bounded IO thread
mkFileSystemPidBdC bounds =
  FileSystemPidBdC
  { toSystemHalf      = fsPidToSystemHalf  bounds
  , toCore            = fsPidToCore        bounds
  , forwardToCore     = fsPidForwardToCore bounds
  , getFromChan       = fsPidGetFromChan   bounds
  , respond           = fsPidRespond       bounds
  , yield             = fsPidYield
  , putStrLn          = fsPidPutStrLn
  , putStr            = fsPidPutStr
  }
