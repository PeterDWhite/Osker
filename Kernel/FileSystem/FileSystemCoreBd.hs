-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileSystemCoreBd
    ( FileSystemCoreBd       -- Abstract data type
    , FileSystemCoreBdC       -- Structure of constructors for FileSystemBd
    , mkFileSystemCoreBdC     -- Constructor for FileSystemBdC
    , respond                 -- Respond to file system "Pid" thread
    , toKernelCore            -- Send to file system core
    , getFromChan             -- Get input from input channel
    , openDir                 -- Open a directory stream
    , readDir                 -- Read a directory stream
    , rewindDir               -- Rewind a directory stream
    , getFileStatus           -- Get the status of a file
    , yield                   -- Take a break
    , myThreadId              -- Return thread id.
    , putStrLn                -- For debugging
    , putStr                  -- For debugging
    , FileSystemCoreBounds    -- Kernel IO bounds
    , mkFileSystemCoreBounds  -- Constructor for FileSystemCoreBounds
    , B.toM                   -- Construct m from bounded m
    ) where

----------------------------------------------------------------------
--  The bounded IO monad for the file system core thread
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding ( putStr, putStrLn )
import qualified System as SYS
import qualified Parsec as PAR
-- Braid imports
import qualified LocalChan as C
import qualified ProcessThread as PT
-- Domain imports
import qualified ParseInt as PI
-- Posix imports
import qualified Posix as P
import qualified FileName as FN
import qualified DirectoryData as D
-- Osker imports
import qualified OskerMessage as OM
import qualified BoundedMonad as B
import qualified ThreadId as TID

-- Bounds on the file system core. The file system core
-- is permitted only to respond to incoming messages, which can only
-- come from file system "Pid" processes, or the kernel core. Thus
-- there are no bounds per se on the file system core, the property
-- that it can only respond to legitimately received messages is
-- enforced in the bounded IO monad for the file system core.
data FileSystemCoreBounds =
    FileSystemCoreBounds { kernelCoreChan :: OM.OskerChannel
                         , inputChan      :: OM.OskerChannel
                         }

-- For now, a renaming of FileSystemCoreBounds
mkFileSystemCoreBounds ::
    OM.OskerChannel ->      -- Channel to the kernel core
    OM.OskerChannel ->      -- Channel to the file system core
    FileSystemCoreBounds    -- The bounds on the IO
mkFileSystemCoreBounds = FileSystemCoreBounds

instance Show FileSystemCoreBounds where
    show (FileSystemCoreBounds _kc _inp) = "FileSystemCoreBounds*"

-- The bounded IO Monad for the file system core
type FileSystemCoreBd a = B.BdM PT.ProcessThread FileSystemCoreBounds a

----------------------------------------------------------------------
-- Package the constructors for the file system core kernel thread
----------------------------------------------------------------------
data FileSystemCoreBdC =
    FileSystemCoreBdC
    { -- A response to a file system "Pid" thread
      respond           :: OM.OskerRspMsg ->
                           OM.OskerRspPay ->
                           FileSystemCoreBd ()
      -- A write to the file system core
    , toKernelCore      :: OM.OskerRspMsg -> FileSystemCoreBd ()
      -- Read the input channel
    , getFromChan       :: FileSystemCoreBd OM.OskerRspMsg
    , openDir           :: FN.FileName -> FileSystemCoreBd D.DIR
    , readDir           :: D.DIR -> FileSystemCoreBd String
    , rewindDir         :: P.DirStream -> FileSystemCoreBd ()
    , getFileStatus     :: FN.FileName -> FileSystemCoreBd P.FileStatus
    , yield             :: FileSystemCoreBd ()
    , myThreadId        :: FileSystemCoreBd TID.ThreadId
      -- Debugging interfaces
    , putStrLn          :: String -> FileSystemCoreBd ()
    , putStr            :: String -> FileSystemCoreBd ()
    }

-- Get the contents of the system half input channel
fsCoreGetFromChan ::
    FileSystemCoreBounds ->           -- The bounds on the IO
    FileSystemCoreBd OM.OskerRspMsg -- Resulting message
fsCoreGetFromChan bounds = B.mTo ( PT.readChan (OM.projPC (inputChan bounds)) )

dirCount :: String -> IO Int
dirCount outfn =
  do { SYS.system ("ls " ++ outfn ++ "| wc -l >dc")
     ; en <- PAR.parseFromFile PI.parseInt "dc"
     ; case en of
         Right n -> return n
         Left e -> error ("Parsing # files: " ++ show e)
     }

-- Open a directory stream
fsCoreOpenDir ::
    FileSystemCoreBounds ->    -- The bounds on the IO
    FN.FileName ->             -- File to query
    FileSystemCoreBd D.DIR    -- Resulting directory stream
fsCoreOpenDir _bounds fn =
  let outfn  = FN.outFileName fn
  in do { dir    <- PT.liftIO (P.openDirStream outfn)
        ; fstat  <- PT.liftIO (P.getFileStatus outfn)
        ; dirCnt <- PT.liftIO (dirCount outfn)
        ; return ( D.DIR { D.stream    = dir
                         , D.dPath     = fn
                         , D.isDir     = P.isDirectory fstat
                         , D.dirCount  = dirCnt
                         , D.readCount = 0
                         }
                 )
        }

-- Read a directory stream
fsCoreReadDir ::
    FileSystemCoreBounds ->    -- The bounds on the IO
    D.DIR ->                   -- Directory stream to read
    FileSystemCoreBd String   -- Resulting file name
fsCoreReadDir _bounds dir =
  let stream = D.stream dir
  in do { fname <- PT.liftIO (P.readDirStream stream)
        ; return ( fname )
        }

fsCoreRewindDir ::
    FileSystemCoreBounds ->    -- The bounds on the IO
    P.DirStream ->             -- Directory stream to rewind
    FileSystemCoreBd ()       -- Bounded IO action
fsCoreRewindDir _bounds dir = PT.liftIO (P.rewindDirStream dir)

-- Get the status of a file
fsCoreGetFileStatus ::
    FileSystemCoreBounds ->        -- The bounds on the IO
    FN.FileName ->                 -- File to state
    FileSystemCoreBd P.FileStatus -- Resulting file status
fsCoreGetFileStatus _bounds fn =
  let outfn = FN.outFileName fn
  in do { fstat <- PT.liftIO (P.getFileStatus outfn)
        ; return ( fstat )
        }

-- Write to the kernel dore
fsCoreToKernelCore ::
    FileSystemCoreBounds -> -- The bounds on the IO
    OM.OskerRspMsg ->      -- Message to write
    FileSystemCoreBd ()
fsCoreToKernelCore bounds sm =
  B.mTo ( PT.writeChan (OM.projPC (kernelCoreChan bounds)) sm)

-- Response to system half "Pid" thread
fsCoreRespond ::
    OM.OskerRspMsg ->   -- Incoming message
    OM.OskerRspPay ->   -- Payload to return
    FileSystemCoreBd ()
fsCoreRespond sm sp = B.mTo (OM.respondMessage sm sp)

-- Yield the processor for a while
fsCoreYield :: FileSystemCoreBd ()
fsCoreYield = B.mTo ( PT.yield )

-- Return current thread Id
fsCoreMyThreadId :: FileSystemCoreBd TID.ThreadId
fsCoreMyThreadId = B.mTo ( PT.myThreadId )

-- Write a line on standard output (for debugging only)
fsCorePutStrLn :: String -> FileSystemCoreBd ()
fsCorePutStrLn s = B.mTo ( PT.putStrLn s )

-- Write a line on standard output (for debugging only)
fsCorePutStr :: String -> FileSystemCoreBd ()
fsCorePutStr s = B.mTo ( PT.putStr s )

-- Construct a bounded IO thread from the bounds given
mkFileSystemCoreBdC ::
    FileSystemCoreBounds -> -- Bounds on the thread IO
    FileSystemCoreBdC       -- The bounded IO thread
mkFileSystemCoreBdC bounds =
  FileSystemCoreBdC
  { respond           = fsCoreRespond
  , toKernelCore      = fsCoreToKernelCore  bounds
  , getFromChan       = fsCoreGetFromChan   bounds
  , openDir           = fsCoreOpenDir       bounds
  , readDir           = fsCoreReadDir       bounds
  , rewindDir         = fsCoreRewindDir     bounds
  , getFileStatus     = fsCoreGetFileStatus bounds
  , yield             = fsCoreYield
  , myThreadId        = fsCoreMyThreadId
  , putStrLn          = fsCorePutStrLn
  , putStr            = fsCorePutStr
  }
