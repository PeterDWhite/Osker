-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Coordinate where

-- Haskell imports
import qualified Concurrent as C
import qualified OskerConcurrent as SC
import qualified IOExts as IOE
import qualified IOUtilities as IOU

type Coordinate = IOE.IORef (C.MVar Int)
type OskerCoordinate = IOE.IORef (SC.MVar Int)

-- Read a coordinate
readCoordinate :: Coordinate -> IO Int
readCoordinate coordl =
  do { sem <- IOE.readIORef coordl
     ; n <- C.readMVar sem
     ; return n
     }

writeCoordinate :: Int -> Coordinate -> IO ()
writeCoordinate n coordl =
  do { sem <- IOE.readIORef coordl
     ; C.swapMVar sem n
     ; return ()
     }

-- Send an arbitrary signal
signalOskerInt :: Int -> Coordinate -> IO ()
signalOskerInt n coordl =
  do { let x = IOE.unsafePerformIO
               ( do { sem <- IOE.readIORef coordl
                    ; C.putMVar sem n
                    }
               )
     ; return x
     }

-- Send an arbitrary signal
signalInt :: Int -> Coordinate -> IO ()
signalInt n coordl =
  do { sem <- IOE.readIORef coordl
     ; C.putMVar sem n
     }

-- Send the done signal
signalDone :: Coordinate -> IO ()
signalDone coordl = return (IOE.unsafePerformIO (signalInt 0 coordl))

-- Wait for any signal
waitForSignal :: Coordinate -> IO Int
waitForSignal coordl =
  do { let n = IOE.unsafePerformIO
               ( do { sem <- IOE.readIORef coordl
                    ; C.takeMVar sem
                    ; return n
                    }
               )
     ; return n
     }

-- Wait for all the signals to come in
waitForDone :: Int -> Coordinate -> IO Int
waitForDone n coordl =
  if n == 0
  then return 0
  else do { sem <- IOE.readIORef coordl
          ; wait <- IOU.repeatIO n (C.takeMVar sem)
          ; return wait
          }

-- Make a new counter
type Counter = C.MVar Int
newCounter :: Counter
newCounter = IOE.unsafePerformIO (C.newMVar 0)

newCoord :: Coordinate
newCoord = IOE.unsafePerformIO
              (IOE.newIORef
                 (IOE.unsafePerformIO
                    (C.newEmptyMVar)))

initCounter :: Counter -> IO ()
initCounter counter = C.putMVar counter 0

incCounter :: Counter -> IO ()
incCounter counter =
  do { n <- C.readMVar counter
     ; _m <- C.swapMVar counter (n+1)
     ; return ()
     }

incCounter' :: Counter -> IO ()
incCounter' counter = return (IOE.unsafePerformIO (incCounter counter))

getCounter :: Counter -> IO Int
getCounter counter =
  do { n <- C.readMVar counter
     ; return n
     }

-- Declared here, used by each test case for communication with
-- the main program
coord :: Coordinate
coord = newCoord
