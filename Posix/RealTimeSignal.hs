-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module RealTimeSignal
    ( -- Real time signal data structures
    , SigEvent (..)
    , SignalMechanism (..)
    , SigevValue (..)
    , sigRtMin
    , sigRtMax
    ) where

----------------------------------------------------------------------
-- The real time signals are POSIX.4, they extend the POSIX.1
-- signal functionality.
----------------------------------------------------------------------

data SignalMechanism
    = SigevSignals -- Use signals for asynchronous notification
    | SigevNone    -- No asynchronous notification
    deriving (Eq, Ord, Enum, Show)

-- Some Posix.4 real time signal configuration values
sigRtMin :: Int
sigRtMin = error "Need to set sigRtMin at config time"
sigRtMax :: Int
sigRtMax = error "Need to set sigRtMax at config time"

-- The freight of the signal is implementation defined. For now
-- just let it be an Int.

newtype SigevValue = SigevValue Int
    deriving (Show)

data SigEvent =
    SigEvent
    { sigevNotify :: SignalMechanism -- Asynchronous mechanism being used
    , sigevSigno  :: Int             -- Number of signal to be delivered
    , sigevValue  :: SigevValue      -- The freight of the message
    } deriving (Show)
