-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module TimerControlBlock
    ( TimerControlBlock  -- Timer control block data type
    , emptyTCB           -- Create an empty TCB
    ) where

-- Haskell imports
import qualified Dynamic as DYN
-- Posix imports
import qualified ProcessName as PN
import qualified Resource as R

----------------------------------------------------------------------
-- The timer system resource blocks
----------------------------------------------------------------------

data TimerControlBlock = -- Abbreviation (tcb)
    TickTimerControlBlock
    { tcbProcessName :: PN.ProcessName -- Who scheduled
    , tcbTickCount   :: Int            -- Number of ticks to wait
    }

instance Show TimerControlBlock where
    show tcb = "{TCB: " ++ PN.outProcName (tcbProcessName tcb) ++ ", " ++
               show (tcbTickCount tcb) ++ "}"

instance R.Resource TimerControlBlock where
    zeroize tcb = tcb { tcbProcessName = PN.emptyPN
                      , tcbTickCount   = 0
                      }

-- An initialized TCB
emptyTCB :: TimerControlBlock
emptyTCB = TickTimerControlBlock
           { tcbProcessName = PN.emptyPN
           , tcbTickCount   = 0
           }

-- Make TCB an instance of dynamic
kTCBCon :: DYN.TyCon
kTCBCon = DYN.mkTyCon "Timer Control Block"
instance DYN.Typeable TimerControlBlock where
    DYN.typeOf _ = DYN.mkAppTy kTCBCon []
