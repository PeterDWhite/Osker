-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module SystemHalfBounds ( SystemHalfBounds (..) ) where

----------------------------------------------------------------------
--  The bounds for a kernel thread that happens to be a system half.
----------------------------------------------------------------------

-- Braid imports
import qualified LocalChan as LC
-- Osker imports
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The bounds on the system half is an explicit list of things that
-- the system half can manipulate, that may have effects outside the
-- scope of the system half kernel thread.
----------------------------------------------------------------------
data SystemHalfBounds =
    SystemHalfBounds
    { -- Input to the system half
      inputChan      :: LC.Chan OM.OskerMsg
      -- Messages to the kernel core
    , kernelCoreChan :: LC.Chan OM.OskerMsg
      -- Messages to the timer device driver
    , timerChan      :: LC.Chan OM.OskerMsg
      -- Messages to the standard device driver
    , standardChan   :: LC.Chan OM.OskerMsg
      -- Messages to the file system "Pid" thread
    , fsChan         :: LC.Chan OM.OskerMsg
    }

instance Show SystemHalfBounds where
    show _ = "SystemHalfBounds *"
