-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module TrapMessage ( TrapReqMessage (..) ) where

-- Haskell imports
import qualified Dynamic as DYN
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The trap message defines the requests and response of the trap
-- handler thread
----------------------------------------------------------------------

-- For inputs from the kernel
data TrapReqMessage
    -- New thread Id map entry
    = TrapNewProc
      { trmUserHalf   :: PN.ProcessName -- tid specialized to process name
      , trmChan       :: OM.OskerChannel
      , trmName       :: PN.ProcessName
      }
    -- Retire a map entry
    | TrapDelProc PN.ProcessName PN.ProcessName

instance Show TrapReqMessage where
    show (TrapNewProc _ _ name) = "TrapNewProc: " ++ PN.outProcessName name
    show (TrapDelProc _ name)   = "TrapDelProc: " ++ PN.outProcessName name

trapMsgCon :: DYN.TyCon
trapMsgCon = DYN.mkTyCon "Trap Request Message"
instance DYN.Typeable TrapReqMessage where
    DYN.typeOf _ = DYN.mkAppTy trapMsgCon []
