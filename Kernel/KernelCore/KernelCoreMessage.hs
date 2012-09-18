-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreMessage
    ( KernelCoreMessageType(..) -- A type for Osker messages in kernel core
    , getCallTableIndex         -- Get index to call table from Osker message
    ) where

----------------------------------------------------------------------
-- The definitions that localize the Osker message to the system
-- half instantiation of the executive.
----------------------------------------------------------------------

-- Osker imports
import qualified CallTableIndex as CTI
import qualified OskerMessage as OM

data KernelCoreMessageType
    -- From the user half, via the trap handler.
    = ToKernelCoreType
    -- From self
    | FromProcessControlType
    -- Commands from the system half
    | FromSystemHalfType
    deriving (Eq, Ord, Enum, Show)

-- Convert the incoming payload to the system half to a message type.
kernelCorePayloadType :: OM.OskerPay -> KernelCoreMessageType
kernelCorePayloadType sp =
  case sp of
    OM.ToKernelCore _kcr       -> ToKernelCoreType
    OM.FromProcessControl _pcr -> FromProcessControlType
    OM.FromSystemHalf _kcc     -> FromSystemHalfType
    _otherwise                 ->
      error ("kernelCorePayloadType: " ++ show sp)

-- Get the message type from the incoming message to the system half
kernelCoreMessageType :: OM.OskerPay -> Int
kernelCoreMessageType = fromEnum . kernelCorePayloadType

-- Get the activity index from the incoming message to the system half.
kernelCoreMessageIndex :: OM.OskerPay -> Int
kernelCoreMessageIndex sp =
  case sp of
    OM.ToKernelCore kcr       -> OM.kernelCoreRequest2Int kcr
    OM.FromProcessControl pcr -> OM.processControlRequest2Int pcr
    OM.FromSystemHalf kcc     -> OM.kernelCoreComply2Int kcc
    _otherwise                ->
      error ("kernelCorePayloadIndex: " ++ show sp)

-- Generate the call table index from the incoming Osker message
-- to the system half.
getCallTableIndex :: CTI.ToCallTableIndex OM.OskerPay
getCallTableIndex sp =
  CTI.CallTableIndex
   (kernelCoreMessageType sp)  -- Message type
   (kernelCoreMessageIndex sp) -- Message index
