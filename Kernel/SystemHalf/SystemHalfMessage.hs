-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SystemHalfMessage
    ( SystemHalfMessageType(..) -- Types of Osker messages in system half
    ) where

----------------------------------------------------------------------
-- The definitions that localize the Osker message to the system
-- half instantiation of the executive.
----------------------------------------------------------------------

-- Posix imports
import qualified SystemCall as SC
-- Osker imports
import qualified OskerMessage as OM

data SystemHalfMessageType
    -- From the user half, via the trap handler.
    = FromTrapHandlerType
    -- Commands from the system half
    | ToSystemHalfType
    deriving (Eq, Ord, Enum, Show)

-- Convert the incoming payload to the system half to a message type.
systemHalfPayloadType :: OM.OskerPay -> SystemHalfMessageType
systemHalfPayloadType sp =
  case sp of
    OM.FromTrapHandler _sysreq -> FromTrapHandlerType
    OM.ToSystemHalf _kcc       -> ToSystemHalfType
    _otherwise                 -> error ("systemHalfPayloadType: " ++ show sp)

-- Get the message type from the incoming message to the system half
systemHalfMessageType :: OM.OskerPay -> Int
systemHalfMessageType = fromEnum . systemHalfPayloadType

-- Get the activity index from the incoming message to the system half.
systemHalfMessageIndex :: OM.OskerPay -> Int
systemHalfMessageIndex sp =
  case sp of
    OM.FromTrapHandler sysreq -> SC.systemRequest2Int sysreq
    OM.ToSystemHalf kcc       -> OM.kernelCoreCommand2Int kcc
    _otherwise                ->
      error ("systemHalfPayloadIndex: " ++ show sp)

-- Generate the call table index from the incoming Osker message
-- to the system half.
getCallTableIndex :: CTI.ToCallTableIndex OM.OskerPay
getCallTableIndex sp =
  CTI.CallTableIndex
   (systemHalfMessageType sp)  -- Message type
   (systemHalfMessageIndex sp) -- Message index
