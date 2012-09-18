-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module DeviceId ( DeviceId (..) ) where

----------------------------------------------------------------------
-- Define the global device identifier
----------------------------------------------------------------------
data DeviceId
    = TimerDeviceId
    | StandardInOut
    | FileSystemId
    deriving (Eq, Ord, Enum, Show)
