-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module DeviceMap
    ( DeviceMap         -- Mapping device Id to thread information
    , DeviceConfig (..) -- Configuration of a device driver
    ) where

----------------------------------------------------------------------
-- A table of thread Ids for the Osker devices
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Utility imports
import qualified Null as N
-- Braid imports
import qualified BraidUtilities as BU
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified DeviceId as DID
import qualified OskerMessage as OM

-- A device Id must be converted to an int to access this map
-- tid is specialized to process name
data DeviceMap =
    DeviceMap ( FM.FiniteMap Int (BU.ForkInfo OM.OskerMsg PN.ProcessName) )

instance N.Null DeviceMap where
    N.mkNull = DeviceMap FM.emptyFM

-- Device drivers are processes, thus they live at the domain braid level
-- t is the type of the thread to encapsulate the device
data DeviceConfig t =
    DeviceConfig { dcDeviceId     :: DID.DeviceId
                 , dcChannel      :: OM.OskerChannel
                 , dcDeviceDriver :: t
                 , dcDeviceName   :: String
                 }
