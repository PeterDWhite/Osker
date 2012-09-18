-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Devices ( standardDevices ) where

----------------------------------------------------------------------
-- Build the device map, a table of information of device drivers.
-- Also build the device braid state.
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Utility imports
import qualified Null as N
-- Braid imports
import qualified PlatformBraid as PlB
import qualified DomainBraid as DB
import qualified BraidExternal as B
-- Osker imports
import qualified Ticker as TCK
import qualified BraidUtilities as BU
import qualified DeviceMap as DM
import qualified DeviceId as DID
import qualified PosixProcEnv as P
import qualified OskerMessage as OM

-- A function to initialize the device map with the standard devices
-- This function operates in the platform braid, but it builds a domain
-- state in the domain braid. The inputs include the domain state and
-- the initial device map, and the outputs are the extended device map
standardDevices ::
    DM.DeviceMap      ->  -- Initial value of device map
    DB.DomainSt       ->  -- Initial value of domain braid state
    [DM.DeviceConfig] ->  -- Device config information
    PlB.PlatformBraid
    ( DB.DomainSt         -- Output value of domain braid state
    , DM.DeviceMap        -- Output value of device map
    )
standardDevices dmap dmst [] = return ( dmst, dmap )
standardDevices dmap dmst0 ( dc:dcs ) =
  -- Bust apart the first device config datum
  let DM.DeviceConfig { DM.dcDeviceId     = did
                      , DM.dcChannel      = chan
                      , DM.dcDeviceDriver = dd
                      , DM.dcDeviceName   = name
                      } = dc
  in do { ( dmst1, threadInfo ) <-
              ( PlB.lift
                 ( BU.myForkLower
                     name
                     (OM.projPC chan)
                     (DB.proj dd)
                     dmst0
                 ) )
                     --name (OM.projPC chan) (DB.proj dd) dmst0 ) )
        ; processTimes <- PlB.lift ( B.liftIO (P.getProcessTimes) )
        ; let threadInfo' = threadInfo { BU.tiChan = OM.projPC chan }
              dmap'       = FM.addToFM dmap (fromEnum did) threadInfo'
        -- Special case for the timer device, need to also start
        -- the ticker, using the thread Id from the timer device driver
        ; if DM.dcDeviceId dc == DID.TimerDeviceId
          then do { ( dmst2, tickerThreadInfo ) <-
                      PlB.lift
                        ( BU.myForkLower
                            "Ticker"
                            ( OM.projPC chan )
                            ( DB.proj ( TCK.ticker processTimes chan ) )
                            dmst1
                        )
                  ; standardDevices dmap' dmst2 dcs
                  }
          else standardDevices dmap' dmst1 dcs
        }
