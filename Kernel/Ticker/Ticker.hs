-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Ticker (  ticker ) where

----------------------------------------------------------------------
-- The ticker, generating timing signals
----------------------------------------------------------------------

-- Haskell imports
import qualified PosixProcEnv as P
-- Braid imports
import qualified ProcessThread as PT
import qualified LocalChan as C
-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified OskerMessage as OM

----------------------------------------------------------------------
-- The ticker:
-- The ticker generates an interrupt to the timer device driver
-- once every second
----------------------------------------------------------------------
ticker :: P.ProcessTimes -> OM.OskerChannel -> PT.ProcessThread ()
ticker processTimes chan =
  do { tid           <- PT.myThreadId
     ; processTimes' <- PT.liftPT (P.getProcessTimes)
     ; let tickMsg = OM.mkOskerMsg
                       (OM.SystemHalf PN.emptyPN)
                       tid
                       OM.External
                       False
                       (Just chan)
                       OM.TimerTick
           elapsed       = P.elapsedTime processTimes'
           -- Apparently there is a tick 100 times per second
     ; if elapsed - P.elapsedTime processTimes >= 100
       then do { tickerOut "Ticker" ("Tick: " ++ show (elapsed `div` 100))
               ; PT.writeChan (OM.projPC chan) tickMsg
               ; ticker processTimes' chan
               }
       else do { ticker processTimes chan }
     }

-- Print outs from timer device drivers
tickerOut ::
    String         ->   -- Name of device driver wanting print out
    String         ->   -- String to print
    PT.ProcessThread () -- This is a threader action
tickerOut ddname s = PT.putStrLn ("***[DD." ++ ddname ++ "]...\t\t" ++ s)
