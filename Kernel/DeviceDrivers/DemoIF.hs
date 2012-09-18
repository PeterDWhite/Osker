-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module DemoIF
    ( OskerInput (..)      -- External inputs to the Osker kernel
    , OskerOutput (..)     -- External outputs to the Osker kernel
    , OM.OskerCommand (..) -- External commands to the Osker kernel
    ) where

----------------------------------------------------------------------
-- Interface between the demo program and the Osker kernel
----------------------------------------------------------------------

-- Posix imports
import qualified ProcessName as PN
import qualified SystemCall as SC
-- Osker imports
import qualified OskerMessage as OM

data OskerInput = OskerInput { target :: PN.ProcessName
                             , input  :: OM.OskerCommand
                             }

instance Show OskerInput where
    show pin = "OskerInput: " ++ PN.outProcessName (target pin) ++
               ", " ++ show (input pin)

data OskerOutput = OskerOutput { source :: PN.ProcessName
                               , output :: SC.SystemResponse
                               }

instance Show OskerOutput where
    show pout = "OskerOutput: " ++ PN.outProcessName (source pout) ++
                ", " ++ show (output pout)
