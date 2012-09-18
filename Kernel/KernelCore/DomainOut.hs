-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module DomainOut ( domOut, niceOut ) where

-- Braid imports
import qualified BraidExternal as B
import qualified DomainBraid as DB
-- Posix imports
import qualified ProcessName as PN

-- Print outs from a domain braid
domOut :: String -> DB.DomainBraid ()
domOut s = DB.lift ( B.bputStrLn ("***[Kernel Core]...\t\t" ++ s) )

-- Make a printout for the IOShell
niceOut :: PN.ProcessName -> String -> DB.DomainBraid ()
niceOut name s = DB.lift (B.bputStrLn (domFormat name s))

domFormat :: PN.ProcessName -> String -> String
domFormat name s = "...[IOShell:" ++ PN.outProcName name ++ "]...\t" ++ s
