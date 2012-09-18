-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreOut ( kernelCoreOut ) where

-- Braid imports
import qualified BraidExternal as B
-- Actor imports
import qualified DomainBraid as DB

-- Print outs from the kernel core
kernelCoreOut :: String -> DB.DomainBraid ()
kernelCoreOut s = DB.lift ( B.bputStrLn ("***[Kernel Core]...\t\t" ++ s) )
