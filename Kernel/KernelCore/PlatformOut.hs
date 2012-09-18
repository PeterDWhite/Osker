-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module PlatformOut ( platOut ) where

-- Braid imports
import qualified BraidExternal as B
import qualified PlatformBraid as PB

-- Print out from a platform braid
platOut :: String -> PB.PlatformBraid ()
platOut s = PB.lift ( B.bputStrLn ("***[Kernel Core]...\t\t" ++ s) )
