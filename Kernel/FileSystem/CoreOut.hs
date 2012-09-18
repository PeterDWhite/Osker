-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module CoreOut ( fsOut ) where

----------------------------------------------------------------------
-- The file system core device driver print out function
----------------------------------------------------------------------

-- Local imports
import qualified FileSystemCoreBd as B

-- Print outs from file system core
fsOut ::
    B.FileSystemCoreBdC -> -- Bounds on the io
    String ->              -- String to print
    B.FileSystemCoreBd ()  -- This is an IO action
fsOut b s = (B.putStrLn b) ("***[FS.Core]...\t\t\t" ++ s)
