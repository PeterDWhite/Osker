-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module DDOut ( ddOut ) where

-- Braid imports
import qualified ProcessThread as PT

-- Print outs from device drivers
ddOut :: String         ->   -- Name of device driver wanting print out
         String         ->   -- String to print
         PT.ProcessThread () -- This is an Threader action
ddOut ddname s = PT.putStrLn ( "***[DD." ++ ddname ++ "]...\t\t" ++ s )
