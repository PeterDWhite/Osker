-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module TCBPool ( TCBPool ) where

----------------------------------------------------------------------
-- The partitioned state for the kernel core
----------------------------------------------------------------------

-- Osker imports
import qualified OskerPool as OP
import qualified TimerControlBlock as TCB

type TCBPool = OP.OskerPool TCB.TimerControlBlock
