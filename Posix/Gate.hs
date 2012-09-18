-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Gate
    ( Gate (..)  -- Data type for gate between threads
    , mkGate     -- Constructor for Gate
    , enterGate  -- Put an item through the gate
    , waitGate   -- Wait for an item to come through the gate
    ) where

----------------------------------------------------------------------
-- The gate is a structure used to support synchronous and
-- asynchronous calls to the system half.
----------------------------------------------------------------------

-- Haskell imports
import Dynamic ( Typeable )
-- Utility imports
import qualified Null as N 
-- Braid imports
import qualified BraidExternal as B

-- The gate represents a synchronous communications path between two threads.
-- The gate is directional, the sending end places a request of type a
-- in the gate, and then blocks, waiting for a response of type b from
-- the gate.
data Gate a b =
    Gate { gateTo      :: B.MVar a
         , gateFrom    :: B.MVar b
         }

-- Make a new call gate
mkGate :: (N.Null gs, Show gs, Show ls) =>
    String -> B.Braid gs ls (Gate a b)
mkGate name =
  do { togate   <- B.newEmptyMVar (name ++ ".to")
     ; fromgate <- B.newEmptyMVar (name ++ ".from")
     ; return (Gate togate fromgate)
     }

-- Put an item through the gate to another thread
enterGate :: (N.Null gs, Show gs, Show ls, Typeable a) =>
    Gate a b ->       -- The gate to enter
    a        ->       -- The item to put through the gate
    B.Braid gs ls ()  -- This is a Braid action
enterGate gate item =
  do { B.putMVar (gateTo gate) item -- This is non blocking
     ; return ()
     }

-- Wait for an item to come through the gate
waitGate :: ( Show b, Typeable b, N.Null gs, Show gs, Show ls ) =>
    Gate a b        -> -- The gate to enter
    B.Braid gs ls b    -- Braid action returning the waited item
waitGate gate =
  do { rsp <- B.takeMVar (gateFrom gate) -- This is blocking
     ; return rsp
     }
