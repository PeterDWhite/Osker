-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module KernelCoreActor
    ( KernelCoreActor                -- Kernel core program
    , KernelCoreActorState           -- State local to kernel core pgoram
    , KernelCoreSegment              -- Segment of an actor tree
    , KernelCoreActorBreak           -- Actor returning a break value
    , EA.Destination (..)            -- Destination in the kernel
    , OM.OskerAction                 -- Deffered IO action
    , KernelCoreParameters           -- Configuration of the kernel core
    , initKernelCoreActorState       -- Init program state
    , KernelCorePartitionedState     -- Partitioned state of the kernel core
    , KernelCorePartitionedStateKey  -- Key to partititioned state
    , KernelCorePartitionedStateElt  -- Value from partitioned state
    -- Some utilities for the kernel core programs
    , EA.getHardElt                  -- Specialize exec actor stuff
    , EA.getVanillaElt               -- Specialize exec actor stuff
    , EA.getHardVanillaElt           -- Specialize exec actor stuff
    , EA.updateVanillaElt            -- Specialize exec actor stuff
    -- Message actions
    , EA.endSegment                  -- End a segment of an actor
    , EA.endActor                    -- End an actor completely
    , EA.waitSegment                 -- Send out a msg and wait for response
    , EA.branch                      -- Create a branch in the tree
    , EA.respondSegment              -- Respond to segment start message
    , EA.respondActor                -- Respond to actor start message
    -- Message actions for the kernel core
    , shRsp                          -- Respond to system half
    , defer                          -- Defer some IO work
    ) where

----------------------------------------------------------------------
-- Specialize the actor concept to the kernel core
----------------------------------------------------------------------

-- Posix imports
import qualified ProcessName as PN
-- Osker imports
import qualified GlobalPartitionedStateKey as GPSK
import qualified GlobalPartitionedState as GPS
import qualified ExecutiveActor as EA
import qualified LocalPartitionedState as LPS
import qualified ExecutiveParameters as EP
import qualified OskerMessage as OM
import qualified OskerDelta as PD

type KernelCorePartitionedState    = GPS.GlobalPartitionedState
type KernelCorePartitionedStateKey = GPSK.GlobalPartitionedStateKey
type KernelCorePartitionedStateElt = GPS.GlobalPartitionedStateElt

type KernelCoreActorState =
    EA.ExecActorState
       KernelCorePartitionedStateKey -- Partitioned state key
       KernelCorePartitionedStateElt -- Partitioned state conponent

initKernelCoreActorState ::
    KernelCorePartitionedState         -> -- Current fractured state
    PD.LocalDelta GPSK.GlobalPartitionedStateKey ->
    PN.ProcessName   ->                   -- Process name for debugging
    KernelCoreActorState
initKernelCoreActorState = EA.initExecActorState 

type KernelCoreActor r a =
    EA.ExecActor
       KernelCorePartitionedStateKey  -- Partitioned state key
       KernelCorePartitionedStateElt  -- Partitioned state conponent
       a

type KernelCoreSegment r =
    EA.ExecSegment
       KernelCorePartitionedStateKey  -- Partitioned state key
       KernelCorePartitionedStateElt  -- Partitioned state conponent
       OM.OskerPay
       r

type KernelCoreActorBreak r =
    EA.ExecActorBreak
       KernelCorePartitionedStateKey  -- Partitioned state key
       KernelCorePartitionedStateElt  -- Partitioned state conponent
       OM.OskerPay
       r

type KernelCoreParameters r =
    EP.ExecutiveParameters
       KernelCorePartitionedStateKey  -- Partitioned state key
       KernelCorePartitionedStateElt  -- Partitioned state conponent
       ()                             -- No global state for kernel core
       OM.OskerPay                    -- Payload of messages
       r                              -- Response channel

-- Response to a system half
shRsp :: OM.KernelCoreResponse -> PN.ProcessName -> KernelCoreActorBreak r
shRsp kcr pn = EA.respondActor (OM.shRsp kcr pn) []

-- Defer some IO work
defer :: OM.ProcessControlRequest -> OM.OskerAction -> KernelCoreActorBreak r
defer pcr act = EA.endSegment [OM.kcDefer pcr act]
