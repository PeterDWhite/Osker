-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ActorState
    ( ActorState     -- State internal to a actor braid
    , actorId        -- Get the local actor Id
    , ActorRet (..)  -- Return value from a actor braid
    ) where

-- Utility imports
import qualified Null as N
-- Braid imports
import qualified BraidLocal as B
-- Posix imports
import qualified ProcessName as PN
import qualified SystemCall as SC
-- Kernel globals
import qualified TimerControlBlock as TCB
import qualified ExceptionData as ED
-- Actor imports
import qualified MQData as MQD
import qualified SignalData as SIGD

----------------------------------------------------------------------
-- Internal state to a actor braid
----------------------------------------------------------------------

-- Structure with local data for a actor thread
-- Different actors can use different states (s)
data ActState s = ActState
    { actorid :: PN.ProcessName
    , special :: s
    }

instance (N.Null s) => N.Null (ActState s) where
    N.mkNull = ActState { actorid = N.mkNull
                        , special = N.mkNull
                        }

instance (Show s) => Show (ActState s) where
    show ast = "ActState " ++ show (actorid ast) ++ ", " ++ show (special ast)

-- The actor state
-- Tid is specialized to PN.ProcessName
type ActorState s = B.LocalState (ActState s) PN.ProcessName

-- Get the actor Id
actorId :: B.Observer (ActState s) PN.ProcessName PN.ProcessName
actorId = B.access ( actorid  )

-- Return value from a actor braid
data ActorRet
    = Response        { rsp     :: SC.SystemResponse }
    | MQResponse      { mmq     :: Maybe MQD.MqdMap
                      , rsp     :: SC.SystemResponse
                      }
      -- For signal programs changing the signal mask
    | SigResponse     { rsp     :: SC.SystemResponse
                      , sigmap  :: SC.SignalMap
                      }
      -- For signal programs changing the signal set
    | SigProcResponse { rsp     :: SC.SystemResponse
                      , sigset  :: SC.SignalSet
                      }
      -- For signal program changing the alarm condition
    | AlarmResponse   { rsp     :: SC.SystemResponse }
      -- When the process should pause
    | PauseResponse
    | StartAlarmResponse { rsp :: SC.SystemResponse
                         , tcb :: Maybe TCB.TimerControlBlock
                         , ed  :: ED.ExceptionData
                         }
    | ReceiveResponse    { ed  :: ED.ExceptionData
                         }
      -- When no action is required
    | NullReturn
      -- When process will exit
    | Exit
      deriving (Show)
