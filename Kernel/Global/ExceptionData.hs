-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ExceptionData
    ( ExceptionData (..)  -- Data type for exception info to user half
    , mkExceptionData     -- Constructor for exception data
    ) where

-- Posix imports
import qualified SystemCall as SC

----------------------------------------------------------------------
-- The message payloads for exception handling
----------------------------------------------------------------------
data ExceptionData
    = Ignore
    | Terminate
    | Continue
    | Catch SC.ExceptionHandler SC.Signal

instance Show ExceptionData where
    show Ignore        = "Ignore"
    show Terminate     = "Terminate"
    show Continue      = "Continue"
    show (Catch _ sig) = "Catch " ++ show sig

mkExceptionData :: SC.Signal -> SC.SignalAction -> ExceptionData
mkExceptionData sig sigact =
  case sigact of
    SC.IgnoreSignal     -> Ignore
    SC.ContinueProcess  -> Continue
    SC.Catch act        -> Catch (SC.saHandler act) sig
    SC.TerminateProcess -> Terminate
    SC.NullSignalAction -> error ("mkExceptionData/Null:" ++ show sig)

