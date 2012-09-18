-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Message
    ( Message (..)    -- Data type for messages
    , Channel         -- Generic message channel
    ) where

---------------------------------------------------------------------
-- The kernel internal message.
---------------------------------------------------------------------

-- Haskell imports
import qualified Dynamic as DYN
-- Braid imports
import qualified OskerConcurrent as C
-- Posix imports
import qualified ProcessName as PN

-- A generic message
data (DYN.Typeable p) =>
    Message p = Message { dest   :: PN.ProcessName
                        , source :: PN.ProcessName
                        , pay    :: p
                        }

type Channel p = C.Chan (Message p)

-- A message is typeable
msgCon :: DYN.TyCon
msgCon = DYN.mkTyCon "Message"
instance DYN.Typeable (Message p) where
    DYN.typeOf _ = DYN.mkAppTy msgCon []
