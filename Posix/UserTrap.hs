-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module UserTrap ( UserTrap (..) ) where

----------------------------------------------------------------------
-- The trap message defines the requests and response of the trap
-- handler thread
----------------------------------------------------------------------

-- Haskell imports
import qualified Dynamic as DYN
import Dynamic ( TyCon, Typeable, mkTyCon, typeOf, mkAppTy )
-- Posix imports
import qualified SystemCall as SC
import qualified SystemCallOptions as SCO

-- For inputs from the user process
data UserTrap tid =
    UserTrap { utSysReq   :: SC.SystemRequest
             , utTid      :: tid
             , utCallType :: SCO.SystemCallOptions
             } deriving (Show)

utCon :: TyCon
utCon = mkTyCon "User Trap"
instance Typeable (UserTrap tid) where
    typeOf _ = mkAppTy utCon []
