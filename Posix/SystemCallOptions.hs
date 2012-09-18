-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SystemCallOptions
    ( SystemCallOptions (..)
    ) where

----------------------------------------------------------------------
-- A system call can be either blocking or non blocking
----------------------------------------------------------------------

-- Haskell imports
import Dynamic ( TyCon, Typeable, mkTyCon, typeOf, mkAppTy )

data SystemCallOptions
    = Blocking
    | NonBlocking
    deriving (Eq, Ord, Enum, Show)

sysOptCon :: TyCon
sysOptCon = mkTyCon "System Call Option"
instance Typeable SystemCallOptions where
    typeOf _ = mkAppTy sysOptCon []
