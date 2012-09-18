-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module TestData where

-- Utility imports
import Null

-- Data structures for the braids in the test programs.
data Gs  = Gs -- Global state
instance Null Gs where
    mkNull = Gs

instance Show Gs where
    show Gs = "Gs"

data Ls  = Ls -- Local State
instance Null Ls where
    mkNull = Ls

instance Show Ls where
    show Ls = "Ls"

data Ms  = Ms -- Micro State
instance Null Ms where
    mkNull = Ms

instance Show Ms where
    show Ms = "Ms"
