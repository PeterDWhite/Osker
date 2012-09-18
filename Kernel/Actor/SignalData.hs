-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module SignalData
    ( PauseCondition  -- Data type of pause condition
    , isPause         -- Check if there is a pause condition
    ) where

----------------------------------------------------------------------
-- Data structures in support of the Osker implementation of the
-- Posix system calls.
----------------------------------------------------------------------

-- The pause condition must store enough information to make a
-- response to the original pause system call.
data PauseCondition = Pause Bool deriving (Show)

-- Check if pause condition is on
isPause :: PauseCondition -> Bool
isPause (Pause c) = c
