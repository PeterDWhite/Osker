-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Statistics ( Statistics (..) ) where

----------------------------------------------------------------------
-- Specify the operations expected on things that are statistics
----------------------------------------------------------------------

class Statistics s where
    initialStatistics :: s
