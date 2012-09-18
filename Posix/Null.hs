-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Null
    ( Null                -- Class with a null value
    , mkNull              -- Produce a null value
    ) where

----------------------------------------------------------------------
-- Class having a reasonable null value
----------------------------------------------------------------------

class Null n where
    mkNull :: n -- Produce a null value

instance Null Int where
    mkNull = 0

instance Null Integer where
    mkNull = 0

instance Null [a] where
    mkNull = []

instance Null (Maybe n) where
    mkNull = Nothing

instance Null () where
    mkNull = ()
