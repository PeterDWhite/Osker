-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Resource
    ( Resource (..)    -- Class of resources
    ) where

----------------------------------------------------------------------
-- Class of resources. The method zeroized is used to initialize a
-- resource when it is obtained from a pool.
----------------------------------------------------------------------

class Resource r where
    zeroize      :: r -> r
