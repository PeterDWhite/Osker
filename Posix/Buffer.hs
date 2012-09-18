-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003

module Buffer ( Buffer (..) ) where

----------------------------------------------------------------------
-- A buffer is a currently a String, with a maximum length
----------------------------------------------------------------------

data Buffer = Buffer { bufDat :: String
                     , bufLen :: Int
                     } deriving (Show)
