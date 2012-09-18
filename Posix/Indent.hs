-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Indent ( indent ) where

----------------------------------------------------------------------
-- Indent a string
----------------------------------------------------------------------

-- Indent a string, tabs at 3
indent :: Int -> String
indent n = replicate (n * 3) ' '
