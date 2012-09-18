-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Krenz
    ( Filter (..) -- The type of Krenz filters
    , idFilter    -- The identity filter
    , zeroFilter  -- The zero filter
    ) where

----------------------------------------------------------------------
-- The filters for the Krenz model of security
----------------------------------------------------------------------

-- For now, A Krenz filter is a string transformer
data Filter = Filter (String -> String)

instance Show Filter where
    show _ = "Filter *"

-- The identity filter
idFilter :: Filter
idFilter = Filter id

-- The zero filter
zeroFilter :: Filter
zeroFilter = Filter (\_s -> "")
