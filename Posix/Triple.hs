-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Triple
    ( Triple (..)  -- A triple of components
    , updateFirst  -- Update the first component
    , updateSecond -- Update the second component
    , updateThird  -- Update the third component
    ) where

----------------------------------------------------------------------
-- Utilities regarding a triple of components
----------------------------------------------------------------------

data Triple a b c =
    Triple { first  :: a
           , second :: b
           , third  :: c
           }

-- Update the second component
updateFirst :: Triple a b c -> d -> Triple d b c
updateFirst t d = Triple { first  = d
                         , second = second t
                         , third  = third t
                         }

-- Update the second component
updateSecond :: Triple a b c -> d -> Triple a d c
updateSecond t d = Triple { first  = first t
                          , second = d
                          , third  = third t
                          }

-- Update the third component
updateThird :: Triple a b c -> d -> Triple a b d
updateThird t d = Triple { first  = first t
                         , second = second t
                         , third  = d
                         }
