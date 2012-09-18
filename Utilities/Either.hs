-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Either
    ( isLeft   -- Determine if the item is built with Left constructor
    , isRight  -- Determine if the item is built with Right constructor
    , first    -- Get the element of the first summand, if any
    , second   -- Get the element of the second summand, if any
    ) where

----------------------------------------------------------------------
-- Some more functions for Either
----------------------------------------------------------------------

-- Extract from the first summand
first:: Either a b -> a
first (Left a) = a
first _ = error "Either/First"

-- Extract from the second summand
second:: Either a b -> b
second (Right b) = b
second _ = error "Either/Second"

-- Determine if it is a left
isLeft :: Either a b -> Bool
isLeft (Left _a)  = True
isLeft (Right _b) = False

-- Determine if it is a right
isRight :: Either a b -> Bool
isRight (Left _a)  = False
isRight (Right _b) = True
