-- Copyright (c) Peter Duncan White, 2003
module BoundedBraid
    ( module B    -- Re-export braid stuff
    , lift        -- Lift an IO action to the braid
    ) where

----------------------------------------------------------------------
-- The bounded monad applied to a braid
----------------------------------------------------------------------

-- Resumption imports
import qualified BoundedMonad as BdM
-- Braid imports
import qualified BraidExternal as B

-- Lift an IO action to a BdM action
lift :: IO a -> BdM.BdM (B.Braid hs ls) b a
lift io = BdM.BdM ( \b -> BdM.pairM b (B.lift io) )
