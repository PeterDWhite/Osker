-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module IntGen
    ( IntGen (..) -- Class of things that can be generated
    , initGen     -- Initialize a generator
    , genNew      -- Generate a new integer
    , genDel      -- Delete a random number
    ) where

----------------------------------------------------------------------
-- A generator for unique integers
----------------------------------------------------------------------

-- Haskell imports
import qualified Random as RAN
import qualified List as L

genNew :: IntGen -> (IntGen, Int)
genNew ig =
  let (n, nseed) = RAN.next (hgSeed ig)
  in if L.elem n (hgOpen ig)
     then genNew ( ig { hgSeed = nseed } )
     else ( ig { hgSeed = nseed
               , hgOpen = n : (hgOpen ig)
               }
          , n
          )

genDel :: IntGen -> Int -> IntGen
genDel ig n = ig { hgOpen = L.delete n (hgOpen ig) }

-- All items generated will be unique
data IntGen = 
    IntGen { hgSeed     :: RAN.StdGen -- Seed for random generation
           , hgOpen     :: [Int]      -- List of randoms currently in use
           } deriving (Show)

initGen :: Int -> IntGen
initGen seed = IntGen { hgSeed = RAN.mkStdGen seed
                      , hgOpen = []
                      }
