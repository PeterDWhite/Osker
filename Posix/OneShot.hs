-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module OneShot
    ( OneShot
    , newOneShot
    , shootOneShot
    , shootManyOneShot
    , shoot
    , waitToBeShot
    ) where

-- Haskell imports
import qualified Concurrent as C

type OneShot = C.MVar Int
shoot :: Int
shoot = 0

-- Make a new one shot
newOneShot :: IO OneShot
newOneShot = C.newEmptyMVar

-- Send the signal to the one shot
shootOneShot :: OneShot -> Int -> IO Int
shootOneShot oneshot j =
  do { C.putMVar oneshot j;
     ; j' <- C.takeMVar oneshot
--     ; C.yield
     ; return j'
     }

waitToBeShot :: OneShot -> IO ()
waitToBeShot o =
  do { j <- C.takeMVar o
     ; C.putMVar o (j + 1)
     }

-- Shoot a bunch of one shots. This is done such that
-- the threads stimulated will run in the order specified
-- by the list of one shots.
shootManyOneShot :: [OneShot] -> Int -> IO ()
shootManyOneShot [] _j = return ()
shootManyOneShot (o:os) j =
  do { j' <- shootOneShot o j
     ; shootManyOneShot os j'
     }
