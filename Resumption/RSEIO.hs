-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module RSEIO
    ( RSE                  -- RSE transformer applied to the identity
    , R.liftSt             -- Lift a function on state
    , R.lift               -- Lift from the underlying monad to SET
    , R.liftma             -- Lift function with return value from under monad
    , R.lifta              -- Lift a function with non trivial return value
    , R.liftb              -- Lift, returning value paired with state
    , R.liftSE             -- Lift from the SET monad to RSET monad
    , R.step               -- Perform a step in the resumption monad
    , R.pause              -- A pause in the resumption action
    , R.pauseSt            -- Cause a pause with a state change
    , R.pauseSta           -- Cause a pause with a state change, and after
                           -- the pause return a function of state
    , R.pauseTranserver    -- Pause a transerver
    , R.nullPause          -- A pause of nothing
    , R.observe            -- Observe a property of the state
    , R.fetch              -- Fetch the current state value
    , R.store              -- Store a new value of the real world
    , R.update             -- Update the state, with a function
    , R.completeStateM     -- Run resumption, staying in underlying monad
    , R.runM               -- Run a resumption until a pause, within underlying
                           -- monad
    , R.rseDone            -- Determine when resumption is done
      -- In support of output to the screen
    , putString            -- Put a string on the screen
    , putLine              -- Put a string with a line terminator on the screen
      -- In support of timing
    , threadDelay          -- Delay for specified ticks
    , getElapsed           -- Get elapsed time, in the RSE monad
    ) where

----------------------------------------------------------------------
--  The resumption monad, mixing state with resumptions
--  This version uses the IO monad as the base monad
----------------------------------------------------------------------

-- Utility imports
import qualified IOUtilities as IOU
-- Local imports
import qualified RSET as R

type RSE s a = R.RSET s IO a
--type Run s a = R.Run s IO a

-- The RSE version of the Haskell function putStr.
putString :: String -> RSE s ()
putString =  R.lift . putStr

-- The RSE version of the Haskell function putStrLn.
putLine :: String -> RSE s ()
putLine = R.lift . putStrLn

-- delay n ticks
threadDelay :: Int -> RSE s ()
threadDelay n = getElapsed >>= threadDelay' n

-- Get the current elapsed time
getElapsed :: RSE s Int
getElapsed = R.lift IOU.getElapsed >>= return . fromEnum

-- Helper for thread delay
-- Elapsed timer goes 100 per second,
-- the thread delay is in microseconds.
threadDelay' :: Int -> Int -> RSE s ()
threadDelay' n elapsed =
  do { elapsed' <- getElapsed
     ; if elapsed' - elapsed > n `div` 10000
       then return ()
       else R.nullPause :: RSE s () >> threadDelay' elapsed n
     }

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE putLine     #-}
{-# INLINE putString   #-}
{-# INLINE threadDelay #-}
{-# INLINE getElapsed  #-}
