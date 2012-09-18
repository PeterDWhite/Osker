-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Thread
    ( Thread          -- Type for a single thread in the braid
    , liftSt          -- Lift a state transformer
    , lifta           -- Lift a transform / observer to thread monad
    , lift            -- Lift an IO action to a thread action
    , liftma          -- Lift a state delta with return value
                      -- from the underlying monad
    , liftSE          -- Lift all the way from SET monad to thread monad
    , step            -- Perform a step of a thread
    , mkThread        -- Constructor for Thread
    , proj            -- Project out of the thread
    , pause           -- Pause raised to the local thread level
    , fetch           -- Fetch raised to the local thread level
    , store           -- Store a value in the state
    , update          -- Update raised to the local thread level
    , observe         -- Observe a feature of the internal state
    , nullPause       -- Take a break
    , pauseSta        -- State change, then pause, then observe the state
    , pauseSt         -- State change, then pause
    , pauseTranserver -- Pause a transerver
    , completeStateM  -- Run resumption, staying in underlying monad
    , putStr          -- Put a string to the screen
    , putStrLn        -- Put a line to the screen
    , runM            -- Run until a pause
    , isDone          -- Determine if thread is done or not.
    , getElapsed      -- Get the elapsed time
    ) where

----------------------------------------------------------------------
-- The local thread
-- Operates only on its own local state
----------------------------------------------------------------------

-- Haskel imports
import Prelude hiding ( putStr, putStrLn )
-- Utility imports
import Null
import MonadUtilities
-- Resumption imports
import qualified RSEIO as R
import qualified SET as S

-- A single thread monad
data Thread ls a = Thread { proj :: (R.RSE ls a) }

-- Constructor for Thread
mkThread :: R.RSE ls a -> Thread ls a
mkThread r = Thread { proj = r }

-- A wrapper on a monad is still a monad
instance Monad (Thread ls) where
    return = Thread . return
    Thread p >>= k = Thread ( p >>= proj . k )

instance (Null a, Null ls) => Null (Thread ls a) where
    mkNull = Thread mkNull

-- Pause raised to the local thread level
pause :: Thread ls a -> Thread ls a
pause (Thread t) = Thread (R.pause t)

-- Fetch raised to the local thread level
fetch :: Thread ls ls
fetch = Thread R.fetch

-- Store a value in the state
store :: ls -> Thread ls ()
store = Thread . R.store

-- Update raised to the local thread level
update :: (ls -> ls) -> Thread ls ()
update = Thread . R.update

-- Observe a feature of the internal state
observe :: (ls -> a) -> Thread ls a
observe = Thread . R.observe

-- Take a break
nullPause :: (Null a) => Thread ls a
nullPause = pause (return mkNull)

-- State change, then pause, then observer on the state
-- This is just a specialization of the underlying resumption pauseSta
pauseSta :: (ls -> ls) -> (ls -> a) -> Thread ls a
pauseSta trans obs = Thread ( R.pauseSta trans obs )

-- State change, then pause
pauseSt :: (Null a) => (ls -> ls) -> Thread ls a
pauseSt = Thread . R.pauseSt

-- Pause a transerver
pauseTranserver :: (ls -> (ls, a)) -> Thread ls a
pauseTranserver = Thread . R.pauseTranserver

-- Lift a function on state
liftSt :: (ls -> ls) -> Thread ls ()
liftSt = Thread . R.liftSt

-- Lift an IO action into the thread monad
lift :: IO a -> Thread ls a
lift = Thread . R.lift

-- Lift a state delta with return value from the underlying monad (IO)
liftma :: (ls -> IO (ls, a)) -> Thread ls a
liftma = Thread . R.liftma

-- Lift a state delta with a return value into the thread monad
lifta :: ( ls -> ( ls, a ) ) -> Thread ls a
lifta = Thread . R.lifta

-- Lift from the SET monad to the Thread monad
liftSE :: S.SET ls IO a -> Thread ls a
liftSE = Thread . R.liftSE

-- Lift a computation on a sub state
-- liftSub :: (s -> s') -> (s' -> s -> s) -> Thread s' () -> Thread s ()
-- liftSub get put sub = Thread (R.liftSub get put (proj sub))

-- Perform a step within the thread resumption monad
step ::
    (ls -> IO (ls', Thread ls' a))                  ->
    ((ls', Thread ls' a) -> IO (ls', Thread ls' a)) ->
    (ls -> (ls', Thread ls' a) -> ls)               ->
    Thread ls ()
step f rn upd = liftSE (S.SET (\s -> f s >>= rn >>= retNull2 . (upd s)))

-- Perform a step within the thread resumption monad
-- stepRun ::
--     (ls -> IO (Run ls' a)) -> (ls -> Run ls' a -> ls) -> Thread ls ()
-- stepRun f upd = liftSE (S.SET (\s -> f s >>= runM >>= retNull2 . (upd s)))

-- Run until a result is returned, and return only the state,
-- this version stays in the underlying monad
completeStateM :: Thread ls a -> ls -> IO ls
completeStateM (Thread t) ls = R.completeStateM t ls

-- Put a string to the screen
putStr :: String -> Thread ls ()
putStr = Thread . R.putString

-- Put a line to the screen
putStrLn :: String -> Thread ls ()
putStrLn = Thread . R.putLine

-- Determine when a thread is done
isDone :: Thread ls a -> Bool
isDone = R.rseDone . proj

-- Run a thread until a pause
-- Also returns a flag when the thread is done
runM :: ls -> Thread ls a -> IO (ls, Thread ls a, Bool)
runM ls thread =
  do { (ls', thread', done ) <- R.runM ls (proj thread)
     ; return (ls', Thread thread', done )
     }

-- Get elapsed time raised to the thread level
getElapsed :: Thread ls Int
getElapsed = Thread R.getElapsed

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkThread         #-}
{-# INLINE pause            #-}
{-# INLINE fetch            #-}
{-# INLINE store            #-}
{-# INLINE update           #-}
{-# INLINE observe          #-}
{-# INLINE nullPause        #-}
{-# INLINE pauseSta         #-}
{-# INLINE pauseSt          #-}
{-# INLINE pauseTranserver  #-}
{-# INLINE liftSt           #-}
{-# INLINE lift             #-}
{-# INLINE liftma           #-}
{-# INLINE lifta            #-}
{-# INLINE liftSE           #-}
{-# INLINE step             #-}
{-# INLINE completeStateM   #-}
{-# INLINE putStr           #-}
{-# INLINE putStrLn         #-}
{-# INLINE runM             #-}
{-# INLINE getElapsed       #-}
