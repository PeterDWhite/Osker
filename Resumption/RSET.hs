-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module RSET
    ( RSET               -- RSE monad transformer
    , liftSt             -- Lift a function on state
    , lift               -- Lift from the underlying monad to RSET
    , liftma             -- Lift function with return value from under monad
    , lifta              -- Lift a function with non trivial return value
    , liftb              -- Lift, returning value paired with state
    , liftSE             -- Lift from the SET monad to RSET monad
    , step               -- A step in the resumption monad
    , pause              -- Cause a pause in the resumption monad
    , pauseSt            -- Cause a pause with a state change
    , pauseSta           -- Cause a pause with a state change and
                         -- return a function of state after the pause
    , pauseTranserver    -- Pause a transerver
    , nullPause          -- A pause of nothing
    , observe            -- Observe a property of the state
    , fetch              -- Fetch the current state value
    , store              -- Store a new value of the real world
    , update             -- Update the state, with a function
    , completeStateM     -- Run resumption, staying in underlying monad
    , runM               -- Run resumption til pause, within underlying monad
    , rseDone            -- Determine when resumption is done
      -- In support of exception handling
    , E.E (..)           -- Data type of exception monad
    , E.unOk             -- Project out good values
    , raise              -- Raise an exception
    , catch              -- Catch an exception
      -- In support of output to the screen
    ) where

----------------------------------------------------------------------
--  The resumption monad, mixing state with resumptions
--  The monad laws:
--    return x >>= f = f x
--    m >>= return = m
--    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--
--  In this file, the resumption / state / exception monad is built
--  up using the resumption monad transformer on the
--  state / exception monad.
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (catch)
-- Utility imports
import Null
import MonadUtilities
import Ers (Transerver)
-- Local imports
import qualified SET as S
import qualified Ex as E
import qualified Resumption as R

-- Resumption state monad transformer
data RSET s m a = RSET { proj :: R.R (S.SET s m) a }

instance (Monad m) => Monad (RSET s m) where
    return = RSET . return
    RSET m >>= k = RSET (m >>= \a -> let RSET k' = k $! a in k')

instance (Monad m, Null s, Null a) => Null (RSET s m a) where
    mkNull = RSET (R.lift mkNull)

-- Determine when an RSE is done
rseDone :: (Monad m) => RSET s m a -> Bool
rseDone = R.isContinue . proj

-- Lift from the exception monad to the RSE monad
liftE :: (Monad m) => E.E a -> RSET s m a
liftE = liftSE . S.liftE

-- Lift from the state / exception monad to the RSE monad
liftSE :: (Monad m) => S.SET s m a -> RSET s m a
liftSE = RSET . R.lift

-- Lift a function on state
liftSt :: (Monad m) => (s -> s) -> RSET s m ()
liftSt = liftSE . S.liftSt

-- Lift from underlying monad
lift :: (Monad m) => m a -> RSET s m a
lift = liftSE . S.lift

-- Lift a state delta with a return value into the resumption monad
lifta :: (Monad m) => (s -> (s, a)) -> RSET s m a
lifta = liftSE . S.lifta

-- Lift a state delta with return value from the underlying monad
liftma :: (Monad m) => (s -> m (s, a)) -> RSET s m a
liftma = liftSE . S.liftma

-- Lift a state delta with a return value into the resumption monad.
-- The return value is paired with the state upon return.
liftb :: (Monad m) => (s -> (s, a)) -> RSET s m (s, a)
liftb = liftSE . S.liftb

-- Raise the pause function to the RSET level
pause :: (Monad m) => RSET s m a -> RSET s m a
pause r = RSET (R.pause (proj r))

-- Cause a pause with a state change
-- ***** Very laborious, simplify
pauseSt :: (Monad m, Null a) => (s -> s) -> RSET s m a
pauseSt f = RSET (R.pause (proj (update f)) >> return mkNull)

-- Cause a pause with a state change, and after the pause
-- return a function of the state
-- **** Kill the extra fetch
pauseSta :: (Monad m) => (s -> s) -> (s -> a) -> RSET s m a
pauseSta f g = -- RSET (R.pause (proj (update f))) >> fetch >>= return . g
  liftSE (S.SET (\s -> let { s' = f s; a = g s } in return (s', E.Ok a)))

-- Pause a transerver
pauseTranserver :: (Monad m) => Transerver s a -> RSET s m a
pauseTranserver f =
  liftSE (S.SET (\s -> let (s', a) = f s in return (s', E.Ok a)))

-- A pause returning nothing
nullPause :: (Monad m, Null a) => RSET s m a
nullPause = pauseSt id

-- Observe a property of the state
observe :: (Monad m) => (s -> a) -> RSET s m a
observe = liftSE . S.observe

-- Fetch the state component of the RSE
fetch :: (Monad m) => RSET s m s
fetch = RSET (R.lift S.fetch)

-- Store a new state variable
store :: (Monad m) => s -> RSET s m ()
store = liftSE . S.store

-- Update the state, according to a function
update :: (Monad m) => (s -> s) -> RSET s m ()
update = liftSE . S.update

-- Run the resumption monad, i.e. uncover the function and
-- execute it. Because of the structure of RSE computations noted
-- above, if an RSE computation is presented in "do" notation, the
-- effect of run will be to execute statements until a computation
-- step that returns a Pause constructor. I.e., the
-- run function will compose all the continues leading up to the
-- Pause and then return the result of the Pause.
-- This version stays in the underlying monad
-- Addition: Returning a flag to determine when the thread is done
runM :: (Monad m) => s -> RSET s m a -> m (s, RSET s m a, Bool)
runM s (RSET (R.Continue a)) = return ( s, return a, True )
runM s (RSET (R.Pause rse)) =
  (S.proj rse) s >>= \ (s', exrse) ->
      case exrse of
        E.Ok rse' -> return (s', RSET rse', False )
        E.Ex e    -> return (s', raise e, False )

-- Run until a result is returned, and return only the state,
-- this version stays in the underlying monad
completeStateM :: (Monad m) => RSET s m a -> s -> m s
completeStateM (RSET (R.Continue _)) s = return s
completeStateM (RSET (R.Pause m)) s0 =
  do { ( s1, em' ) <- S.runM s0 m
     ; case em' of
         E.Ok m' -> completeStateM (RSET m') s1
         E.Ex _e -> error "completeStateM: An exception occured"
     }

-- Raise an exception
raise :: (Monad m) => E.Exception -> RSET s m a
raise = liftE . E.raise

-- Helper for catch
dropE :: (Monad m) =>
    (E.Exception -> RSET s m a) -> (E.Exception -> S.SET s m a)
dropE h = \e -> let RSET r = h e in R.run r

-- Catch an exception
catch :: (Monad m) =>
    RSET s m a -> (E.Exception -> RSET s m a) -> RSET s m a
catch (RSET (R.Continue v)) _h = RSET (R.Continue v)
catch (RSET (R.Pause _m)) h =
  RSET (R.lift (S.catch undefined (dropE h))) -- Fix this later!!!!!

----------------------------------------------------------------------
-- Some step functions in the resumption monad
----------------------------------------------------------------------

-- Run a step in the resumption monad.
-- First get a subcomputation, on a substate
-- Then run the subcomputation on the substate, resulting in a new
-- subcomputation and a new substate.
-- Finally use the new subcomputation and the new substate to update
-- the global state.
-- This function is in preparation for lifting of subcomputations and
-- separation in the braid.
-- In this function, we want to dig down into the SET level, to
-- avoid lots of unecessary state manipulations, and extra monadic
-- bind operations.
-- It took me two weeks to write this one line of code, so don't mess with it!
step :: (Monad m) =>
    (s -> m (s', RSET s' m a))                 ->
    ((s', RSET s' m a) -> m (s', RSET s' m a)) ->
    (s -> (s', RSET s' m a) -> s)              ->
    RSET s m ()
step f rn upd = liftSE (S.SET (\s -> f s >>= rn >>= retNull2 . (upd s)))

-- Specialize the step to the runM function defined for the resumption
-- monad
-- stepRun :: (Monad m) =>
--     (s -> m (Run s' m a)) -> (s -> Run s' m a -> s) -> RSET s m ()
-- stepRun f upd = liftSE (S.SET (\s -> f s >>= runM >>= retNull2 . (upd s)))

-- Lift a computation on a substate
-- liftSub :: (Monad m) =>
--   (s -> s') -> (s' -> s -> s) -> RSET s' m () -> RSET s m ()
-- liftSub get put sub =
--   liftSE ( S.SET ( \s -> let rn = Run { state = get s, cont = sub }
--                          in runM rn >>= \ rn' -> return ( put (state rn') s,
--                                                           E.Ok ()
--                                                         )
--                  )
--          )

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE rseDone        #-}
{-# INLINE liftE          #-}
{-# INLINE liftSE         #-}
{-# INLINE liftSt         #-}
{-# INLINE lift           #-}
{-# INLINE liftma         #-}
{-# INLINE lifta          #-}
{-# INLINE liftb          #-}
{-# INLINE pause          #-}
{-# INLINE pauseSt        #-}
{-# INLINE pauseSta       #-}
{-# INLINE nullPause      #-}
{-# INLINE observe        #-}
{-# INLINE fetch          #-}
{-# INLINE store          #-}
{-# INLINE update         #-}
{-# INLINE runM           #-}
{-# INLINE completeStateM #-}
{-# INLINE raise          #-}
{-# INLINE dropE          #-}
{-# INLINE catch          #-}
{-# INLINE step           #-}
