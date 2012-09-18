-- copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module SET
    ( SET (..)      -- State / exception monad transformer
    , proj          -- Project out the underlying function
    , liftSt        -- Lift a function on state
    , liftE         -- Lift from the exception monad
    , lift          -- Lift from the underlying monad to SET
    , liftma        -- Lift function with return value from underlying monad
    , liftsm        -- Lift func returning (s, a) inside the underlying monad
    , lifta         -- Lift a function with non trivial return value
    , liftb         -- Lift, returning value paired with state
    , liftSub       -- Lift a substate computation
    , step          -- A step in the monad
    , observe       -- Observe a property of the state
    , fetch         -- Extract the state value
    , store         -- Store a new state value
    , update        -- Update the state with a function
    , change        -- Change the state to a new value
    , runM          -- Run a state / exception monad, within underlying monad
      -- In support of exceptions
    , E.E (..)      -- Exception monad data type
    , raise         -- Raise an exception
    , catch         -- Catch an exception
    ) where

----------------------------------------------------------------------
--  The state exception monad transformer
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (catch)
-- Utility imports
import Ers
import Null
import MonadUtilities
-- Local imports
import qualified Ex as E

newtype (Monad m) => SET s m a = SET ( s -> m (s, E.E a) )

instance (Monad m) => Monad (SET s m) where
    return x = SET (\s -> return (s, E.Ok x))
    SET m >>= k = SET ( \s -> m s >>= ( \(s', ea) ->
                                          case ea of
                                            E.Ok a -> let SET k' = k a
                                                      in k' s'
                                            E.Ex e -> return (s', E.Ex e)
                                      )
                      )

instance (Null s, Null a, Monad m) => Null (SET s m a) where
    mkNull = SET ( \_s -> return (mkNull, E.Ok mkNull) )

-- Project out the underlying function from the monad action
proj :: (Monad m) => SET s m a -> (s -> m (s, E.E a))
proj (SET f) = f

-- Get the underlying state transform
-- st :: (Monad m) => SET s m a -> (s -> m s)
-- st (SET f) = \s -> f s >>= return . fst

-- Lift from the exception monad
liftE :: (Monad m) => E.E a -> SET s m a
liftE x = case x of
            E.Ok y -> SET (\s -> return (s, E.Ok y))
            E.Ex e -> SET (\s -> return (s, E.Ex e))

-- Lift from underlying monad
lift :: (Monad m) => m a -> SET s m a
lift m = SET ( \s -> do { a <- m; return (s, E.Ok a) } )

-- Lift a function that returns state and value inside the underlying monad
liftsm :: (Monad m) => (s -> m a) -> (a -> s) -> SET s m a
liftsm f state = SET (\s -> do { a <- f s; return (state a, E.Ok a) } )

-- Lift a function on state
liftSt :: (Monad m) => Transformer s -> SET s m ()
liftSt f = SET ( \s -> return ( f s, E.Ok () ) )

-- Lift a state delta with a return value into the resumption monad
lifta :: (Monad m) => Transerver s a -> SET s m a
lifta f = SET ( \s -> let (s', a) = f s in return ( s', E.Ok a ) )

-- Lift a state delta with return value from the underlying monad
liftma :: (Monad m) => (s -> m (s, a)) -> SET s m a
liftma m = SET ( \s -> m s >>= \ (s', a) -> return (s', E.Ok a) )

-- Lift a computation on a substate
liftSub :: (Monad m) => (s -> s') -> (s' -> s -> s) -> SET s' m a -> SET s m a
liftSub get put sub =
  SET ( \s -> runM (get s) sub >>= \ (s', ea) -> return ( put s' s, ea ) )

-- Lower to a transformer of a feature of the state
-- dropSub :: (Monad m) =>
--     s -> (s -> s') -> (s' -> s -> s) -> SET s m a -> SET s' m a
-- dropSub s1 get put set =
--   SET ( \s' -> let s2   = put s' s1
--                    mset = runM s2 set
--                in mset >>= \(s3, ea) -> return (get s3, ea)
--       )

-- Observe a property of the state
observe :: (Monad m) => Observer s a -> SET s m a
observe f = SET (\s -> return (s, E.Ok (f s)))

-- Lift a state delta with a return value into the resumption monad.
-- The return value is paired with the state upon return.
liftb :: (Monad m) => (s -> (s, a)) -> SET s m (s, a)
liftb f = SET ( \s -> let (s', a) = f s
                      in return (s', (E.Ok (s', a)))
              )

-- Extract the value of the state
fetch :: (Monad m) => SET s m s
fetch = SET (\s -> return (s, E.Ok s))

-- Store a new value of the state
store :: (Monad m) => s -> SET s m ()
store s = SET (\_ -> return (s, E.Ok ()))

-- Update the state with a function of state
update :: (Monad m) => Transformer s -> SET s m ()
update f = SET (\s -> return (f s, E.Ok ()))

-- Change the state to a new value
change :: (Monad m) => s -> SET s m ()
change s = SET (\_ -> return (s, E.Ok ()))

-- Run a state / exception monad, on a specified state
runM :: (Monad m) => s -> SET s m a -> m (s, E.E a)
runM s (SET f) = f s

-- Run a state / exection monad, until a specified condition
-- runTilCond :: (Monad m) => s -> SET s m a -> (E.E a -> Bool) -> m (s, E.E a)
-- runTilCond s (SET f) p =
--   do { (s', ea) <- f s
--      ; if p ea
--        then return (s', ea)
--        else runTilCond s' (SET f) p
--      }

----------------------------------------------------------------------
-- In support of exceptions
----------------------------------------------------------------------

-- Raise an exception
raise :: (Monad m) => E.Exception -> SET s m a
raise = liftE . E.raise

-- Catch an exception
catch :: (Monad m) => SET s m a -> (E.Exception -> SET s m a) -> SET s m a
catch (SET p) h =
  SET ( \s -> let msea = p s
              in msea >>= \ (s', ea) ->
                               case ea of
                                 E.Ok a -> return (s', E.Ok a)
                                 E.Ex e -> let SET h' = h e
                                           in h' s'
      )

----------------------------------------------------------------------
-- Some step functions in the state / exception monad
----------------------------------------------------------------------

-- This step first gets a subcomputation of the monad, then "runs"
-- that subcomputation, When the subcomputation is run, it produces
-- a new subcomputation. This new subcomputation is assumed to be
-- incorporated into the new state. ....
-- Now that it is defined here, it is no longer needed, I just defined
-- it so that I could figure out how it works in the simplest setting
step :: (Monad m) =>
    (s -> m (s', SET s' m a))                ->
    ((s', SET s' m a) -> m (s', SET s' m a)) ->
    (s -> (s', SET s' m a) -> s)             ->
    SET s m ()
step f run upd = SET (\s-> f s >>= run >>= retNull2 . (upd s))

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE proj    #-}
{-# INLINE liftE   #-}
{-# INLINE lift    #-}
{-# INLINE liftma  #-}
{-# INLINE liftsm  #-}
{-# INLINE liftSt  #-}
{-# INLINE lifta   #-}
{-# INLINE liftSub #-}
{-# INLINE observe #-}
{-# INLINE liftb   #-}
{-# INLINE fetch   #-}
{-# INLINE store   #-}
{-# INLINE update  #-}
{-# INLINE runM    #-}
{-# INLINE raise   #-}
{-# INLINE catch   #-}
{-# INLINE step    #-}
