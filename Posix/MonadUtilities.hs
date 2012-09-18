-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module MonadUtilities
    ( listM               -- Convert list of actions into a single action
    , listMUnit           -- Like listM, specialized to ()
    , listMaybe           -- List of (Maybe a) into single action  [a]
    , repeatM             -- Repeat an action n times in sequence
    , mapListUnit         -- Generate list of actions and do them
    , mapMaybe            -- Same as map, but propagate Nothing returns
    , foldWithState       -- Propagate a state through seq of braid actions
    , foldWithStateSnd    -- Propagate a state through seq of braid actions
    , foldStateCombinator -- Fold with specified combinator
    , retNull2            -- Return a null value as the second component
    , pairM               -- Pair two monad utilities
    ) where

-- Utility imports
import Null

-- Convert a list of actions into a single action
listM :: (Monad m) => [m a] -> m [a]
listM mss = foldr conM nilM mss
  where nilM = return []
        conM m ms = do { r <- m
                       ; rs <- ms
                       ; return (r:rs)
                       }

-- Do a list of actions returning unit
listMUnit :: (Monad m) => [m ()] -> m ()
listMUnit [] = return ()
listMUnit (m:ms) = m >> (listMUnit ms)

-- Convert a list of actions returning a maybe into a
-- single action returning a definite value
listMaybe :: (Monad m) => [m (Maybe a)] -> m [a]
listMaybe [] = return []
listMaybe (m:ms) =
  do { mr <- m
     ; rs <- listMaybe ms
     ; case mr of
         Nothing -> return rs
         Just r  -> return (r:rs)
     }

-- Generate a list of Braid actions, and do them
mapListUnit :: (Monad m) => (a -> m ()) -> [a] -> m ()
mapListUnit f xs = listMUnit (map f xs)

-- Given a way to generate an action returning a maybe,
-- create a sequence of actions, and compress the sequence
-- into a single action returning a definite answer
mapMaybe :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybe f xs = listMaybe (map f xs)

-- Repeat an action n times in sequence
repeatM :: (Monad m) => Int -> m a -> m a
repeatM 0 _action = error "Cannot repeat 0 times"
repeatM 1 action = action
repeatM n action = action >> repeatM (n - 1) action

-- Propagate a state through a sequence of braid actions
foldWithState :: (Monad m) =>
    (a -> s -> m (b,s)) -> s -> [a] -> m ([b], s)
foldWithState _f s [] = return ([], s)
foldWithState f s (x:xs) =
  do { ( b, s' )   <- f x s
     ; ( bs, s'' ) <- foldWithState f s' xs
     ; return ( b:bs, s'' )
     }

-- Propagate a state through a sequence of braid actions
-- With return as second part of the pair
foldWithStateSnd :: (Monad m) =>
    (a -> s -> m (s,b)) -> s -> [a] -> m (s, [b])
foldWithStateSnd _f s [] = return (s, [])
foldWithStateSnd f s (x:xs) =
  do { ( s', b )   <- f x s
     ; ( s'', bs ) <- foldWithStateSnd f s' xs
     ; return ( s'', b:bs )
     }

-- Fold with the specfied combinator
foldStateCombinator :: (Null b, Monad m) =>
    (a -> s -> m (s,b)) -> -- Each step returns state and value
    (b -> b -> b)       -> -- Combinator
    s                   -> -- Initial state
    [a]                 -> -- List to fold
    m (s, b)               -- Resulting braid action
foldStateCombinator _f _c s [] = return ( s, mkNull )
foldStateCombinator f c s (x:xs) =
  do { ( s', b1 )  <- f x s
     ; ( s'', b2 ) <- foldStateCombinator f c s' xs
     ; return ( s'', c b1 b2 )
     }

-- Return a null value as the second component
retNull2 :: (Monad m, Null a) => s -> m (s, a)
retNull2 s = return (s, mkNull)

-- Pair two monad actions, returning a result pair
pairM :: (Monad m) => m a -> m b -> m (a, b)
pairM ma mb = ma >>= \a -> mb >>= \b -> return (a, b)
