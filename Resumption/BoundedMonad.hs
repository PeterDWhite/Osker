-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module BoundedMonad
    ( BdM (..)       -- Bounded monad type
    , mTo            -- Convert underlying monad to BdM
    , toM            -- Convert BdM to underlying monad
    , pairM          -- Pair return of a monad action with specified value
    , lifta          -- Lift a function with non trivial return value
    ) where

----------------------------------------------------------------------
-- Bounded IO: A state monad with a very restricted
-- set of IO primitives.
--
-- The monad laws:
--    return x >>= f = f x
--    m >>= return = m
--    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--
----------------------------------------------------------------------

sndM :: (Monad m) => m (a, x) -> m x
sndM m = do { (_a, x) <- m; return x }

-- Pair the return of a monad action with the specified value
pairM :: (Monad m) => x -> m a -> m (x, a)
pairM x m = do { a <- m; return (x, a) }

-- The bounded IO monad, defined just like the state monad transformer.
newtype (Monad m) => BdM m b a = BdM (b ->  m (b, a))

-- Convert the BdM to the underlying monad
toM :: (Monad m) => b -> BdM m b a -> m a
toM bounds (BdM f) = sndM ( f bounds )

 -- Convert the underlying monad to BdM
mTo :: (Monad m) => m a -> BdM m b a
mTo m = BdM (\b -> pairM b m)

-- Lift a transerver to the bounded monad
lifta :: (Monad m) => (b -> (b, x)) -> BdM m b x
lifta f = BdM (\b -> return (f b))

-- We have made the monad operations strict
instance (Monad m) => Monad (BdM m b) where
    return x = BdM $! (\b -> return $! (b, x))
    p >>= k = BdM $! (\b -> let BdM p' = p
                            in (p' $! b) >>= (\(b', a) ->
                                                let BdM k' = k $! a
                                                in k' $! b'))

----------------------------------------------------------------------
-- Proofs of the monad laws for SystemHalfBdM
----------------------------------------------------------------------

{-
----------------------------------------------------------------------
**** First law:  return x >>= f = f x
----------------------------------------------------------------------

return x >>= f
= BdM (\b -> return (x, b)) >>= f
= BdM (\b' -> let BdM p' = BdM (\b -> return (x, b))
              in p' b' >>= (\ (a, b'') -> let BdM f' = f a
                                          in f' b''))
= BdM (\b' -> let p' = \b -> return (x, b)
              in p' b' >>= (\ (a, b'') -> let BdM f' = f a
                                          in f' b''))
= BdM (\b' -> return (x, b') >>= (\ (a, b'') -> let BdM f' = f a
                                                in f' b''))
= BdM (\b' -> let BdM f' = f x in f' b')
= BdM (\b' -> f' b') where BdM f' = f x
= BdM f' where BdM f' = f x
= f x

----------------------------------------------------------------------
**** Second law:  m >>= return = m
----------------------------------------------------------------------
BdM p >>= return
= BdM (\b -> let BdM p' = p
             in p' b >>= (\ (a, b') -> let BdM k' = return a
                                       in k' b'))
= BdM (\b -> let BdM p' = p
             in p' b >>= (\ (a, b') -> let BdM k' = BdM (\b'' -> return (a, b''))
                                       in k' b'))
= BdM (\b -> let BdM p' = p
             in p' b >>= (\ (a, b') -> let k' = \b'' -> return (a, b'')
                                       in k' b'))
= BdM (\b -> let BdM p' = p
             in p' b >>= (\ (a, b') -> return (a, b')))
= BdM (\b -> let BdM p' = p in p' b)
= BdM (\b -> p' b) where BdM p' = p
= BdM p' where BdM p' = p
= p

----------------------------------------------------------------------
**** Third law: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
----------------------------------------------------------------------
****** Do this one over.....
(BdM p >>= f) >>= g
= BdM (do { x <- p
            ; let BdM q = f x
              in q
            }) >>= g
= BdM (do { x' <- do { x <- p
                       ; let BdM q = f x
                         in q
                       }
            ; let BdM q' = g x'
              in q'
            })
= BdM (do { x <- p
            ; let BdM q = f x
            ; x' <- q
            ; let BdM q' = g x'
              in q'
            })

**** Now expand the other side, namely:  m >>= (\x -> f x >>= g)
BdM p >> (\x -> f x >>= g)
= BdM (do { x <- p
            ; let BdM q = (\x -> f x >>= g) x
              in q
            })
= BdM (do { x <- p
            ; let BdM q = f x >>= g
              in q
            })

**** Now let BdM m = f x
= BdM (do { x <- p
            ; let BdM m = f x
                  BdM q = f x >>= g
              in q
            })
= BdM (do { x <- p
            ; let BdM m = f x
                  BdM q = BdM (do { x' <- m
                                      ; let BdM q' = g x'
                                        in q'
                                      }
                                  )
              in q
            })
= = BdM (do { x <- p
              ; let BdM m = f x
                    q = do { x' <- m
                           ; let BdM q' = g x'
                             in q'
                           }
                in q
              })
= = BdM (do { x <- p
              ; let BdM m = f x
              ; x' <- m
              ; let BdM q' = g x'
                in q'
              })

**** This is the same as the left hand side (with some renamings)
**** This completes the proof of the third monad law for BdM
 -}
