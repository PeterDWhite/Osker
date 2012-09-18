-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003

-----------------------------------------------------------------------------
-- MonadRec Library
--
-- Suitable for use with the mdo extension of Hugs 98
-- Stolen from Levent Erkok :-)
-----------------------------------------------------------------------------

module MonadRec (MonadRec(mfix), fix) where
    
fix :: (a -> a) -> a
fix f = let a = f a in a

{-P: {-< Fixed point laws >-}
assert FixedPoint =
    All f :: a -> a.
        { f (fix f) } === { fix f }
 -}

-- The MonadRec class definition

class Monad m => MonadRec m where
    mfix    :: (a -> m a) -> m a 

{-P: {-< Monadic fix point laws >-}
assert Purity =
    All h :: a -> a.
        { mfix (return . h) } === { return (fix h) }

assert LeftShrinking =
    All f :: a -> b -> m a.
    All k :: m b.
        { mfix ( \x -> ( k >>= f x ) ) } ===
        { k >>= \y -> mfix( \x -> f x y ) }

assert Nesting =
    All f :: (a, a) -> m a.
       { mfix (\x -> mfix (\y -> f (x,y))) } === { mfix ( \x -> f (x, x) ) }

-- An assertion that is equivalent to nesting.
assert MultipleVariables =
    All f :: (a, b) -> m (a, b).
       { mfix (\ (x, _) -> mfix (\ (_, y) -> f ( x, y ))) } === { mfix f }

 -}

-- Instances of MonadRec

-- Maybe:
instance MonadRec Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust Nothing = error "unJust"

-- List:
instance MonadRec [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)
