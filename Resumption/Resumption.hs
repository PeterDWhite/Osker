-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Resumption
    ( R (..)       -- Resumption transformer without state
    , run          -- Run a simple resumption
    , lift         -- Lift underlying monad into resumpton monad
    , pause        -- Produce a computation that pauses
    , nullPause    -- Pause with () return
    , continuation -- Get continuation out of a resumption value
    , isContinue   -- Determine when the resumption is at a Continue
    ) where

----------------------------------------------------------------------
-- Vanilla resumption monad, from "A study of evaluation order
-- semantics in expressions with side effects" by Nikolaos S.
-- Papaspyrou and Dragan Aacos, 2000.
-- We rely on the papers of Papaspyrou for the proofs of the monad
-- laws for the resumption monad transformer.
--
--  The resumption monad
--  The monad laws:
--    return x >>= f = f x
--    m >>= return = m
--    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
----------------------------------------------------------------------

-- The resumption monad transformer
data (Monad m) => R m a = Continue a | Pause (m (R m a))

-- Watch out! The monadic bind here is not strict. Operations
-- that affect the real world may require a "seq" in them to
-- guarantee that the real world will indeed be affected.
instance (Monad m) => Monad (R m) where
    return x         = Continue x
    Continue x >>= f = f $! x
    Pause m    >>= f = Pause (m >>= \r -> return (r >>= f))

instance (Monad m, Show a) => Show (R m a) where
    show (Continue a) = "Continue " ++ show a
    show (Pause _m)   = "Pause *"

-- Determine if a resumption is at a Continue
isContinue :: (Monad m) => R m a -> Bool
isContinue (Continue _) = True
isContinue (Pause _)    = False

-- Run a resumption monad, i.e. make it produce something in the
-- underyling monad. This will run until a pause occurs. This could
-- also be called "drop", since it drops back to the underlying monad
run :: (Monad m) => R m a -> m a
run (Continue v) = return v
run (Pause m)    = m >>= run

-- Run a single step of the resumption monad. This is also how
-- a value in the underlying monad is lifted to the resumption monad
-- Called "step" by Papaspyrou
lift :: (Monad m) => m a -> R m a
lift m = Pause (m >>= (return . Continue))

-- Get the continuation part of the resumption
continuation :: (Monad m) => R m a -> m (R m a)
continuation (Continue _a) = error "continuation / Continue"
continuation (Pause m)     = m

-- Insert a pause into a computation
pause :: (Monad m) => R m a -> R m a
pause r = Pause (return r)

-- A null computation in the monad R m ()
nullR :: (Monad m) => R m ()
nullR = return ()

-- Pause with a null computation
nullPause :: (Monad m) => R m ()
nullPause = pause nullR

{----------------------------------------------------------------------
-- Property: run . lift = id
-- Proof:
run (lift m)
-- Definition of lift
= run (Pause (m >>= return . Continue
-- Definition of run
= m >>= return . Continue >>= run
-- Definition of return (from monad)
= m >>= run . Continue
-- Definition of run
= m >>= return
-- Right unit (monad)
= m
.Qed

-- Property: lift . run = id
-- Rewrite as (lift (run r)) = r
-- Want both sides equal under run (defintion of equality for R m a)
-- i.e. want run (lift (run r)) = run r
-- This is true by property run . lift = id
.Qed

-- Property: run (Pause (return (Continue v))) = return v
run (Pause (return (Continue v)))
-- Definition of run
= return (Continue v) >>= run
-- Definition of return (Monad)
= run (Continue v)
-- Definition of run
= return v
-- Boths side, when run in the underlying monad m, will produce v,
-- Thus they are equal in the equality on the underlying monad m.

-- Property: run distributes over >>=
-- run (r >>= s) = run r >>= \x.run (s x)
-- Proof: Case 1: r = Continue v
LHS = run (Continue v >>= s)
-- Defintion of >>=(R)
= run (s v)
RHS = run (Continue v) >>= \x.run (s x)
-- Definition of run
= return v >>= \x.run (s x)
-- Monad retrn property
= run (s x)
-- LHS = RHS in case 1
-- Proof: Case 2: r = Pause m
LHS = run (Pause m >>= s)
-- Definition >>=(R)
= run (m >>= \r.return (r >>= s))
-- Some kind of induction or coinduction?
= run m >>= \r.run (return (r >>= s))
-- Case 2a: m emits a resumption of the form Continue w
LHS = run (Continue w) >>= \r.run (return (r >>= s))
-- Definition of run
= run (return (Continue w >>= s))
-- Definition of >>=(R)
= run (return (s w))
-- Definition of return(R)
= run (Continue (s w))
-- Definition of run
= run (s w)
-- So LHS = RHS in case 2a
-- Case 2b: m emits a resumption of the form Pause n
LHS = run (return (Pause n >> s))

RHS = run (Pause m) >>= \x.run (s x)
-- Definition of run
= (m >>= run) >>= \x.run (s x)
-- Case 2a: m emits a resumption of the form Continue w
RHS = run (Continue w) >>= \x.run (s x)
-- Definition of run
= return w >>= \x.run (s x)
-- return monad properties
= run (s w)
-- Case 2b: m emits a resumption of the form Pause n
RHS = (Pause n >>= run) >>= \x. run (s x)
-- Definition of >>=(R)
= Pause (n >>= \r.return (r >>= run)) >>= \x.run (s x)
-- Definition of >>=(R)
= Pause ((n >>= \r.return(r>>=run)) >>= \r'.return (r' >>= \x.run(s x)))
----------------------------------------------------------------------}
