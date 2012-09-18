-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Ex
    ( E.Exception (..)  -- Exceptions
    , E (..)            -- Exception monad
    , raise             -- Raise an exception
    , catch             -- Catch a possible exception
    , unOk              -- Project out non exception values
    , module General    -- Export properties
    ) where

----------------------------------------------------------------------
--  The execption monad
--  The monad laws:
--    return x >>= f = f x
--    m >>= return = m
--    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--
--  The exception monad is a base monad, no need for a monad
--  transformer to build it up.
--
--  This file is called "Ex" rather than "Exception" in order not
--  to interfere with the Haskell IO "Exception" file.
--
----------------------------------------------------------------------

-- Local imports
import MonadRec
import qualified Except as E
-- Haskell imports
import Prelude hiding (catch)
import General -- P-Logic
-- Utility imports
import Null

data E a
    = Ok a
    | Ex E.Exception deriving (Show)

-- Project out non exception values
unOk :: E a -> a
unOk (Ex _) = error "unOk"
unOk (Ok x) = x

{-P: {- Properties of the exception monad -}
assert UnOk =
    { unOk . Ok } === { id }
 -}

-- The exception monad is well known to be a monad
instance Monad E where
    return = Ok
    Ok v >>= f = f v
    Ex e >>= _f = Ex e

-- An instance of Null for E
instance (Null a) => Null (E a) where
    mkNull = Ok mkNull

-- Raise an exception
raise :: E.Exception -> E a
raise = Ex

-- Provide an exception handler to a monad action
catch :: E a -> (E.Exception -> E a) -> E a
catch (Ok x) _h = Ok x
catch (Ex e) h = h e

{-P: {- Properties of exceptions -}
assert Catch =
    All e :: E.Exception.
    All h :: E.Exception -> E a.
    { catch (raise e) h } === { h e }
 -}

-- Finally, the mfix operator (supporting recursive monadic bindings)
-- for the state monad (See "Recursive Monadic Bindings" by Erkok
-- and Launchbury)
instance MonadRec E where
    mfix f = fix (f. unOk)

{-
----------------------------------------------------------------------
--  Proof of the execption monad laws
----------------------------------------------------------------------
--  First law: return x >>= f = f x
LHS = return x >>= f
= Ok x >>= f
= f x
= RHS
-- This completes the proof of the first monad law

--  Second law:  m >>= return = m
LHS = m >>= return
Case 1: m = Ok x
LHS = Ok x >>= return
= return x
= Ok x
-- So LHS = RHS in case 1
Case 2: m = Ex e
LHS = Ex e >>= return
= Ex e
-- So LHS = RHS in case 2
-- This completes the proof of the second monad law

-- Third law: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
LHS = (m >>= f) >>= g
Case 1: m = Ok x
LHS = (OK x >>= f) >>= g
= f x >>= g
RHS = OK x >>= (\x -> f x >>= g)
= f x >> = g
-- So LHS = RHS in case 1
Case 2: m = Ex e
LHS = (Ex e >>= f) >>= g
= Ex e >>= g
= Ex e
RHS = Ex e >>= (\x -> f x >>= g)
= Ex e
-- So LHS = RHS in case 2
-- This completes the proof of the third monad law
-- This completes the proof of the monad laws
----------------------------------------------------------------------}
