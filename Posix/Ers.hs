-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Ers
    ( Transformer -- Transform a state
    , Observer    -- Observe a feature of a state
    , Transerver  -- Transform a state and observe a feature
    , Updator     -- Update a state
    ) where

----------------------------------------------------------------------
--  Transformations, observations, updates
----------------------------------------------------------------------

-- A single step in the state trajectory
type Transformer s  = s -> s
-- Get some property (or feature) of the state
type Observer s a   = s -> a 
-- Combine a step with an observation
type Transerver s a = s -> (s, a)
-- Update a state with a feature
type Updator s a    = a -> s -> s
-- Run a monadic state transformer
