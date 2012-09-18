-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module GraphConstraint ( validateGraphTransitiveConstraint ) where

----------------------------------------------------------------------
-- Validate constraints between a node in a graph, and its
-- subnodes, be they simple or recursive.
----------------------------------------------------------------------

-- Graph imports
import qualified Deep as D
import qualified NodeName as N

-- Validate a transitive constraint. The algorithm depends upon the
-- transitive property of the input constraint.
validateGraphTransitiveConstraint :: (N.NodeName nn, Ord c) =>
    c                    -> -- High level value
    (D.Node b nn a -> c) -> -- Convert node to constraint value
    (c -> c -> Bool)     -> -- Constraint
    D.Graph b nn a       -> -- Graph to validate
    Bool                    -- Overall result on the entire graph
-- All constraints are considered to be true on the empty graph
validateGraphTransitiveConstraint
  _highLevel _nodeConvert _constraint D.Empty = True
validateGraphTransitiveConstraint
    highLevel
    nodeConvert
    constraint
    (D.Graph g (D.Context _succs node _preds)) =
  let res1 = validateGraphTransitiveConstraint
               highLevel   -- High level value
               nodeConvert -- Convert node to constraint value
               constraint  -- Constraint
               g           -- Graph to validate
  in res1 &&
     if D.isEmpty (D.subGraph node)
     then constraint highLevel (nodeConvert node)
     else validateGraphTransitiveConstraint
            (nodeConvert node) -- High level value
            nodeConvert        -- Convert node to constraint value
            constraint         -- Constraint function
            (D.subGraph node)  -- Subgraph to validate
