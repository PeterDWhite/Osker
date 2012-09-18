-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module GraphStatistics ( validateGraphStatisticalConstraint ) where

----------------------------------------------------------------------
-- Compute graph statistics, check graph constraints,
-- based on the node labels
----------------------------------------------------------------------

-- Utility imports
--import qualified Unsafe as U
-- Graph imports
import qualified Deep as D
import qualified NodeName as NN
import qualified Statistics as S

-- Validate a statistical constraint. This requires accumulating
-- statistics in the subgraphs, but the accumulated statistics are
-- not propagated up, they do not change. There is another function
-- for that.
validateGraphStatisticalConstraint ::
    (NN.NodeName nn, S.Statistics s, Show s) =>
    D.Graph b nn a   -> -- Graph to validate
    (s -> s -> s)    -> -- Accumulator for statistic
    (s -> s -> Bool) -> -- Validation function
    (a -> Maybe s)   -> -- Conversion of node data to statistic
    s                -> -- Initial value of statistic
    Maybe s             -- Nothing when validation fails
                        -- Overall stat when validation passes
validateGraphStatisticalConstraint
    D.Empty _accum _valid _convert _initial = Just S.initialStatistics
validateGraphStatisticalConstraint
    (D.Graph g (D.Context _succs node _preds)) accum valid convert initial =
  let ms1 = validateGraphStatisticalConstraint
              g       -- Subgraph to validate
              accum   -- accumulator for statistics
              valid   -- validation function
              convert -- convert node data to stats
              initial -- initial value of stats
  in case ms1 of
       Nothing -> Nothing
       Just s1 ->
         if D.isEmpty (D.subGraph node)
         then let ms2 = convert (D.nodeFreight node)
              in case ms2 of
                   Nothing ->
                     error "validateGraphStatisticalConstraint: Leaf node with no statistics"
                   Just s2 ->
                     let s3 = accum s1 s2
                     in if valid s3 initial
                        then Just s3
                        else Nothing
         else let ms2 = convert (D.nodeFreight node)
                  sub = D.subGraph node
              in case ms2 of
                   Nothing ->
                      -- When there are not stats at this level,
                      -- just pass the stats down to the next lower
                      -- level
                      validateGraphStatisticalConstraint
                         sub     -- Subgraph to validate
                         accum   -- accumulator for statistics
                         valid   -- validation function
                         convert -- convert node data to stats
                         initial -- initial value of stats
                   Just s2 ->
                     let s3 = accum s1 s2
                         ms4 = validateGraphStatisticalConstraint
                                 sub     -- Subgraph to validate
                                 accum   -- accumulator for statistics
                                 valid   -- validation function
                                 convert -- convert node data to stats
                                 s2      -- initial value of stats
                     in case ms4 of
                          Nothing -> Nothing
                          Just _s4 -> Just s3

{-
-- Accumulate the statistics on a graph, and reset the statistics
-- at each node to the accumulated statistics.
accumulateGraph :: (NN.NodeName nn, S.Statistics s) =>
    D.Graph b nn a ->   -- Graph to accumulate
    (a -> s -> s)  ->   -- Accumulator for statistic
    (a -> s -> a)  ->   -- Convert node with new stats
    (D.Graph b nn a, s) -- Graph with accumlated statistics
accumulateGraph D.Empty _accum _convert = (D.Empty, S.initialStatistics)
accumulateGraph (D.Graph h (D.Context succs node preds)) accum convert =
  if D.isEmpty (D.subGraph node)
  then let (accumh, hs) = accumulateGraph h accum convert
       in (D.Graph accumh (D.Context succs node preds),
           (accum (D.nodeFreight node) hs))
  else let (accumh, hs) = accumulateGraph h accum convert
           (accumsub, subs) = accumulateGraph (D.subGraph node) accum convert
           newfreight = convert (D.nodeFreight node) subs
           accumnode = D.Node { D.nodeName    = D.nodeName node
                              , D.subGraph    = accumsub
                              , D.nodeFreight = newfreight
                              , D.nodeLabel   = D.nodeLabel node
                              }
       in ((D.Graph accumh (D.Context succs accumnode preds)),
           accum newfreight hs)
 -}
