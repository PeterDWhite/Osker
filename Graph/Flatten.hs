-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Flatten ( flatten ) where

----------------------------------------------------------------------
-- Flatten a graph into a single level graph, but with the
-- recursive information stored up in the node names and edge names
----------------------------------------------------------------------

import qualified Flat as Flat
import qualified Deep as Deep
import qualified NodeName as N

-- Flatten a deep graph
flatten :: (N.NodeName nn) => Deep.Graph b nn a -> Flat.Graph b nn a
flatten Deep.Empty = Flat.Empty
flatten g =
  let -- Get the nodes of g. The list is generated such that the
      -- name of a node appears before the names of its subnodes.
      flatNodes = map flattenNode (Deep.listNodeNames N.nnNull g)
      -- Now insert the nodes into a flat graph
      nodeGraph = Flat.insNodes flatNodes Flat.Empty
      -- Now get the edges
      (preds, succs) = Deep.listEdges N.nnNull g
      flatPreds  = flattenEdges preds
      flatSuccs  = flattenEdges succs
      edgeGraph  = Flat.insEdges Flat.Sink flatPreds nodeGraph
      edgeGraph' = Flat.insEdges Flat.Source flatSuccs edgeGraph
  in edgeGraph'

-- Flatten a deep edge, which is already in global form, i.e. no
-- up link and no down link
flattenEdge :: (N.NodeName nn) => Deep.Edge b nn -> Flat.Edge b nn
flattenEdge e = Flat.Edge
                { Flat.eSnk     = Deep.eSink e
                , Flat.eSrc     = Deep.eSource e
                , Flat.eFreight = Deep.eFreight e
                , Flat.eLabel   = Deep.eLabel e
                }

-- Flatten a bunch of edges
flattenEdges :: (N.NodeName nn) => [Deep.Edge b nn] -> [Flat.Edge b nn]
flattenEdges edges = map flattenEdge edges

-- Flatten a deep node, having an empty subgraph
flattenNode :: (N.NodeName nn) => Deep.Node b nn a -> Flat.Node nn a
flattenNode n =
  if Deep.isEmpty (Deep.subGraph n)
  then Flat.Node { Flat.nodeName  = Deep.nodeName n
                 , Flat.nodeLabel = Deep.nodeLabel n
                 , Flat.nodeFreight = Deep.nodeFreight n
                 }
  else error "flattenNode"
