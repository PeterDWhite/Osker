-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Deepen
    ( deepen      -- Deepen a grapu
    , deepenEdges -- Exported for debugging
    ) where

----------------------------------------------------------------------
-- Deepen a flat graph. The recursive information is stored as node
-- names in the flat graph.
----------------------------------------------------------------------

-- Haskell imports
import List
-- Graph imports
import qualified Flat as Flat
import qualified Deep as Deep
import qualified NodeName as N

-- Deepen a deep graph
deepen :: (N.NodeName nn, Show nn, Show a) =>
          Flat.Graph b nn a -> Deep.Graph b nn a
deepen Flat.Empty = Deep.Empty
deepen g =
  let nodes = Flat.listNodes g
      -- Sort the nodes in increasing length, to avoid conflicts
      -- in the Deep.insNode. For example, inserting [5] after [5,1]
      -- results in a conflict, since the insert of [5,1] results in
      -- a [5] already.
      -- First define the function to compare two nodes, based on the
      -- length (number of components) in the node name.
      increasingLength :: (Flat.NodeName nn) =>
          Flat.Node nn a -> Flat.Node nn a -> Ordering
      increasingLength n1 n2 =
        if Flat.nnLength (Flat.nodeName n1) <
           Flat.nnLength (Flat.nodeName n2)
        then LT
        else if Flat.nnLength (Flat.nodeName n1) ==
                Flat.nnLength (Flat.nodeName n2)
             then EQ
             else GT
      -- Now use the comparison function is a sort of the nodes
      sortedNodes = sortBy increasingLength nodes
      -- get the node names from the list of nodes
      nodeNames = map deepenNode sortedNodes
      -- Make a graph with nodes only
      nodeGraph = Deep.insNodes nodeNames Deep.Empty
      -- Now get the edges
      (preds, succs) = Flat.listEdges g
      deepPreds = deepenEdges preds
      deepSuccs = deepenEdges succs
      -- And add them to the deep graph
      edgeGraph  = Deep.insEdges Deep.Sink deepPreds nodeGraph
      edgeGraph' = Deep.insEdges Deep.Source deepSuccs edgeGraph
  in edgeGraph'

-- Deepen a flat edge, but in a globalized form (no up or down links)
deepenEdge :: (N.NodeName nn) => Flat.Edge b nn -> Deep.Edge b nn
deepenEdge e = Deep.Edge { Deep.eSource   = Flat.eSrc e
                         , Deep.eUplink   = N.nnNull
                         , Deep.eDownlink = N.nnNull
                         , Deep.eSink     = Flat.eSnk e
                         , Deep.eFreight  = Flat.eFreight e
                         , Deep.eLabel    = Flat.eLabel e
                         }

-- Deepen a bunch of flat edges
deepenEdges :: (N.NodeName nn) => [Flat.Edge b nn] -> [Deep.Edge b nn]
deepenEdges edges = map deepenEdge edges

-- Deepen a node
deepenNode :: (N.NodeName nn) => Flat.Node nn a -> Deep.Node b nn a
deepenNode (Flat.Node nn a label) =
  Deep.Node { Deep.nodeName    = nn
            , Deep.subGraph    = Deep.Empty
            , Deep.nodeFreight = a
            , Deep.nodeLabel   = label
            }
