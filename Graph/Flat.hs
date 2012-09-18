-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Flat
    ( -- Constructors
      Graph (..)             -- Data type of flat graphs
    , Context (..)           -- Context (node and edges)
    , Node (..)              -- A node in the graph
    , Edge (..)              -- An edge in the graph
    , EdgeAnchor (..)        -- Insert at source or sink
    , insEdge                -- Insert an edge in the graph
    , insEdges               -- Insert several edges in the graph
    , insNode                -- Insert a node in the graph
    , insNodes               -- Insert several nodes in the graph
    , updateFreight          -- Update the label of a node
    , updateName             -- Update the name of a node
    , merge                  -- Mergo two flat graphs
    -- Analyzers
    , isEmpty                -- Determine if graph is empty
    , contextEdges           -- Get preds and succs from a context
    , listNodeNames          -- List the node names in a graph
    , listNodes              -- List the nodes in a graph
    , listEdges              -- List the edges in a graph
    -- Searches
    , isEdge                 -- Determine if an edge is in a graph
    , isEdges                -- Determine if several edges are in graph
    , findEdge               -- Find an edge in a graph
    , findNode               -- Find a node in a graph
    -- Nodes
    , N.NodeName (..)        -- Re-export node name
    -- Destructors
    , delNode                -- Delete a node from a graph
    , delEdge                -- Deleta an edge from a graph
    -- For debugging
    , modifyContext
    , delPred
    )
    where

-- Haskell imports
import List
-- Graph imports
import qualified NodeName as N

----------------------------------------------------------------------
-- Flat graphs, i.e. having no subgraphs.
----------------------------------------------------------------------

data (N.NodeName nn) => Node nn a =
    Node { nodeName     :: nn     -- Id of the node
         , nodeFreight  :: a      -- Data carried by the node
         , nodeLabel    :: String -- Used in DaVinci
         } deriving (Eq)

-- Update the name of a node
updateName :: (N.NodeName nn) => Node nn a -> nn -> Node nn a
updateName n nn = n { nodeName = nn }

-- Update the label of a node
updateFreight :: (N.NodeName nn) => Node nn a -> a -> Node nn a
updateFreight n a = n { nodeFreight = a }

instance (N.NodeName nn, Show a, Show nn) => Show (Node nn a) where
    show node = show (nodeName node)

----------------------------------------------------------------------
-- Flat Edges
----------------------------------------------------------------------
data (N.NodeName nn) => Edge b nn
    = Edge { eSrc     :: nn     -- The source of the edge
           , eSnk     :: nn     -- The sink of the edge
           , eFreight :: b      -- The label of the edge
           , eLabel   :: String -- Used in DaVinci
           }

-- Two edges are considered equal when the source and sink are the
-- same. The labels are ignored.
instance (N.NodeName nn) => Eq (Edge b nn) where
    re1 == re2 = eSrc re1 == eSrc re2 && eSnk re1 == eSnk re2

instance (N.NodeName nn, Show nn, Show b) => Show (Edge b nn) where
    show e = "<" ++ show (eSrc e) ++ "," ++ show (eSnk e) ++
             ", " ++ show (eLabel e) ++ ">"

----------------------------------------------------------------------
-- A has a list of predecessor edges, a list of successor edges
-- and a node (added only once)
----------------------------------------------------------------------
data (N.NodeName nn) => Context b nn a =
    Context { cPreds  :: [Edge b nn] -- The edges TO the node
            , cNode   :: Node nn a   -- The node itself
            , cSuccs  :: [Edge b nn] -- The edges FROM the node
            } deriving (Eq)

instance (N.NodeName nn, Show a, Show b, Show nn) =>
         Show (Context b nn a) where
    show (Context preds node succs) =
      if null preds
      then if null succs
           then "(-|" ++ show node ++ "|-)"
           else "(-|" ++ show node ++ "|-)...S..." ++ show succs
      else if null succs
           then "(-|" ++ show node ++ "|-)...P..." ++ show preds
           else "(-|" ++ show node ++ "|-)...P..." ++ show preds ++
                "...S..." ++ show succs

-- Get the list of edges in the context
contextEdges :: (N.NodeName nn) => Context b nn a -> ([Edge b nn], [Edge b nn])
contextEdges (Context preds _node succs) = (preds, succs)

-- Get the node name out of the context
contextName :: (N.NodeName nn) => Context b nn a -> nn
contextName = nodeName . cNode

-- Add a successor edge to a context
addSucc :: (N.NodeName nn) => Edge b nn -> Context b nn a -> Context b nn a
addSucc e c = c { cSuccs = e:(cSuccs c) }

-- Add a predecessor edge to a context
addPred :: (N.NodeName nn) => Edge b nn -> Context b nn a -> Context b nn a
addPred e c = c { cPreds = e:(cPreds c) }

-- Delete a successor from a context
delSucc :: (N.NodeName nn) => nn -> Context b nn a -> Context b nn a
delSucc src c = c { cSuccs = filter (\e -> eSrc e /= src) (cSuccs c) }

  -- Delete a predecessor from a context
delPred :: (N.NodeName nn) => nn -> Context b nn a -> Context b nn a
delPred snk c = c { cPreds = filter (\e -> eSnk e /= snk) (cPreds c) }
  
-- Specify if an edge will be anchored at source or sink
data EdgeAnchor = Source | Sink deriving (Eq, Show)

----------------------------------------------------------------------
-- Finally, graph data type
----------------------------------------------------------------------
data (N.NodeName nn) => Graph b nn a
    = Empty
    | Graph (Graph b nn a) (Context b nn a) -- The embed operation
    deriving (Show, Eq)

-- Determine if a graph is the empty graph
isEmpty :: Graph b nn a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- A single step decomposition of a graph
data Decomp b nn a =
    Decomp { dmContext :: Maybe (Context b nn a)
           , dGraph    :: Graph b nn a
           }

----------------------------------------------------------------------
-- Operators on graphs, stolen and modified from Martin Erwig
----------------------------------------------------------------------
-- Insert a node in its proper place in the hierarchy
insNode :: (N.NodeName nn) => Node nn a -> Graph b nn a -> Graph b nn a
insNode node g = Graph g (Context [] node [])

-- Convert insNode into a function on list of nodes
insNodes :: (N.NodeName nn) => [Node nn a] -> Graph b nn a -> Graph b nn a
insNodes nodes g = foldr insNode g nodes

-- Delete a node from a graph.
delNode :: (N.NodeName nn, Show nn) => nn -> Graph b nn a -> Graph b nn a
delNode nodename graph = dGraph (match nodename graph)

-- Insert edges in the recursive graph
insEdge :: (N.NodeName nn) =>
    EdgeAnchor ->   -- Anchor at source or at sink
    Edge b nn ->    -- Edge to insert
    Graph b nn a -> -- Graph in which to insert
    Graph b nn a    -- Resulting graph
insEdge _ _e Empty = error ("insEdge.Empty.1\n")
insEdge Source e g = modifyContext (eSrc e) g (addSucc e)
insEdge Sink e g   = modifyContext (eSnk e) g (addPred e)

insEdges :: (N.NodeName nn) => EdgeAnchor -> [Edge b nn] -> Graph b nn a -> Graph b nn a
insEdges anchor edges g = foldr (insEdge anchor) g edges

-- Delete an edge from the graph
delEdge :: (N.NodeName nn) => nn -> nn -> Graph b nn a -> Graph b nn a
delEdge src snk g =
  if isEdge g src snk
  then let g' = modifyContext src g (delSucc src)
       in modifyContext snk g' (delPred snk)
  else g

----------------------------------------------------------------------
-- Graph matching, and other decomposition operators
----------------------------------------------------------------------
-- Decompose a graph, taking out the context that introcudes the node
-- with the specified name.
match :: (N.NodeName nn, Show nn) => nn -> Graph b nn a -> Decomp b nn a
match _nn Empty = Decomp { dmContext = Nothing, dGraph = Empty }
match nn (Graph g c) = 
  if contextName c == nn
  then match nn g
  else let Context preds node succs = c
           preds' = filter (\p -> eSrc p /= nn) preds
           succs' = filter (\s -> eSnk s /= nn) succs
           decomp = match nn g
       in case dmContext decomp of
            Nothing -> Decomp
                       { dmContext = Just c
                       , dGraph    = Graph
                                       (dGraph decomp)
                                       (Context preds' node succs')
                       }
            Just _context ->
              error ("Node name " ++ show nn ++ " matched twice")

----------------------------------------------------------------------
-- Analyzers for recursive graphs
----------------------------------------------------------------------

-- Determine if an edge is in a graph
isEdge :: (N.NodeName nn) => Graph b nn a -> nn -> nn -> Bool
isEdge g source sink =
  let medge = findEdge g source sink
  in case medge of
       Nothing ->    False
       Just _edge -> True

-- Determine if a list of edges is in a recursive graph
isEdges :: (N.NodeName nn) => Graph b nn a -> [(nn, nn)] -> Bool
isEdges _g [] = True
isEdges g (e:es) =
  let (source, sink) = e
  in isEdge g source sink && isEdges g es

-- Find an edge in the graph
findEdge :: (N.NodeName nn) => Graph b nn a -> nn -> nn -> Maybe (Edge b nn)
findEdge g source sink =
    let -- See if the source node is in the graph
        msourceContext = findContext source g
        -- See if the sink node is in the graph
        msinkContext   = findContext sink g
        -- Here is what the edge will look like, should it be found
        edge = Edge { eSrc     = source
                    , eSnk     = sink
                    , eFreight = error "Don't care for equality"
                    , eLabel   = ""
                    }
    in case msourceContext of
         Nothing -> Nothing
	 Just sourceContext ->
           case msinkContext of
             Nothing -> Nothing
             Just sinkContext ->
               let misPred = find (== edge) (cPreds sinkContext)
               in case misPred of
                    Nothing ->
                      find (== edge) (cSuccs sourceContext)
                    Just isPred -> Just isPred
 
-- Find the context that introduces a node into the recursive graph
findContext :: (N.NodeName nn) => nn -> Graph b nn a -> Maybe (Context b nn a)
findContext _nn Empty = Nothing
findContext nn (Graph g c) =
  if contextName c == nn
  then Just c
  else findContext nn g

-- Modify a context in place
modifyContext :: (N.NodeName nn) =>
    nn -> Graph b nn a -> (Context b nn a -> Context b nn a) -> Graph b nn a
modifyContext _nn Empty _f = Empty
modifyContext nn (Graph g c) f =
  if contextName c == nn
  then Graph g (f c)
  else Graph (modifyContext nn g f) c

-- Find a node in a recursive graph
findNode :: (N.NodeName nn) => nn -> Graph b nn a -> Maybe (Node nn a)
findNode nn g =
  let mrc = findContext nn g
  in case mrc of
       Nothing -> Nothing
       Just rc -> Just (cNode rc)

-- List the node names in a graph
listNodeNames :: (N.NodeName nn) => Graph b nn a -> [nn]
listNodeNames Empty = []
listNodeNames (Graph g c) = listNodeNames g ++ [contextName c]

-- List the nodes in a graph
listNodes :: (N.NodeName nn) => Graph b nn a -> [Node nn a]
listNodes Empty = []
listNodes (Graph g c) = listNodes g ++ [cNode c]

-- List the edges in a graph, predecessors and then successors
listEdges :: (N.NodeName nn) => Graph b nn a -> ([Edge b nn], [Edge b nn])
listEdges Empty = ([], [])
listEdges (Graph g c) =
  let (ps, ss) = contextEdges c
      (ps', ss') = listEdges g
  in (ps ++ ps', ss ++ ss')

-- Merge two flat graphs
merge :: (N.NodeName nn) => Graph b nn a -> Graph b nn a -> Graph b nn a
merge g1 g2 =
  case g2 of
    Empty -> g1
    Graph g2' c -> Graph (merge g1 g2') c
