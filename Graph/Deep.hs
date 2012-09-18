-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Deep
    ( -- Constructors
      Graph (..)             -- Data type of recursive graphs
    , StructuralGraph        -- When only structure is of interest
    , Context (..)           -- Context (node and edges)
    , Node (..)              -- A node in the graph
    , Edge (..)              -- An edge in the graph
    , EdgeAnchor (..)        -- Insert at source or sink
    , insEdge                -- Insert an edge in the graph
    , insEdges               -- Insert several edges in the graph
    , insNode                -- Insert a node in the graph
    , insNodes               -- Insert several nodes in the graph
    , insNodeNoConflict      -- Insert a node, do nothing if name conflict
    -- Analyzers
    , listNodes              -- List the nodes in a graph
    , listImmediateNodes     -- List immediate subnodes of a graph
    , listNodeNames          -- List the node names, complete form
    , listEdges              -- List the edges in a graph
    , listContexts           -- List the contexts in a graph
    , isRecursive            -- Determine if a node is recursive
    , isEmpty                -- Determine if a graph is empty
    , depth                  -- Determine the depth of a graph
    , stratify               -- Graph to list of top level contexts
    , contextName            -- Name of node in context
    , contextFreight         -- Freight of node in context
    -- Searches
    , isEdge                 -- Determine if an edge is in a graph
    , isEdges                -- Determine if several edges are in graph
    , findEdge               -- Find an edge in a graph
    , findNode               -- Find a node in a graph
    , isNode                 -- Determine if a node is in a graph
    , resolveNodeName        -- Find a node from a single node name component
    , resolveAndFindNodeName -- Find a name and anode find last component
    , isDupName              -- Determine if a name is duplicated
    , isDupNames             -- Determine if names are duplicated
    , getNthSubNode          -- Get the nth subnode of a graph
    , directlyConnected      -- See if two nodes are directly connected
    , communicates           -- See if two nodes are indirectly connected
    -- Nodes
    , N.NodeName (..)        -- Re-export
    -- Updates
    , updateNodeFreight      -- Update the label of a node in the graph
    , updateNodeFreightAndName -- Update freight and name of a node
    , updateNodeSubgraph     -- Update the subgraph of a node
    , updateFreight          -- Apply update function to label of a node
    , changeFreight          -- Change the label of a node
    , changeName             -- Change the name of a node
    , updateLeafNodes        -- Apply update function to the leaf nodes
    -- Destructors
    , delNode                -- Delete a node from a graph
    , match                  -- Export for debugging
    -- Print outs
    , outNode                -- Format a node a a string
    -- Maps and folds
    , foldlG                 -- Fold left over a graph (nodes only)
    , foldrG                 -- Fold right over a graph (nodes only)
    , mapG                   -- Map over a graph (nodes and edges)
    )
    where

-- Haskell imports
import List
import Maybe
-- Recursive graph imports
import qualified NodeName as N

-- An output utility, tabs at 3
indent :: Int -> String
indent n = replicate (n * 3) ' '

----------------------------------------------------------------------
-- Recursive (deep) graph, defined inductively
----------------------------------------------------------------------

-- The node of the graph
data (N.NodeName nn) => Node b nn a
     = Node { nodeName    :: nn           -- The pathname of the node
            , subGraph    :: Graph b nn a -- A possible subgraph
            , nodeFreight :: a            -- The freight of the node
            , nodeLabel   :: String       -- Used in DaVinci
            }

instance (N.NodeName nn, Eq a) => Eq (Node b nn a) where
    n1 == n2 = nodeName n1 == nodeName n2 && nodeFreight n1 == nodeFreight n2

instance (N.NodeName nn, Eq a) => Ord (Node b nn a) where
    n1 <= n2 = nodeName n1 <= nodeName n2

-- Update the name of a node
updateName :: (N.NodeName nn) => Node b nn a -> (nn -> nn) -> Node b nn a
updateName node f = node { nodeName = f (nodeName node) }

-- Change the name of a node
changeName :: (N.NodeName nn) => Node b nn a -> nn -> Node b nn a
changeName node nn = node { nodeName = nn }

-- Apply update function to the label of a node
updateFreight :: (N.NodeName nn) => Node b nn a -> (a -> a) -> Node b nn a
updateFreight node f = node { nodeFreight = f (nodeFreight node) }

-- Change the label of a node
changeFreight :: (N.NodeName nn) => Node b nn a -> a -> Node b nn a
changeFreight node a = node { nodeFreight = a }

-- Update the graph of a node
updateNodeGraph :: (N.NodeName nn) =>
    Node b nn a -> (Graph b nn a -> Graph b nn a) -> Node b nn a
updateNodeGraph node f = node { subGraph = f (subGraph node) }

-- Change the graph of a node
changeNodeGraph :: (N.NodeName nn) =>
    Node b nn a -> Graph b nn a -> Node b nn a
changeNodeGraph node g = node { subGraph = g }

-- globalize a node, by adding the path name to it to the node name
-- The subgraph is also made empty.
globalizeNode :: (N.NodeName nn) => nn -> Node b nn a -> Node b nn a
globalizeNode path n = Node { nodeName    = path `N.nnConcat` (nodeName n)
                            , subGraph    = Empty
                            , nodeFreight = nodeFreight n
                            , nodeLabel   = nodeLabel n
                            }

-- Format a node for printing, with indentation
outNode :: (N.NodeName nn, Show nn, Show a) => Int -> Node b nn a -> String
outNode n (Node nn g a _label) =
  indent n ++ show nn ++ "//" ++ show a ++ indent n ++ "//" ++ show g

-- Determine if a node is simple
isSimple :: (N.NodeName nn) => Node b nn a -> Bool
isSimple = isEmpty . subGraph

-- Determine if a node is recursive
isRecursive :: (N.NodeName nn) => Node b nn a -> Bool
isRecursive = not . isSimple

instance (N.NodeName nn, Show a, Show nn) => Show (Node b nn a) where
    show node = show (nodeName node)

----------------------------------------------------------------------
-- If the edge is a predecessor, the sink of the edge will be equal
-- to the nodename of the context in which the edge is found.
-- If the edge is a successor, the source of the edge will be equal
-- to the nodename of the context in which the edge is found.
----------------------------------------------------------------------
data (N.NodeName nn) => Edge b nn
    = Edge { eSource    :: nn     -- The source of the edge
           , eUplink    :: nn     -- The up path
           , eDownlink  :: nn     -- The down path
           , eSink      :: nn     -- The sink of the edge
           , eFreight   :: b      -- The label of the edge
           , eLabel     :: String -- Used in DaVinci
           } deriving (Eq, Ord)

-- localize an edge that is specified in terms of global source and sink
localizeEdge :: (N.NodeName nn) => Edge b nn -> Edge b nn
localizeEdge e =
  let -- Get the length of the common prefix of source and sink nodes
      lcp = N.nnLength (N.nnCommonPrefix (eSource e) (eSink e))
  in Edge { eSource   = N.nnLast (N.nnDrop lcp (eSource e))
          , eUplink   = N.nnInit (N.nnDrop lcp (eSource e))
          , eDownlink = N.nnInit (N.nnDrop lcp (eSink e))
          , eSink     = N.nnLast (N.nnDrop lcp (eSink e))
          , eFreight  = eFreight e
          , eLabel    = eLabel e
          }

-- Globalize an edge, by adding the path in front of the source
-- and sink, and making the edge global, having only source and
-- sink, no up and down links.
globalizeEdge :: (N.NodeName nn) => nn -> Edge b nn -> Edge b nn
globalizeEdge path e =
  let maxvertical = max (N.nnLength (eUplink e)) (N.nnLength (eDownlink e))
      prefix = N.nnTake (N.nnLength path - maxvertical) path
  in Edge { eSource   = prefix `N.nnConcat` (eUplink e) `N.nnConcat`
                        (eSource e)
          , eUplink   = N.nnNull
          , eDownlink = N.nnNull
          , eSink     = prefix `N.nnConcat` (eDownlink e) `N.nnConcat`
                        (eSink e)
          , eFreight  = eFreight e
          , eLabel    = eLabel e
          }

-- Check if two edges have the same source and destination
samePath :: (N.NodeName nn) => Edge b nn -> Edge b nn -> Bool
samePath e1 e2 = eSource e1   == eSource e2   &&
                 eUplink e1   == eUplink e2   &&
                 eDownlink e1 == eDownlink e2 &&
                 eSink e1     == eSink e2

instance (N.NodeName nn, Show nn) => Show (Edge b nn) where
    show (Edge source up down sink _freight _label) =
      "<" ++ show source ++ "," ++ show up ++ "," ++ show down ++
      "," ++ show sink ++ ">"

----------------------------------------------------------------------
-- A recursive (deep) context has a list of predecessor edges (added
-- one at a time), a list of successor edges (added one at a time),
-- and a node (added only once)
----------------------------------------------------------------------
data (N.NodeName nn) => Context b nn a =
    Context { cPreds  :: [Edge b nn]  -- The edges TO the node
            , cNode   :: Node b nn a  -- The node itself
            , cSuccs  :: [Edge b nn]  -- The edges FROM the node
            }

-- Get the node name out of the context
contextName :: (N.NodeName nn) => Context b nn a -> nn
contextName = nodeName . cNode

-- Get the label out of the context
contextFreight :: (N.NodeName nn) => Context b nn a -> a
contextFreight = nodeFreight . cNode

-- Get the graph out of the context
contextGraph :: (N.NodeName nn) => Context b nn a -> Graph b nn a
contextGraph = subGraph . cNode

-- Get the list of edges in the context, predecessors and then
-- successors
contextEdges :: (N.NodeName nn) =>
    nn -> Context b nn a -> ([Edge b nn], [Edge b nn])
contextEdges path c = ( map (globalizeEdge path) (cPreds c),
                        map (globalizeEdge path) (cSuccs c) )

-- Update the label of the node of a context
updateContextLabel :: (N.NodeName nn) => a -> Context b nn a -> Context b nn a
updateContextLabel a c = c { cNode = updateFreight (cNode c) (\_ -> a) }

-- Update the label of the node of a context
updateContextName :: (N.NodeName nn) => nn -> Context b nn a -> Context b nn a
updateContextName nn c = c { cNode = updateName (cNode c) (\_ -> nn) }

-- Update the label of the node of a context
updateContextSubgraph :: (N.NodeName nn) =>
    Graph b nn a -> Context b nn a -> Context b nn a
updateContextSubgraph g c = c { cNode = changeNodeGraph (cNode c) g }

-- Update the label of the node of a context
updateContextLabelAndName :: (N.NodeName nn) =>
    a -> nn -> Context b nn a -> Context b nn a
updateContextLabelAndName a nn c =
   updateContextLabel a (updateContextName nn c)

-- Add a successor edge to a context
addSucc :: (N.NodeName nn) => Edge b nn -> Context b nn a -> Context b nn a
addSucc e c = c { cSuccs = (localizeEdge e):(cSuccs c) }

-- Add a predecessor edge to a context
addPred :: (N.NodeName nn) => Edge b nn -> Context b nn a -> Context b nn a
addPred e c = c { cPreds = (localizeEdge e):(cPreds c) }

-- Specify if an edge will be anchored at source or sink
data EdgeAnchor = Source | Sink deriving (Eq, Show)

instance (N.NodeName nn, Show a, Show nn) => Show (Context b nn a) where
    show (Context preds node succs) =
      if null preds
      then if null succs
           then "(-|" ++ show node ++ "|-)"
           else "(-|" ++ show node ++ "|-)...S..." ++ show succs
      else if null succs
           then "(-|" ++ show node ++ "|-)...P..." ++ show preds
           else "(-|" ++ show node ++ "|-)...P..." ++ show preds ++
                "...S..." ++ show succs

----------------------------------------------------------------------
-- Finally, the recursive graph data type
----------------------------------------------------------------------
data (N.NodeName nn) => Graph b nn a
    = Empty
    | Graph (Graph b nn a) (Context b nn a) -- The embed operation

-- When only structure is of interest
type StructuralGraph = Graph () () ()

-- Two graphs are equivalent when they have the same nodes and edges
equivGraph :: (N.NodeName nn, Ord b, Eq a) =>
    Graph b nn a -> Graph b nn a -> Bool
equivGraph g1 g2 =
  let g1nodes = map (globalizeNode N.nnNull) (listNodeNames N.nnNull g1)
      g2nodes = map (globalizeNode N.nnNull) (listNodeNames N.nnNull g2)
      (g1preds, g1succs) = listEdges N.nnNull g1
      (g2preds, g2succs) = listEdges N.nnNull g2
  in sort g1nodes == sort g2nodes &&
     sort g1preds == sort g2preds &&
     sort g1succs == sort g2succs

instance (Eq a, Eq b, N.NodeName nn, Ord b) => Eq (Graph b nn a) where
    g1 == g2 = equivGraph g1 g2

-- Determine if a graph is the empty graph
isEmpty :: (N.NodeName nn) => Graph b nn a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Determine the depth of a graph
-- nn is the node name
-- b is the edge freight
-- a is the node freight
depth :: (N.NodeName nn) => Graph b nn a -> Int
depth Empty = 0
depth (Graph g c) =
  if isEmpty (contextGraph c)
  then depth g
  else max (depth g) (depth (contextGraph c) + 1)

-- Print out a graph hierarchically, with indentation
showHierarchy :: (N.NodeName nn, Show a, Show nn) =>
    Int ->          -- Indentation
    Graph b nn a -> -- Graph to show
    String          -- Resulting output
showHierarchy _n Empty      = ""
showHierarchy n (Graph g c) =
  indent n ++ show c ++ "\n" ++
     ( if not (isEmpty (contextGraph c))
       then indent (n+1) ++ "*\n" ++ showHierarchy (n+1) (contextGraph c)
       else ""
     ) ++
     ( if isEmpty g
       then ""
       else "\n" ++ showHierarchy n g
     )

instance (N.NodeName nn, Show a, Show nn) => Show (Graph b nn a) where
    show Empty = "Empty"
    show g = "\n" ++ showHierarchy 0 g

-- A single step decomposition of a graph
data Decomp b nn a =
    Decomp
    { dmContext :: Maybe (Context b nn a)
    , dGraph    :: Graph b nn a
    , dEdges    :: [Edge b nn]
    } deriving (Show)

----------------------------------------------------------------------
-- Operators on graphs, stolen and modified from Martin Erwig
----------------------------------------------------------------------
-- Insert a new node (complete with subgraph) into the deep graph
insNode :: (N.NodeName nn, Show nn, Show a) =>
           Node b nn a -> Graph b nn a -> Graph b nn a
-- Bottom of recursion, node name is null
insNode n g | N.isNnNull (nodeName n) = g
-- Insert a node name into the empty graph
insNode n Empty =
  let -- Begin by placing the first component of the node name
      start = Graph Empty (Context [] (updateName n (N.nnHead)) [])
  in if N.nnLength (nodeName n) == 1
     then -- If the first component is all there is, we are done
          start
     else -- If the component is longer, then the new node is somewhat
          -- underspecified, so we reuse the node label as we go down
          insNode n start
-- Insert a node name into a non-empty graph
insNode n (Graph g c) =
  if nodeName n == contextName c
  then error ("Deep.insNode.conflict: " ++ show (nodeName n) ++
              "\n" ++ show (Graph g c))
  else if N.nnIsPrefixOf (contextName c) (nodeName n)
       then -- The node name of the current context is a prefix of
            -- the node to be added. Add into the subgraph of the context
            let shorter = updateName n (N.nnDrop (N.nnLength (contextName c)))
                node    = cNode c
                -- Add the node to the subgraph
                node'   = updateNodeGraph node (insNode shorter)
                newc     = c { cNode = node' }
            in (Graph g newc)
       else -- The node name of the current node is not
            -- a prefix of the node name to be added. Search
            -- the graph component rather than the context component
            Graph (insNode n g) c

-- Convert insNode into a function on list of nodes
insNodes :: (N.NodeName nn, Show nn, Show a) =>
            [Node b nn a] -> Graph b nn a -> Graph b nn a
insNodes [] g = g
insNodes (n:ns) g =
  let g' = insNode n g
  in insNodes ns g'

-- Insert a node, doing nothing if there is already a node by
-- that name
insNodeNoConflict :: (N.NodeName nn, Show nn, Show a) =>
    Node b nn a -> Graph b nn a -> Graph b nn a
insNodeNoConflict n g =
  if isNode (nodeName n) g
  then g
  else insNode n g

-- Delete a node from a recursive graph. This includes deleting
-- any edges to or from the node that are kept in the context
-- of another node in the graph.
delNode :: (N.NodeName nn) => nn -> Graph b nn a -> Graph b nn a
delNode nn g =
  let decomp = match nn g
  in case dmContext decomp of
       Nothing -> g
       Just _context -> dGraph decomp

-- Insert to edges in the graph. The edge has to be in global form
insEdge :: (N.NodeName nn) =>
    EdgeAnchor ->   -- Anchor at source or at sink
    Edge b nn ->    -- Edge to insert
    Graph b nn a -> -- Graph into which edge is inserted
    Graph b nn a    -- Resulting graph
insEdge Source e g = modifyContext (eSource e) g (addSucc e)
insEdge Sink   e g = modifyContext (eSink e) g (addPred e)

insEdges :: (N.NodeName nn) =>
            EdgeAnchor -> [Edge b nn] -> Graph b nn a -> Graph b nn a
insEdges anchor edges g = foldr (insEdge anchor) g edges

----------------------------------------------------------------------
-- Graph matching, and other decomposition operators
----------------------------------------------------------------------
-- Identify a context (possibly deeply buried within the graph)
-- and return the graph with that context extracted. All edges to and
-- from nodes in that context are also extracted.
match :: (N.NodeName nn) =>
    nn           -> -- Node name to look for
    Graph b nn a -> -- Graph to search and modify
    Decomp b nn a   -- Resulting decomposition
match _nn Empty = Decomp { dmContext = Nothing
                         , dGraph = Empty
                         , dEdges = []
                         }
match nn (Graph g c) =
    if N.nnIsPrefixOf (contextName c) nn
    then -- The node name at the current node is a prefix of the
         -- node name we are looking for
         if (contextName c) == nn
         then -- We have found what we are looking for
              Decomp { dmContext = Just c
                     , dGraph = g
                     , dEdges = fst (contextEdges N.nnNull c) ++
                                snd (contextEdges N.nnNull c)
                     }
         else -- Need to look in the subgraph of the context
              let node = cNode c
                  -- Get the current subgraph
                  subgraph = subGraph node
                  -- Localize the name to the subgraph
                  shorter = N.nnDrop (N.nnLength (contextName c)) nn
                  -- Decompose the subgraph of the node
                  decomp = match shorter subgraph
                  -- Decompose the graph component. Since the match must
                  -- be in the subgraph of the node, the match cannot
                  -- be in the graph g. This decomposition serves to
                  -- remove edges to or from the node being deleted.
                  decomp' = match nn g
              in case dmContext decomp of
                   Nothing ->
                     -- Not found in subgraph, so not found anywhere
                     Decomp { dmContext = Nothing
                            , dGraph    = Graph g c
                            , dEdges    = []
                            }
                     -- Found in the subgraph, so must build new node
                   Just ctxt ->
                     let node' = node { subGraph = dGraph decomp }
                         c' = c { cNode = node' }
                     in Decomp { dmContext = Just ctxt
                               , dGraph    = Graph (dGraph decomp') c'
                               , dEdges    = dEdges decomp ++ dEdges decomp'
                               }
    else let -- Look in the context component. This reduces the context.
             -- There is no hope of a match in the context, since the
             -- context name is not a prefix of the name sought.
             decomp = matchContext nn c
             -- Now look in the graph component
             decomp' = match nn g
         in case dmContext decomp' of
              Nothing -> -- Not found anywhere
                Decomp { dmContext = Nothing
                       , dGraph    = Graph g c
                       , dEdges    = []
                       }
              Just ctxt -> -- Found in the graph component
                Decomp { dmContext = Just ctxt
                       , dGraph    = Graph (dGraph decomp') (fromJust (dmContext decomp))
                       , dEdges    = dEdges decomp ++ dEdges decomp'
                       }

-- Match the subgraph of a context
matchContext :: (N.NodeName nn) =>
    nn             -> -- Node name to look for
    Context b nn a -> -- Context to search and modify
    Decomp b nn a     -- Reduced context
matchContext nn (Context preds node succs) =
  let (predsin, predsnotin) = partition (matchEdge nn) preds
      (succsin, succsnotin) = partition (matchEdge nn) succs
      -- Continue the match in the subgraph, to continue filtering
      -- out the edges to the extracted node. This decomp should
      -- return a context of Nothing, since we already know the node
      -- we seek is not in this subgraph.
      decomp = match nn (subGraph node)
      node' = node { subGraph = dGraph decomp }
      c = Context { cPreds = predsin, cNode = node', cSuccs = succsin }
  in Decomp { dmContext = Just c,
              dGraph = dGraph decomp,
              dEdges = predsnotin ++ succsnotin
            }

-- Check if either the source or sink matches a node
matchEdge :: (N.NodeName nn) => nn -> Edge b nn -> Bool
matchEdge nn e =
  N.nnConcat (eUplink e) (eSource e) == nn ||
  N.nnConcat (eDownlink e) (eSink e) == nn

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

findEdge :: (N.NodeName nn) => Graph b nn a -> nn -> nn -> Maybe (Edge b nn)
findEdge g source sink =
    let -- See if the source node is in the graph
        msourceContext = findContext source g
        -- See if the sink node is in the graph
        msinkContext   = findContext sink g
        -- Get the common prefix of the source and sink nodes
        cp = N.nnCommonPrefix source sink
        -- The important fact is the length of the common prefix
        lcp = N.nnLength cp
        -- Drop the common prefix from both node names
        localSourceName = N.nnDrop lcp source
        localSinkName   = N.nnDrop lcp sink
        -- The node name is the local graph
        localSource     = N.nnLast localSourceName
        localSink       = N.nnLast localSinkName
        -- The path up through the recursive graph
        localUp         = N.nnInit localSourceName
        localDown       = N.nnInit localSinkName
        -- Here is what the edge will look like, should it be found
        edge = Edge
               { eSource    = localSource
               , eUplink    = localUp
               , eDownlink  = localDown
               , eSink      = localSink
               , eFreight   = error "Don't care for equality test"
               , eLabel     = ""
               }
    in case msourceContext of
         Nothing -> Nothing
	 Just sourceContext ->
           case msinkContext of
             Nothing -> Nothing
             Just sinkContext ->
               let misPred = find (samePath edge) (cPreds sinkContext)
               in case misPred of
                    Nothing ->
                      find (samePath edge) (cSuccs sourceContext)
                    Just isPred -> Just isPred
 
-- Find the context that introduces a node into the recursive graph
findContext :: (N.NodeName nn) => nn -> Graph b nn a -> Maybe (Context b nn a)
findContext _nn Empty = Nothing
findContext nn (Graph g c) =
  if N.nnIsPrefixOf (contextName c) nn
  then if contextName c == nn
       then Just c
       else let shorter = N.nnDrop (N.nnLength (contextName c)) nn
            in findContext shorter (contextGraph c)
  else findContext nn g

-- Resolve a node name, given only the last element of the node name.
-- This will find the first node matching the specification
resolveNodeName :: (N.NodeName nn) =>
    nn           -> -- Node name to look for
    Graph b nn a -> -- Graph to look in
    Maybe nn        -- Node name returned
resolveNodeName nn g = resolveNodeName' N.nnNull nn g

resolveNodeName' :: (N.NodeName nn) => nn -> nn -> Graph b nn a -> Maybe nn
resolveNodeName' _accum nn Empty =
  if N.nnLength nn == 1
  then Nothing
  else error "resolveNodeName, node length not 1"
resolveNodeName' accum nn (Graph g c) =
  if N.nnLength nn == 1
  then if contextName c == nn
       then Just (N.nnConcat accum nn)
       else case resolveNodeName' accum nn g of
              Nothing -> resolveNodeName'
                           (N.nnConcat accum (contextName c))
                           nn
                           (contextGraph c)
              Just found -> Just found
  else resolveNodeName' accum nn g

-- Find a node name from the last component, by doing a resolve first
resolveAndFindNodeName :: (N.NodeName nn) =>
    nn ->            -- Node name to look for
    Graph b nn a ->  -- Graph to look in
    Maybe (nn, Node b nn a) -- Node name and node returned
resolveAndFindNodeName nn g =
  let mname = resolveNodeName nn g
  in case mname of
       Nothing -> Nothing
       Just name ->
         let mnode = findNode name g
         in case mnode of
              Nothing -> Nothing
              Just node -> Just (name, node)

-- Modify a context in place
modifyContext :: (N.NodeName nn) =>
    nn -> Graph b nn a -> (Context b nn a -> Context b nn a) -> Graph b nn a
modifyContext _nn Empty _f = Empty
modifyContext nn (Graph g c) f =
  if N.nnIsPrefixOf (contextName c) nn
  then if contextName c == nn
       then Graph g (f c)
       else let shorter = N.nnDrop (N.nnLength (contextName c)) nn
                node = cNode c
                node' = node { subGraph = modifyContext shorter (subGraph node) f }
                c' = c { cNode = node' }
            in Graph (modifyContext nn g f) c'
  else Graph (modifyContext nn g f) c
  
-- Find a node in a recursive graph
findNode :: (N.NodeName nn) => nn -> Graph b nn a -> Maybe (Node b nn a)
findNode nn g = case findContext nn g of
                  Nothing -> Nothing
                  Just rc -> Just (cNode rc)

-- Determine if node is in a graph
isNode :: (N.NodeName nn) => nn -> Graph b nn a -> Bool
isNode nn g = isJust (findNode nn g)

-- Determine if the local name is duplicated. This deep graph structure
-- does not care about duplicated local names, but some applications,
-- such as DaVinci, do care
isDupName :: (N.NodeName nn) => nn -> Graph b nn a -> Bool
isDupName _nn Empty = False
isDupName nn (Graph g c) =
   isDupName nn g || contextName c == nn || isDupName nn (contextGraph c)

-- Determine if any element of a list of names is locally duplicated
isDupNames :: (N.NodeName nn) => [nn] -> Graph b nn a -> Bool
isDupNames nns g = or (map (\nn -> isDupName nn g) nns)

----------------------------------------------------------------------
-- Graph algorithms specifically for a recursive graph
----------------------------------------------------------------------
-- Produce the list of nodes that make up a graph
listNodes :: (N.NodeName nn) => Graph b nn a -> [Node b nn a]
listNodes Empty = []
listNodes (Graph g (Context _preds node _succs)) =
  [node] ++ (listNodes g) ++ (listNodes (subGraph node))

-- List the full (globalized) node names of the deep graph
-- The output is actually a list of nodes, but the node names
-- have been globalized.
listNodeNames :: (N.NodeName nn) => nn -> Graph b nn a -> [Node b nn a]
listNodeNames _path Empty = []
listNodeNames path (Graph g c) =
  let node = cNode c
      sg   = subGraph node
      subNodes = listNodeNames (path `N.nnConcat` (nodeName node)) sg
      gNodes = listNodeNames path g
      newNode = Node { nodeName    = path `N.nnConcat` nodeName node
                     , subGraph    = Empty
                     , nodeFreight = nodeFreight node
                     , nodeLabel   = ""
                     }
  in gNodes ++ (newNode : subNodes)

-- Produce the list of globalized edges (i.e. full path names in source
-- and sink, with no up or down links) in the graph, predecessors and
-- then successors
listEdges :: (N.NodeName nn) =>
             nn -> Graph b nn a -> ([Edge b nn], [Edge b nn])
listEdges _path Empty = ([], [])
listEdges path (Graph g c) =
  let (ps, ss)     = contextEdges path c
      (ps', ss')   = listEdges path g
      (ps'', ss'') = listEdges (path `N.nnConcat` (contextName c))
                               (subGraph (cNode c))
  in (ps ++ ps' ++ ps'', ss ++ ss' ++ ss'')

-- Produce the list of contexts that make up the graph
listContexts :: (N.NodeName nn) => Graph b nn a -> [Context b nn a]
listContexts Empty = []
listContexts (Graph graph context) = context : (listContexts graph)

-- Produce a list of immediate subnodes of a node, i.e. do not
-- probe the subgraphs
listImmediateNodes :: (N.NodeName nn) => Graph b nn a -> [Node b nn a]
listImmediateNodes Empty = []
listImmediateNodes (Graph g c) = (cNode c):(listImmediateNodes g)

-- Find the nth immediate subnode of a graph
getNthNode :: (N.NodeName nn) => Int -> Graph b nn a -> Maybe (Node b nn a)
getNthNode _n Empty = Nothing
getNthNode n (Graph g c) =
  if n == 0
  then Just (cNode c)
  else getNthNode (n - 1) g

-- Find the nth immediate subnode of a node
getNthSubNode :: (N.NodeName nn) => Int -> Node b nn a -> Maybe (Node b nn a)
getNthSubNode n node = getNthNode n (subGraph node)

-- Update a recursive graph, with the specified node update function.
-- The node name is accumulated as we walk the graph
-- updateGraph :: (N.NodeName nn) =>
--     nn                                 -> -- Node name accumulator
--     Graph b nn a                       -> -- Graph to update
--     (nn -> Node b nn a -> Node b nn a) -> -- Node update func
--     Graph b nn a                          -- Updated graph
-- updateGraph _accum Empty _f = Empty
-- updateGraph accum (Graph g c) f =
--   let node' = f accum (cNode c)
--       node'' = node' { subGraph =
--                         updateGraph (N.nnConcat accum (contextName c))
--                                     (subGraph node')
--                                     f
--                      }
--       c' = c { cNode = node'' }
--   in Graph (updateGraph accum g f) c'

-- Change the label (freight) associated with a node name
updateNodeFreight :: (N.NodeName nn) =>
    a            -> -- New node freight
    nn           -> -- Existing node name
    Graph b nn a -> -- Graph to look in
    Graph b nn a    -- Resulting graph
updateNodeFreight a nn g = modifyContext nn g (updateContextLabel a)

-- Change the label (freight) and name of an existing node
updateNodeFreightAndName :: (N.NodeName nn) =>
    a            -> -- New node freight
    nn           -> -- Existing node name
    nn           -> -- New node name
    Graph b nn a -> -- Graph to look in
    Graph b nn a    -- Resulting graph
updateNodeFreightAndName a oldnn newnn g =
    modifyContext oldnn g (updateContextLabelAndName a newnn)

-- Change the label (freight) and name of an existing node
updateNodeSubgraph :: (N.NodeName nn) =>
    nn           -> -- Node to update
    Graph b nn a -> -- Graph to update
    Graph b nn a -> -- New subgraph of the node
    Graph b nn a    -- Updated graph
updateNodeSubgraph nn g sub = modifyContext nn g (updateContextSubgraph sub)

-- Update a graph, with the specified node update function. 
-- The node name is accumulated as the graph is walked.
-- Only leaf nodes of the graph are updated.
updateLeafNodes :: (N.NodeName nn) =>
    nn                                 -> -- Node name accumulator
    Graph b nn a                       -> -- Graph to update
    (nn -> Node b nn a -> Node b nn a) -> -- Node update function
    Graph b nn a                          -- Updated graph
updateLeafNodes _accum Empty _f = Empty
updateLeafNodes accum (Graph g c) f =
  let node = cNode c
      node' =
        if isEmpty (subGraph node)
        then f accum node
        else node { subGraph = updateLeafNodes
                                 (N.nnConcat accum (nodeName node))
                                 (subGraph node)
                                 f
                  }
  in Graph (updateLeafNodes accum g f) (c { cNode = node' })

-- Convert a graph to a list of its top level contexts
stratify :: (N.NodeName nn) => Graph b nn a -> [Context b nn a]
stratify Empty = []
stratify (Graph g c) = c:stratify g

----------------------------------------------------------------------
-- Graph searches
----------------------------------------------------------------------

-- See if two nodes are directly connected
directlyConnected :: (N.NodeName nn) => Graph b nn a -> nn -> nn -> Bool
directlyConnected = isEdge

-- See if two nodes are indirectly connected
communicates :: (N.NodeName nn) => Graph b nn a -> nn -> nn -> Bool
communicates = undefined

----------------------------------------------------------------------
-- Map and fold
--
-- The folds consider only the nodes, not the edges at all.
----------------------------------------------------------------------

-- Left fold over a graph
foldlG :: (N.NodeName nn) => (d -> Node b nn a -> d) -> d -> Graph b nn a -> d
foldlG _f d Empty      = d
foldlG f d (Graph g c) = foldlG f (f d (cNode c)) g

-- Right fold over a graph
foldrG :: (N.NodeName nn) => (Node b nn a -> d -> d) -> d -> Graph b nn a -> d
foldrG _f d Empty = d
foldrG f d (Graph g c) = f (cNode c) (foldrG f d g)

-- Map from a graph with one node / edge type to a graph with another
-- node / edge type
mapG :: (N.NodeName nn) =>
    (a -> a') -> (b -> b') -> Graph b nn a -> Graph b' nn a'
mapG _h _k Empty     = Empty
mapG h k (Graph g c) = Graph (mapG h k g) (mapC h k c)

-- Map a context with one node / edge type to a context with another
-- node / edge type
mapC :: (N.NodeName nn) =>
    (a -> a') -> (b -> b') -> Context b nn a -> Context b' nn a'
mapC h k c =
  let n = cNode c
  in Context { cPreds = map (\e -> e { eFreight = k (eFreight e) }) (cPreds c)
             , cNode  = n { nodeFreight = h (nodeFreight n)
                          , subGraph    = mapG h k (subGraph n)
                          }
             , cSuccs = map (\e -> e { eFreight = k (eFreight e) }) (cSuccs c)
             }
