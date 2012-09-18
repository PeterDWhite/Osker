-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ProcessDomainBraid
    ( NodeBraid (..)     -- Braid corresponding to a node in the process /
                         -- domain model graph
    , NodeBraidType (..) -- The type of a node braid
    , nodeBraidType      -- Get the type of a node braid
    ) where

----------------------------------------------------------------------
-- The braid applied to each node of the process domain model
----------------------------------------------------------------------

-- Utility imports
import qualified Null as N
-- Posix imports
import qualified ProcessName as PN
-- Braid imports
import qualified BraidInternal as B
import qualified BraidState as BST
-- Graph imports
--import qualified Deep as D
-- Domain imports
--import qualified DomainGraph as DG
-- Osker imports
--import qualified ProcessBraid as PrB
import qualified DomainBraid as DB
import qualified PlatformBraid as PlB

-- A braid corresponding to a node can be either a platform braid,
-- A domain braid, or a process braid. Thus a node braid encodes
-- the structure of the process domain model as a braid.
data NodeBraid
    = PlatformNode  { plBraidSt :: PlB.PlatformSt
                    , name      :: PN.ProcessName
                    }
    | DomainNode    { dmBraidSt :: DB.DomainSt
                    , name      :: PN.ProcessName
                    }
    | ProcessNode   { name      :: PN.ProcessName }
    | NullNodeBraid { name      :: PN.ProcessName }
      deriving (Show)

data NodeBraidType
    = PlatformType
    | DomainType
    | ProcessType
    | NullType
      deriving (Show)

-- Get the type of a node braid
nodeBraidType :: NodeBraid -> NodeBraidType
nodeBraidType (PlatformNode _ _) = PlatformType
nodeBraidType (DomainNode _ _)   = DomainType
nodeBraidType (ProcessNode _)    = ProcessType
nodeBraidType (NullNodeBraid _)  = NullType

instance N.Null NodeBraid where
    N.mkNull = NullNodeBraid { name = PN.emptyPN }

{-
-- Convert the process domain model into a node braid
mkProcessDomainBraid :: DG.DomainGraph -> NodeBraid 
mkProcessDomainBraid g = D.foldlG mkSingleContextBraid N.mkNull g

-- Convert a single node in the process / domain model into a node braid
mkSingleContextBraid :: NodeBraid -> DG.DomainGraphNode -> NodeBraid
mkSingleContextBraid nb node =
  let sub = D.subGraph node
  in case DG.nodeType node of
       DG.PlatformNodeType     -> error "mkSingleContextBraid.1"
       DG.DomainNodeType       -> error "mkSingleContextBraid.2"
       DG.ProcessNodeType      -> error "mkSingleContextBraid.5"
       DG.SessionNodeType      ->
         -- There is no braid corresponding to the session
         error "mkSingleContextBraid.3"
       DG.ProcessGroupNodeType ->
         -- There is no braid corresponding to the process group
         error "mkSingleContextBraid.4"

-- Accumulate the node name on the way down the graph
accumulateNodeName :: NodeBraid -> DG.DomainGraphNode -> PN.ProcessName
accumulateNodeName nb node = name nb ++ DG.nodeName node

-- Accumulate the node name on the way down the graph, and insert
-- it into the node braid
accumulateNodeBraid :: NodeBraid -> DG.DomainGraphNode -> NodeBraid
accumulateNodeBraid nb node =
  let accum = accumulateNodeName nb node
  in case DG.nodeType node of
       DG.PlatformNodeType     ->
         PlatformNode { plBraidSt = N.mkNull
                      , name      = accum
                      }
       DG.DomainNodeType       ->
         let subgraph = DB.subGraph node
             subbraid = mkProcessDomainBraid 
         in case nodeBraidType nb of
              PlatformType ->
                let threadinit =
                      B.ThreadInit
                      { B.tiName       = PN.outProcessName accum
                      , B.tiProg       = undefined
                      , B.tiLocalState = N.mkNull
                      }
                in PlatformNode { plBraidSt = B.lifts (plBraidSt nb) threadinit
                                , name      = accum
                                }
              DomainType ->
                DomainNode  { dmBraidSt = undefined
                            , name      = accum
                            }
              ProcessType -> error "accumulateNodeBraid/domain under process"
              NullType    -> error "accumulateNodeBraid/domain under null"
 -}
