-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileSystem
    ( FileSystem           -- Recursive graph, specialized to file system
    , FileNode             -- Node in the graph, representing a file
    , FileEdge             -- Usually representing a link
    , FileFreight (..)     -- Info about a file
    , LinkFreight (..)     -- Information about a link
    , createFileSystem     -- Create the dummy file system
    , parseFileSystem      -- Parse in the file system
    , outFileSystem        -- Print out a file system
    ) where

----------------------------------------------------------------------
-- File system and directory structure, based on the
-- recursive graph data type.
----------------------------------------------------------------------

-- Haskell imports
import qualified System as SYS
import qualified Parsec as P
-- Utility imports
import qualified Indent as I
-- Recursive graph imports
import qualified Deep as D
-- Posix imports
import qualified FileName as FN
import qualified ParseFileName as PFN

-- Information stored in a node
data FileFreight =
    FileFreight
    { fileName :: FN.FileName -- relative to base of file system
    } deriving (Show)

-- Information stored on an edge
data LinkFreight = 
    LinkFreight
    { linkTo :: FN.FileName -- Where the link goes to
    } deriving (Show)

----------------------------------------------------------------------
-- The file system graph uses the recursive graph, with
--    node name as file name
--    edge freight as file system link
--    node freight as file name (same as node name)
----------------------------------------------------------------------

type FileSystem = D.Graph LinkFreight FN.FileName FileFreight
-- For an arbitrary node in the graph
type FileNode = D.Node LinkFreight FN.FileName FileFreight
-- An edge in the graph, usually to represent a link (or alias)
type FileEdge = D.Edge LinkFreight FN.FileName

-- Create the file system, from the indicated root (fsName)
createFileSystem :: String -> IO [PFN.FileEntry]
createFileSystem fsName =
  do { exitCode <-
         SYS.system ("find " ++ fsName ++ " -printf \"%p>%l\\n\" >t")
     ; efsEntries <- P.parseFromFile PFN.parseFileSystem "t"
     ; case efsEntries of
         Right fsEntries -> return fsEntries
         Left e -> error ("File system parse error: " ++ show e)
     }

-- Make a file system node from a File Entry
mkNode :: PFN.FileEntry -> FileNode
mkNode fe =
  let fn = drop 3 (PFN.getFileName fe) -- Get rid of the "/home/peter/ghc-5.04"
  in D.Node { D.nodeName = fn
            , D.subGraph = D.Empty
            , D.nodeFreight = FileFreight fn
            , D.nodeLabel = ""
            }

-- Parse in a file system
-- ***** Does not handle the links yet
parseFileSystem :: String -> IO FileSystem
parseFileSystem fsName =
  do { fsEntries <- createFileSystem fsName
     ; let fsNodes   = map mkNode fsEntries
     ; return ( D.insNodes fsNodes D.Empty )
     }

-- Print out a file system
outFileSystem :: Int -> FileSystem -> String
outFileSystem _n D.Empty = ""
outFileSystem n (D.Graph g c) =
  let node = D.cNode c
      f    = D.nodeFreight node
      nn   = D.nodeName node
  in if length nn > 1
     then error ("Node name too long: " ++ show nn)
     else I.indent n ++ show (D.nodeName node) ++ " ... " ++
          (FN.outFileName (fileName f)) ++ "\n" ++
          outFileSystem (n + 1) (D.subGraph node) ++
          outFileSystem n g
