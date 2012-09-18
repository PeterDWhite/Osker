-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module DirectoryCache
    ( DirectoryCache   -- The directory system cache
    , NodeFreight (..) -- Data stored at each node
    , dirInitCache     -- Initialize the file system cache
    , dirInsertCache   -- Insert a file into the cache
    , dirDetectCache   -- See if a file is in the cache
    , dirRemoveCache   -- Remove an entry from the cache
    , dirComplete      -- Is directory entry completed?
    , lookupDirectory  -- Lookup a directory entry in the cache
    , getNthFile       -- Get the nth file from a directory
    , getFileInfo      -- Get file information from a node
    , bumpDir          -- Update the read count for a directory
    , updateDir        -- Update the freight of a node
    ) where

----------------------------------------------------------------------
-- The file system cache, yet another instance of the recursive
-- graph type
----------------------------------------------------------------------

-- Haskell imports
import qualified Posix as P
-- Recursive graph imports
import qualified Deep as D
-- Posix imports
import qualified FileName as FN

-- Information stored in a node
data NodeFreight =
    NodeFreight
    { fileName    :: FN.FileName -- relative to base of file system
    , isDirectory :: Bool        -- Is it a directory?
    , dirCount    :: Int         -- Number of files in the directory
    , readCount   :: Int         -- Number of files cached under directory
    , stream      :: Maybe P.DirStream -- Directory stream, when file is dir
    }

-- Is the directory completed in the cache?
dirComplete :: NodeFreight -> Bool
dirComplete nf = dirCount nf == readCount nf

-- Bump the readCount in the node freight
bump :: NodeFreight -> NodeFreight
bump nf = nf { readCount = readCount nf + 1 }

instance Show NodeFreight where
    show nf = "NodeFreight " ++ FN.outFileName (fileName nf) ++
              ", id = " ++ show (isDirectory nf) ++
              ", dc = " ++ show (dirCount nf) ++
              ", rc = " ++ show (readCount nf)

-- Information stored on an edge
type EdgeFreight = () -- Nothing for now

----------------------------------------------------------------------
-- The file system cache uses the recursive graph, with
--    node name as file name
--    edge freight as file system link
--    node freight as file name (same as node name)
----------------------------------------------------------------------

type DirectoryCache = D.Graph EdgeFreight FN.FileName NodeFreight
-- For an arbitrary node in the graph
type FileNode = D.Node EdgeFreight FN.FileName NodeFreight
-- An edge in the graph
--type FileEdge = D.Edge EdgeFreight FN.FileName

-- Initialize the file system cache
dirInitCache :: DirectoryCache
dirInitCache = D.Empty

-- Insert a new entry into the directory cache
dirInsertCache ::
    DirectoryCache    -> -- Current state of directory cache
    FN.FileName       -> -- File to insert
    Bool              -> -- Is it a directory?
    Int               -> -- Number of files in the directory
    Maybe P.DirStream -> -- Posix directory stream
    DirectoryCache    -- Updated directory cache
dirInsertCache cache fn isDir n mpstream =
  let node = D.Node { D.nodeName    = fn
                    , D.subGraph    = D.Empty
                    , D.nodeFreight = NodeFreight { fileName    = fn
                                                  , isDirectory = isDir
                                                  , dirCount    = n
                                                  , readCount   = 0
                                                  , stream      = mpstream
                                                  }
                    , D.nodeLabel   = ""
                    }
  in D.insNode node cache

-- Remove a cached directory entry
dirRemoveCache :: DirectoryCache -> FN.FileName -> DirectoryCache
dirRemoveCache cache fn = D.delNode fn cache

-- Detect if a filename is in the cache
dirDetectCache :: DirectoryCache -> FN.FileName -> Maybe NodeFreight
dirDetectCache cache fn =
  let mnode = D.findNode fn cache
  in case mnode of
       Nothing -> Nothing
       Just node -> Just (D.nodeFreight node)

-- Find an entry in the cache
lookupDirectory :: DirectoryCache -> FN.FileName -> Maybe FileNode
lookupDirectory cache fn = D.findNode fn cache

-- Get the Nth file from a directory
getNthFile :: Int -> FileNode -> Maybe (FileNode)
getNthFile = D.getNthSubNode

-- Get node freight from node
getFileInfo :: FileNode -> NodeFreight
getFileInfo = D.nodeFreight

-- Bump the read count in a directory in the cache
bumpDir :: DirectoryCache -> FN.FileName -> DirectoryCache
bumpDir cache fn =
  let mnode = lookupDirectory cache fn
  in case mnode of
       Nothing -> cache
       Just node -> 
         let freight = getFileInfo node
         in D.updateNodeFreight (bump freight) fn cache

-- Update the freight of a directory
updateDir ::
    DirectoryCache ->  -- Current cache
    FN.FileName ->     -- Name of node to update
    NodeFreight ->     -- New freight
    DirectoryCache     -- Updated cache
updateDir cache fn freight =
  let mnode = lookupDirectory cache fn
  in case mnode of
       Nothing -> cache
       Just _node -> D.updateNodeFreight freight fn cache
