-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module CoreData
    ( FSData (..) -- The state maintained by the file system core
    , initFSData  -- Initializer for the file system core state
    ) where

----------------------------------------------------------------------
-- The file system core device driver data structures
----------------------------------------------------------------------

-- Osker imports
import qualified DirectoryCache as DC

-- The data maintained by the file system core device driver
data FSData = FSData { fileSystem     :: String
                     , directoryCache :: DC.DirectoryCache
                     }

initFSData :: String -> FSData
initFSData fsName =
  FSData { fileSystem     = fsName
         , directoryCache = DC.dirInitCache
         }
