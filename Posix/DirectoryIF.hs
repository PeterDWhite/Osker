-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module DirectoryIF
    ( closedir    -- Posix interface
    , opendir     -- Posix interface
    , readdir     -- Posix interface
    , rewinddir   -- Posix interface
    , D.DIR (..)  -- Directory structure, re-exported
    ) where

----------------------------------------------------------------------
-- The POSIX.1 directory interfaces
----------------------------------------------------------------------

-- Posix imports
import qualified SystemCall as SC
import qualified DirectoryData as D

----------------------------------------------------------------------
-- 
-- closedir ( DIR *dirp )
-- Close the directory stream indicated by dirp
--
----------------------------------------------------------------------
closedir ::
    D.DIR         ->       -- Directory structure
    SC.U SC.SystemResponse -- Sometimes returns the existing sigaction
closedir dir = SC.osker (SC.CloseDirReq dir)

----------------------------------------------------------------------
--
-- opendir ( char *dirname )
-- Open a directory stream corresponding to the path name indicated
--
----------------------------------------------------------------------
opendir ::
    String        ->      -- The path name of the directory
    SC.U SC.SystemResponse -- Error indication
opendir path = SC.osker (SC.OpenDirReq path)

----------------------------------------------------------------------
--
-- readdir ( DIR *dirp )
-- Return a pointer to a *NEXT* directory entry from the
-- parameter dirp.
-- On end of file, Nothing is returned.
-- The directories . and .. are NOT returned.
--
----------------------------------------------------------------------
readdir ::
    D.DIR         ->       -- The directory to scan
    SC.U SC.SystemResponse -- Error indication returned
readdir dir = SC.osker (SC.ReadDirReq dir)

----------------------------------------------------------------------
--
-- rewinddir ( DIR *dirp )
-- *RESET* position associated with directory stream indicated to
-- the first directory entry.
--
----------------------------------------------------------------------
rewinddir ::
    D.DIR         ->       -- The directory to rewind
    SC.U SC.SystemResponse -- Error indication returned
rewinddir dir = SC.osker (SC.RewindDirReq dir)
