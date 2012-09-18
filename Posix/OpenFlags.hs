-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module OpenFlags
    ( OpenFlag (..) -- A single open flag
    , OpenFlags     -- A list of open flags
    , flagRead      -- Is read permission requested
    , flagWrite     -- Is write permission requested
    , nullFlags     -- Empty set of flags
    ) where

----------------------------------------------------------------------
-- The POSIX file open flags. They are also used by other operations,
-- such as the POSIX.4 message queue.
----------------------------------------------------------------------

data OpenFlag
    = O_RDONLY   -- Open with only read permission
    | O_WRONLY   -- Open with only write permission
    | O_RDWR     -- Open with read and write permission
    | O_CREAT    -- Create the object
    | O_EXCL     -- Create exclusively (do not open existing)
    | O_NONBLOCK -- Do not block for resources, return error instead
    deriving (Eq, Ord, Enum, Show)

type OpenFlags = [OpenFlag]

-- Empty flags
nullFlags :: OpenFlags
nullFlags = []

-- Check if read permission requested
flagRead :: OpenFlags -> Bool
flagRead flags = elem O_RDONLY flags || elem O_RDWR flags

-- Check if write permission requested
flagWrite :: OpenFlags -> Bool
flagWrite flags = elem O_WRONLY flags || elem O_RDWR flags
