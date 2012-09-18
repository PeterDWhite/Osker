-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Mode
    ( Mode (..)          -- Create modes for files, message queues, ...
    , isCreatorUserRead  -- Does creator get read permission
    , isCreatorUserWrite -- Does creator get write permission
    , isLocalUserRead    -- Check if local user gets read
    , isLocalUserWrite   -- Check if local user gets write
    ) where

----------------------------------------------------------------------
-- The POSIX file / object modes
----------------------------------------------------------------------

data Mode
    = S_IRUSR -- Read permission for user
    | S_IWUSR -- Write permission for user
    | S_IXUSR -- Execute permission for user
    | S_IRWXU -- Read, write, and execute for user
    | S_IRGRP -- Read permission for group
    | S_IWGRP -- Write permission for group
    | S_IXGRP -- Execute permission for group
    | S_IRWXG -- Read, write, and execute for group
    | S_IROTH -- Read permission for others
    | S_IWOTH -- Write permission for others
    | S_IXOTH -- Execute permission for others
    | S_IRWXO -- Read, write, and execute for others
    deriving (Eq, Ord, Enum, Show)

-- Check if the creator gets read permission
isCreatorUserRead :: Mode -> Bool
isCreatorUserRead S_IRUSR = True
isCreatorUserRead S_IRWXU = True
isCreatorUserRead S_IRGRP = True
isCreatorUserRead S_IRWXG = True
isCreatorUserRead S_IROTH = True
isCreatorUserRead S_IRWXO = True
isCreatorUserRead _ = False

-- Check if the creator gets read permission
isCreatorUserWrite :: Mode -> Bool
isCreatorUserWrite S_IWUSR = True
isCreatorUserWrite S_IRWXU = True
isCreatorUserWrite S_IWGRP = True
isCreatorUserWrite S_IRWXG = True
isCreatorUserWrite S_IWOTH = True
isCreatorUserWrite S_IRWXO = True
isCreatorUserWrite _ = False

-- Check if local user gets read permission
-- **** Does not implement groups yet
isLocalUserRead :: Mode -> Bool
isLocalUserRead S_IRUSR = True
isLocalUserRead S_IRWXU = True
isLocalUserRead S_IROTH = True
isLocalUserRead S_IRWXO = True
isLocalUserRead _ = False

-- Check if local user gets write permission
-- **** Does not implement groups yet
isLocalUserWrite :: Mode -> Bool
isLocalUserWrite S_IWUSR = True
isLocalUserWrite S_IRWXU = True
isLocalUserWrite S_IWOTH = True
isLocalUserWrite S_IRWXO = True
isLocalUserWrite _ = False
