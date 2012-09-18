-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module Errno ( Errno (..) ) where

----------------------------------------------------------------------
-- The POSIX error numbers
----------------------------------------------------------------------

data Errno =
    NOERROR   | -- No error has been returned yet
    E2BIG     | -- Argument list is too long
    EACCESS   | -- Permission denied
    EAGAIN    | -- Insufficient resources, or an internal limit exceeded
    EBADF     | -- Bad file descriptor
    EBUSY     | -- Resource unavailable
    ECHILD    | -- No child processes
    EDEADLK   | -- Resource deadlock would result
    EDOM      | -- Mathematical domain error
    EEXIST    | -- File / object already exists
    EFAULT    | -- Invalid address
    EFBIG     | -- File / object too large
    EINTR     | -- Function interrupted by signal
    EINVAL    | -- Invalid argument
    EIO       | -- I/O error
    EISDIR    | -- Is a directory
    EMFILE    | -- Too many open files by the process
    EMLINK    | -- Too many links to a file
    ENAMETOOLONG | -- Filename too long
    ENFILE    | -- Too many open files in the system
    ENODEV    | -- No such device
    ENOENT    | -- No such file
    ENOEXEC   | -- Not an executable file
    ENOLCK    | -- No locks available
    ENOMEM    | -- Not enough memory
    ENOSPC    | -- No space left on device
    ENOSYS    | -- Function not supported
    ENOTDIR   | -- Not a directory
    ENOTEMPTY | -- Directory not empty
    ENOTTY    | -- Inappropriate I/O control operation
    ENXIO     | -- No such device or address
    EPERM     | -- Operation not permitted
    EPIPE     | -- Broken pipe
    ERANGE    | -- Result too large
    EROFS     | -- Read only file system
    ESPIPE    | -- Invalid seek
    ESRCH     | -- No such process
    EXDEV     | -- Invalid link
    ELAST       -- Placeholder for last error message
    deriving (Eq, Ord, Enum, Show)
