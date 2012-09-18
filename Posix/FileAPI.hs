-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module FileAPI
    ( F.File (..)     -- Re-export
    , fputs           -- Posix fputs, Write a string to a file
    , fputc           -- Posix fputc, Write a character to a file
    , fgets           -- Posix fgets, Read a string from a file
    , fgetc           -- Posix fgetc, Read a char from a file
    , ftell           -- Posix ftell, Tell where file is positioned
    , fseek           -- Posix fseek, Position the file
    , gets            -- Posix gets, Get a string from standard input
    , getchar         -- Posix getchar, Get a char from standard input
    , puts            -- Posix puts, Put a string on standard output
    , putchar         -- Posix putchar, Put a char on standard output
    ) where

----------------------------------------------------------------------
-- Implementing the Posix stream
----------------------------------------------------------------------

-- Local imports
import qualified File as F
import qualified SystemCall as SC
import qualified Whence as W

-- Write a string to a file
fputs ::
    String        ->       -- String to write
    F.File        ->       -- File to write
    SC.U SC.SystemResponse -- Response
fputs s f = SC.osker (SC.FPutsReq s f)

-- Write a char to a file
fputc ::
    Char          ->       -- Character to write
    F.File        ->       -- File to write
    SC.U SC.SystemResponse -- Response
fputc c f = SC.osker (SC.FPutcReq c f)

-- Read up to n chars from a file
fgets ::
    Int           ->       -- Length of string to get
    F.File        ->       -- File to read
    SC.U SC.SystemResponse -- String returned
fgets n f = SC.osker (SC.FGetsReq n f)

-- Read up a char from a file
fgetc ::
    F.File        ->       -- File to read
    SC.U SC.SystemResponse -- Char returned
fgetc f = SC.osker (SC.FGetcReq f)

-- Tell where file is positioned
ftell ::
    F.File        ->       -- File to read
    SC.U SC.SystemResponse -- Char returned
ftell f = SC.osker (SC.FTellReq f)

-- Position the file
fseek ::
    F.File        ->       -- File to read
    Int           ->       -- Offset
    W.Whence      ->       -- Base for offset
    SC.U SC.SystemResponse -- Char returned
fseek f offset base = SC.osker (SC.FSeekReq f offset base)

-- Get a string from standard input
gets :: SC.U SC.SystemResponse -- String returned
gets = SC.osker SC.GetsReq

-- Get a char from standard input
getchar :: SC.U SC.SystemResponse -- Char returned
getchar = SC.osker SC.GetCharReq

-- Put a string on standard output
puts ::
    SC.SystemResponse ->    -- Response to send
    SC.U SC.SystemResponse  -- Error indication returned
puts sysrsp = SC.osker (SC.PutsReq sysrsp)

-- Put a character on standard output
putchar :: Char -> SC.U SC.SystemResponse
putchar c = SC.osker (SC.PutCharReq c)
