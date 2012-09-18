-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ProgramTable
    ( ProgramTable      -- Table mapping process name to user half
    , listProcessNames  -- List the process names from the table
    , emptyProgramTable -- Build an empty program table
    ) where

----------------------------------------------------------------------
-- Maps process name to user program.
-- Used only during Osker initialization
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
-- Posix imports
import qualified ProcessName as PN
-- User Process imports
import qualified SystemCall as SC

type ProgramTable = FM.FiniteMap PN.ProcessName ( SC.U () )

-- List out the program names from the program table.
listProcessNames :: ProgramTable -> [PN.ProcessName]
listProcessNames = FM.keysFM

-- Empty program table
emptyProgramTable :: ProgramTable
emptyProgramTable = FM.emptyFM
