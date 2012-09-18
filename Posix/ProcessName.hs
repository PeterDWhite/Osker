-- Copyright (C) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module ProcessName
    ( ProcessName        -- Type of process names
    , outProcessName     -- Observer on ProcessName
    , outProcName        -- Format process name without platform / domain
    , outProcessNameLast -- Print last element of process name
    , emptyPN            -- Constructor for ProcessName
    , ProcessGroupName   -- Type of process group names
    , SessionName        -- Type of session names
    , DomainName         -- Type of domain names
    , processGroupName   -- Process name to process group name
    , sessionName        -- Process name to session name
    , domainName         -- Process name to domain name
    , sameDomain         -- Check if two processes are in the same domain
    ) where

----------------------------------------------------------------------
-- This is the name of the process, per the process / domain model.
-- Session names and process group names are more of the same.
----------------------------------------------------------------------

-- Haskell imports
import List

type ProcessNameComponent = String
type ProcessName      = [ProcessNameComponent]
type ProcessGroupName = ProcessName
type SessionName      = ProcessName
type DomainName       = ProcessName

-- An empty process name
emptyPN :: ProcessName
emptyPN = []

-- Format the process name for printing
outProcessName :: ProcessName -> String
outProcessName pn = concat (intersperse "/" pn)

-- Format the process name without the platform/domain components
outProcName :: ProcessName -> String
outProcName pn =
  if length pn <= 3
  then outProcessName pn 
  else (outProcessName . tail . tail) pn

-- Print out the last element of the process name
outProcessNameLast :: ProcessName -> String
outProcessNameLast pn = if null pn
                        then ""
                        else last pn

-- Because of the structure of names in the domain / process model,
-- the following routines can convert a process name into a process
-- group name and a session name and a domain name
processGroupName :: ProcessName -> ProcessGroupName
processGroupName = init

-- Get the session name from a process name
sessionName :: ProcessName -> ProcessGroupName
sessionName = init . init

-- Get the domain name from a process name
domainName :: ProcessName -> ProcessGroupName
domainName = init . init . init

-- Check if two processes are in the same domain
sameDomain :: ProcessName -> ProcessName -> Bool
sameDomain pn1 pn2 = domainName pn1 == domainName pn2
