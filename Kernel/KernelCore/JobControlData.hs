-- Copyright (C) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module JobControlData
    ( SessionMap           -- session name -> session
    , outSessionMap        -- Format session map for printing
    , ProcessGroupMap      -- process group id -> processgroup
    , outProcessGroupMap   -- Format process group map for printing
    , DomainMap            -- domain name -> domain
    , outDomainMap         -- Format domain map for printing
    , JobControlData (..)  -- Combine maps into one structure
    , initJobControlData   -- Set all the maps to empty
    , newProcessGroupByPid -- Create a new process group
    , newSession           -- Create a new session
    , moveProcess          -- Move a process to another process group
    , searchPid            -- Check for Pid in PidMap
    , searchProcessGroupId -- Check for process group in job control data
    , addSessions          -- Add sessions to job control data
    , addDomains           -- Add domains to job control data
    , addProcessGroups     -- Add process groups to job control data
    , addProcesses         -- Add processes to job control data
    ) where

----------------------------------------------------------------------
-- Tables dealing with domains, sessions, process groups,
-- and processes.
--
-- The process / domain model is currently used as static data only.
-- It is converted to a dynamic form, as follows:
--
-- Domain map:        domain name -> domain
-- Domain:            domain name, [session name], [domain name]
-- Session map:       session name -> session
-- Session:           session name, [process group id]
-- Process group map: process group id -> processgroup
-- Processgroup:      process group name, process group id, [ProcessId]
-- PidMap:            process id -> process name
-- Process map:       process name -> Process
-- Process:           process name, process id, ioshell info,
--                    exec info, user half info.
-- Thread map:        thread id -> process name
--
----------------------------------------------------------------------

-- Haskell imports
import qualified FiniteMap as FM
import List
import Maybe
-- Posix imports
import qualified ProcessName as PN
import qualified ProcessId as PID
-- Osker imports
import qualified Session as SD
import qualified ProcessGroup as PG
import qualified Domain as DOM
import qualified ProcessMap as PM
import qualified PidMap as PIDM
import qualified ThreadMap as TM
import qualified Process as P
import qualified ProcessGroup as PG
import qualified OskerMessage as OM

-- The session map, maps the session name to a structure containing
-- dynamic information about the session. Sessions have no Ids, so
-- the session name is used for the domain of the session map.
type SessionMap = FM.FiniteMap PN.SessionName SD.Session

outSessionMap :: SessionMap -> String
outSessionMap dm = show (FM.fmToList dm)

-- The process group map, maps the process group id to a structure
-- containing dynamic information about the process group. The
-- process group id is the same as the process id, and it is
-- drawn from the same pool of ids as the process id.
type ProcessGroupMap = FM.FiniteMap PID.ProcessGroupId PG.ProcessGroup

outProcessGroupMap :: ProcessGroupMap -> String
outProcessGroupMap pgm = show (FM.fmToList pgm)

-- The domain map, maps the name of a leaf domain to information about
-- the domain.
type DomainMap = FM.FiniteMap PN.DomainName DOM.Domain

outDomainMap :: DomainMap -> String
outDomainMap dm = show (FM.fmToList dm)

-- Combine the job control data into one bunch
data JobControlData =
    JobControlData
    { jcdDomainMap       :: DomainMap
    , jcdSessionMap      :: SessionMap
    , jcdProcessGroupMap :: ProcessGroupMap
    , jcdProcessMap      :: PM.ProcessMap
    , jcdPidMap          :: PIDM.PidMap
    , jcdThreadMap       :: TM.ThreadMap
    }

-- Add sessions to the job control data
addSessions :: [SD.Session] -> JobControlData -> JobControlData
addSessions sessions jcd =
  foldl addSession jcd sessions
    where addSession :: JobControlData -> SD.Session -> JobControlData
          addSession jcd' session =
            jcd' { jcdSessionMap = FM.addToFM (jcdSessionMap jcd')
                                              (SD.sdSessionName session)
                                              session
                 }

-- Add domains to the job control data
addDomains :: [DOM.Domain] -> JobControlData -> JobControlData
addDomains domains jcd =
  foldl addDomain jcd domains
    where addDomain :: JobControlData -> DOM.Domain -> JobControlData
          addDomain jcd' domain =
            jcd' { jcdDomainMap = FM.addToFM (jcdDomainMap jcd')
                                              (DOM.ddDomainName domain)
                                              domain
                 }

-- Add process groups to the job control data
addProcessGroups :: [PG.ProcessGroup] -> JobControlData -> JobControlData
addProcessGroups pgs jcd =
  foldl addProcessGroup jcd pgs
   where addProcessGroup :: JobControlData -> PG.ProcessGroup -> JobControlData
         addProcessGroup jcd' pg =
           jcd' { jcdProcessGroupMap = FM.addToFM (jcdProcessGroupMap jcd')
                                                  (PG.pgProcessGroupId pg)
                                                  pg
                }

-- Add processes to the job control data
addProcesses :: [P.Process OM.OskerRspMsg] -> JobControlData -> JobControlData
addProcesses pgs jcd =
  foldl addProcess jcd pgs
   where addProcess :: JobControlData           ->
                       P.Process OM.OskerRspMsg ->
                       JobControlData
         addProcess jcd' p =
           jcd' { jcdProcessMap = FM.addToFM (jcdProcessMap jcd')
                                             (P.getProcessName p)
                                             p
                , jcdPidMap     = FM.addToFM (jcdPidMap jcd')
                                             (P.getProcessId p)
                                             (P.getProcessName p)
                , jcdThreadMap  = FM.addToFM (jcdThreadMap jcd')
                                             (P.getThreadId p)
                                             (P.getProcessName p)
                }

-- Get the length of a finite map
fmLen :: FM.FiniteMap a b -> Int
fmLen = length . FM.fmToList

instance Show JobControlData where
    show jcd =
      let domMap     = jcdDomainMap jcd
          sessMap    = jcdSessionMap jcd
          pgMap      = jcdProcessGroupMap jcd
          processMap = jcdProcessMap jcd
          pidMap     = jcdPidMap jcd
          threadMap  = jcdThreadMap jcd
      in "Job Control Data\nDomain Map: (" ++ show (fmLen domMap) ++
         ") " ++ show domMap ++
         "\nSession Map: (" ++ show (fmLen sessMap) ++
         ") " ++ show sessMap ++
         "\nProcessGroupMap: (" ++ show (fmLen pgMap) ++
         ") " ++ show pgMap ++
         "\nProcessMap: (" ++ show (fmLen processMap) ++
         ") " ++ show processMap ++
         "\nPidMap: (" ++ show (fmLen pidMap) ++
         ") " ++ show pidMap ++
         "\nThreadMap: (" ++ show (fmLen threadMap) ++
         ") " ++ show threadMap

-- Make a new job control data structure.
initJobControlData :: JobControlData
initJobControlData =
    JobControlData
    { jcdDomainMap       = FM.emptyFM
    , jcdSessionMap      = FM.emptyFM
    , jcdProcessGroupMap = FM.emptyFM
    , jcdProcessMap      = FM.emptyFM
    , jcdPidMap          = FM.emptyFM
    , jcdThreadMap       = FM.emptyFM
    }

----------------------------------------------------------------------
--
-- Operations on the job control data.
-- Operators defined as needed in support of the POSIX job control
-- system calls.
--
----------------------------------------------------------------------

-- A utility to fetch the job control data of a process id
getProcessData ::
    JobControlData ->    -- The input job control information
    PID.ProcessId  ->    -- The process id to put in the new process
                           -- group, also the new process group id.
    Maybe ( P.Process OM.OskerRspMsg  -- Output info about process
          , PG.ProcessGroup           -- Output info about process group
          , SD.Session                -- Output info about session
          )
getProcessData jcd pid =
  let pidMap          = jcdPidMap jcd
      processMap      = jcdProcessMap jcd
      processGroupMap = jcdProcessGroupMap jcd
      sessionMap      = jcdSessionMap jcd
      mprocessName    = FM.lookupFM pidMap pid
  in case mprocessName of
       Nothing -> Nothing
       Just processName ->
         let mprocess      = FM.lookupFM processMap processName
             sessionName   = PN.sessionName processName
             msession = FM.lookupFM sessionMap sessionName
         in case mprocess of
              Nothing -> error "Pid with no process"
              Just process ->
               let processTags = P.prProcessTags process
                   processGroupId = P.prProcessGroupId processTags
                   mprocessGroup = FM.lookupFM
                                     processGroupMap
                                     processGroupId
                 in case mprocessGroup of
                     Nothing -> error "Process with no process group"
                     Just processGroup ->
                      case msession of
                       Nothing -> error "Process with no session"
                       Just session ->
                         Just ( process
                              , processGroup
                              , session
                              )

-- Check if we know something about a PID
searchPid ::
    JobControlData -> -- Job control data to search
    PID.ProcessId  -> -- PID to search for
    Bool              -- True if found
searchPid jcd pid =
  let pidMap          = jcdPidMap jcd
      mprocessName    = FM.lookupFM pidMap pid
  in case mprocessName of
       Nothing -> False
       Just _processName -> True

-- Create a new process group, in the same session as before,
-- having a single process. The process Id of the single process in
-- the new process group is the same as the process group id of the
-- new process group. The process Id is alwasy the process Id of
-- an existing process, which is removed from an existing process
-- group. The new process group is created in the same session as
-- process group that currently contains the process group that
-- contains the specified process id.
newProcessGroupByPid ::
    JobControlData ->  -- The input job control information
    PID.ProcessId  ->  -- The process id to put in the new process
                       -- group, also the new process group id.
    Maybe JobControlData -- The output job control data.
newProcessGroupByPid jcd pid =
  let mjcddat = getProcessData jcd pid
      processGroupMap = jcdProcessGroupMap jcd
  in case mjcddat of
       Nothing -> Nothing
       Just (process, processGroup, session) ->
         let processTags = P.prProcessTags process
             processGroupId = P.prProcessGroupId processTags
             sessionName = SD.sdSessionName session
             -- Make a new process group
             pgroup = PG.mkProcessGroup sessionName pid
             -- Remove the process from the current process group
             processGroup' =
               processGroup { PG.pgProcesses =
                                delete pid (PG.pgProcesses processGroup)
                            }
             processGroupMap' =
               FM.addToFM processGroupMap processGroupId processGroup'
             -- Add in the new process group
             processGroupMap'' =
               FM.addToFM processGroupMap' pid pgroup
         in Just jcd { jcdProcessGroupMap = processGroupMap'' }

-- Look for a process group id in the job control data.
searchProcessGroupId ::
    JobControlData -> -- Job control data to search
    PID.ProcessId  -> -- Process group id to search for
    Bool              -- True if process group id found
searchProcessGroupId jcd pgid =
  let processGroupMap = jcdProcessGroupMap jcd
      mprocessGroup = FM.lookupFM processGroupMap pgid
  in case mprocessGroup of
       Nothing -> False
       Just _processGroup -> True

-- Move a process to another process group
moveProcess ::
    JobControlData -> -- Job control data to search
    PID.ProcessId  -> -- Process to move
    PID.ProcessId  -> -- Process group to move it to
    Maybe JobControlData -- Output job control data
moveProcess jcd pid pgid =
  let mjcddat = getProcessData jcd pid
      processGroupMap = jcdProcessGroupMap jcd
  in case mjcddat of
       Nothing -> Nothing
       Just (process, processGroup, session) ->
         let processTags = P.prProcessTags process
             processGroupId = P.prProcessGroupId processTags
             -- Remove the process from the current process group
             processGroup' =
               processGroup { PG.pgProcesses =
                                delete pid (PG.pgProcesses processGroup)
                            }
             processGroupMap' =
               FM.addToFM processGroupMap processGroupId processGroup'
             -- Look up the new process group
             mprocessGroupNew = FM.lookupFM processGroupMap pgid
         in case mprocessGroupNew of
              Nothing -> Nothing
              Just processGroupNew ->
                let -- Add the process to the new process group
                    processGroupNew' =
                      processGroupNew { PG.pgProcesses =
                                         pid:(PG.pgProcesses processGroupNew) }
                    -- Update the prcess group map with this change
                    processGroupMap'' =
                      FM.addToFM processGroupMap' pgid processGroupNew'
                    -- Commit the updates to the job control data.
                    jcd' = jcd { jcdProcessGroupMap = processGroupMap'' }
                in if elem pgid (SD.sdProcessGroups session)
                   then Just jcd'
                   else Nothing

-- Create a new session. The session will have one process, which
-- will be in its own process group. The id of the process group
-- will be the same as the id of the process.
-- The session will be named after the process that made it.
newSession ::
    JobControlData   ->  -- The input job control information
    PID.ProcessId    ->  -- The process id to put in the new session
    Maybe JobControlData -- The output job control data.
newSession jcd pid =
  let mjcddat = getProcessData jcd pid
      processGroupMap = jcdProcessGroupMap jcd
      sessionMap      = jcdSessionMap jcd
  in case mjcddat of
       Nothing -> Nothing
       Just (process, processGroup, session) ->
         let processTags = P.prProcessTags process
             processGroupId = P.prProcessGroupId processTags
             -- Build the new session name, named after the process.
             sessionName  = SD.sdSessionName session
             newSessionName = sessionName ++ [show pid]
             -- Make a new process group for the session, having only
             -- pid in its list of pids.
             pgroup = PG.mkProcessGroup sessionName pid
             -- Make a new session, containing the new process group.
             newSessionData = SD.Session newSessionName [pid]
             -- Remove the process from the current process group
             processGroup' =
               processGroup { PG.pgProcesses =
                                delete pid (PG.pgProcesses processGroup)
                            }
             processGroupMap' =
               FM.addToFM processGroupMap processGroupId processGroup'
             -- Add in the new process group
             processGroupMap'' =
               FM.addToFM processGroupMap' pid pgroup
             -- Add in the new session
             sessionMap' =
               FM.addToFM sessionMap newSessionName newSessionData
         in Just jcd { jcdProcessGroupMap = processGroupMap''
                     , jcdSessionMap = sessionMap' }
