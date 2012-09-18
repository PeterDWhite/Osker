-- Copyright (C) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module JCState
    ( JCD.JobControlData (..)  -- Combine maps into one structure
    , JCD.initJobControlData   -- Set all the maps to empty
      -- Structured job control state
    , JCState (..)         -- The state passed up and down job control
    , updateGps            -- Update the GPS within a JCS
    , updateJcd            -- Update the JCD within a JCS
    , updateGpsJcd         -- Use JCD from JCS to update JCD in GPS in JCS
    , updateDs             -- Update the domain state
    , addSessions          -- Add sessions to job control state
    , addDomains           -- Add domains to job control state
    , addProcessGroups     -- Add process groups to job control state
    , addProcesses         -- Add processes to job control state
    , getProcessMap        -- Get the process map from job control state
      -- Return values for job control stack
    , JCRet                -- Return value of three lists
    , mkJCRet              -- Constructor for JC Return value
    , nullJCRet            -- Null return value
    , append               -- Combine two return values
    , append2              -- Combine two lists, injects a value into the third
    , TL.first             -- Get the second list from a return
    , TL.second            -- Get the second list from a return
    , TL.third             -- Get the third list from a return
    , updateThird          -- Update the third list in a return value
    ) where

----------------------------------------------------------------------
-- Job control state, passed up and down job control stack
----------------------------------------------------------------------

-- Haskell imports
import List
-- Utility imports
import qualified TripleList as TL
-- Braid imports
import qualified BraidUtilities as BU
import qualified DomainBraid as DB
-- Osker imports
import qualified JobControlData as JCD
import qualified Session as SD
import qualified ProcessGroup as PG
import qualified Domain as DOM
import qualified ProcessMap as PM
import qualified Process as P
import qualified ProcessGroup as PG
import qualified OskerMessage as OM
import qualified GlobalPartitionedState as GPS
import qualified OskerGlobal as OG
import qualified FileName as FN

----------------------------------------------------------------------
-- Structure the data passed up and down the job control stack
----------------------------------------------------------------------

data JCState =
    JCState { -- Updateable fields
              jcsGps        :: GPS.GlobalPartitionedState
            , jcsJcd        :: JCD.JobControlData
              -- Read only fields
            , jcsOskerMVar  :: OG.OskerMVar    -- Osker global tables
            , jcsKcChan     :: OM.OskerChannel -- Channel to kernel core
            , jcsFsCoreChan :: OM.OskerChannel -- Channel to file system core
            , jcsFsRoot     :: FN.FileName     -- File system root directory
              -- Layered braid support
            , jcsDomainSt   :: DB.DomainSt     -- Building a domain braid state
            }

-- Update the job control portion of the global partitioned state
updateGpsJcd :: JCState -> JCState
updateGpsJcd jcs = updateGps jcs (GPS.updateJcd (jcsJcd jcs))

-- Update the global partitioned state in the job control state
updateGps ::
    JCState ->
    (GPS.GlobalPartitionedState -> GPS.GlobalPartitionedState) -> JCState
updateGps jcs f = jcs { jcsGps = f (jcsGps jcs) }

-- Update the domain state
updateDs :: JCState -> DB.DomainSt -> JCState
updateDs jcs ds = jcs { jcsDomainSt = ds }

-- Update the job control data
updateJcd :: JCState -> (JCD.JobControlData -> JCD.JobControlData) -> JCState
updateJcd jcs f = jcs { jcsJcd = f (jcsJcd jcs ) }

-- Add sessions to job control state
addSessions :: JCState -> [SD.Session] -> JCState
addSessions jcs sessions = updateJcd jcs (JCD.addSessions sessions)

-- Add domains to job control state
addDomains :: JCState -> [DOM.Domain] -> JCState
addDomains jcs domains = updateJcd jcs (JCD.addDomains domains)

-- Add process groups to a job control state
addProcessGroups :: JCState -> [PG.ProcessGroup] -> JCState
addProcessGroups jcs pgs = updateJcd jcs (JCD.addProcessGroups pgs)

-- Add processes to a job control state
addProcesses :: JCState -> [P.Process OM.OskerRspMsg] -> JCState
addProcesses jcs ps = updateJcd jcs (JCD.addProcesses ps)

-- Get the process map from the job control state
getProcessMap :: JCState -> PM.ProcessMap
getProcessMap = JCD.jcdProcessMap . jcsJcd

----------------------------------------------------------------------
-- Return values for the job control stack
----------------------------------------------------------------------

-- The return values always have two lists of one shots
type JCRet a = TL.TripleList BU.OneShot BU.OneShot a

-- Make a return value
mkJCRet :: [BU.OneShot] -> [BU.OneShot] -> [a] -> JCRet a
mkJCRet = TL.mkTripleList

-- Null return value
nullJCRet :: JCRet a
nullJCRet = TL.nullTripleList

-- Combine two return values
append :: JCRet a -> JCRet a -> JCRet a
append = TL.append

-- Combine the first two lists, and inject a value into the third
append2 :: JCRet a -> JCRet b -> (JCRet a -> JCRet b -> c) -> JCRet c
append2 jcr1 jcr2 f =
  let c = f jcr1 jcr2
  in mkJCRet ( TL.first jcr1  ++ TL.first jcr2 )
             ( TL.second jcr1 ++ TL.second jcr2 )
             [c]

-- Update the third component of a return value
updateThird :: JCRet a -> [x] -> JCRet x
updateThird = TL.updateThird
