-- Copyright (c) Peter Duncan White, 2003
module Locale
    ( -- Requests
      Req (..)           -- Type for mvar operation requests
    , MV.MVarId          -- MVarId re-exported
    , MV.name            -- Get the name of an MVarId
    , reqMVarTid         -- Get the tid of a request
    , mkTakeReq          -- Make a take request
    , mkPutReq           -- Make a put request
      -- Responses
    , Rsp (..)           -- Responses to MVar requests
      -- Relays and Locales
    , Relay (..)         -- Relay for MVar operations
    , Locale (..)        -- Upward and downward relays
    , Direction (..)     -- Direction of a request or response
      -- Locale operations
    , addLocales         -- Add two locales together.
    , addReq             -- Add request of specified direction
    , addRsp             -- Add response of specified direction
    , localeNull         -- Determine if a locale is nullx
    , hasUpward          -- Determine if locale has upward req or rsp
    , hasDownward        -- Determine if locale has downward req or rsp
      -- Local relay
    , takeLocalReqs      -- Take the local requests from a locale
    , takeLocalRsps      -- Take the local responses from a locale
    , getFirstLocalRsp   -- Get the first local response, if any
    , addLocalReqs       -- Add local requests
    , addLocalRsps       -- Add local responses
      -- -- Upward relay
    , nullUpwardRelay    -- Null out the upward relay
    , nullDownwardRelay  -- Null out the downward relay
      -- -- Upward bound requests
    , nullUpwardReqs     -- Null out the upward bound requests
    , addUpwardReq       -- Add an upward bound request
    , addUpwardReqs      -- Add upward bound requests
    , isUpwardReq        -- Check if there is upward bound request
    , upwardReqs         -- Get the upward bound requests
    , setUpwardReqs      -- Set upward requests to specified value
      -- -- Downward bound requests
    , nullDownwardReqs   -- Null out the downward bound requests
    , addDownwardReq     -- Add an downward bound request
    , isDownwardReq      -- Check if there is downward bound request
    , downwardReqs       -- Get the downward bound requests
      -- -- Upward bound responses
    , setUpwardRsps      -- Set upward responses to specified value
    , nullUpwardRsps     -- Null out the upward bound requests
    , addUpwardRsp       -- Add an upward bound request
    , isUpwardRsp        -- Check if there is upward bound request
    , upwardRsps         -- Get the upward bound requests
      -- -- Downward bound responses
    , nullDownwardRsps   -- Null out the downward bound requests
    , addDownwardRsp     -- Add an downward bound request
    , isDownwardRsp      -- Check if there is downward bound request
    , downwardRsps       -- Get the downward bound requests
      -- -- Globalization and localization support
    , raise              -- Raise upward bound responses
    , lower              -- Lower downward bound responses
    , localize           -- Localize requests from enclosing braid to local th
    , globalize          -- Localize requests from enclosing braid to local th
    ) where

-- Haskell imports
import Dynamic (Dynamic)
import List (partition)
-- Utility imports
import Null
-- Resumption imports
-- import Unsafe
import qualified Ex as E
-- Local imports
import ThreadId
import qualified BraidMVar as MV

----------------------------------------------------------------------
-- This file implements the locale concept, which supports the flow
-- of requests and responses through the layered braid.  The layered
-- braid in turn implements the process / domain model of Osker.
--
-- The locale has local operations, upward bound operations, and
-- downward bound operations. Operations implement braid features
-- such as MVars, exceptions, and delays.
--
-- The main part of the implementation is MVars, described below.
--
-- MVar operations are abstracted to data items, so that they can
-- be stored in a local state. This makes it possible for MVar
-- operations to be purely local operations, until there is a pause
-- in the thread. At the pause, the braid will examine the local
-- state to see what MVar operations are needed, and perform them,
-- subject to policy constraints.
--
-- The mvarid determines the level of the MVar. When the MVarId
-- (which is a tid) has a tid that is equal to the local tid, then
-- the MVar is a local MVar. When the MVar has a tid that is a
-- prefix of the local tid, then the MVar exists at a higher level.
-- These are the only two possibilities, otherwise MVars could be
-- created in domains or processes with which no communication is
-- allowed. We say that the MVar is located in the braid identified
-- by the tid of the MVarId.
--
-- MVars are always created at the local level. There is no
-- waiting on a newMVar operation.
--
-- The value of the MVar is kept in the braid containing the MVar.
-- There is no value storage in the thread itself.
--
-- Only the basic MVar ops are implemented, other ops, such as
-- modify, with, swap, and read need not be represented here, since
-- they are implemented in terms of put and take, so they will appear
-- as a sequence of put and take operations.
--
-- Since MVars of any type are just MVarIds, and since MVarIds are
-- not polymorphic, the polymorphism can be removed from the MVarOp
-- type be using MVarIds. These MVarIds are local MVarIds, which
-- are converted by the braid to global (braid-wide) MVarIds.
--
-- MVars use dynamic types, so the value to be stored is already
-- converted to dynamic in this representation.
--
-- An MVar op can be waited on, or not. When it is waited on, the
-- waiting thread stops all activity until the MVar op is completed,
-- from the higher levels (where the MVar is located).
----------------------------------------------------------------------

-- The multi-level requests for operations
data Req
    = NewEmptyReq { reqId   :: MV.MVarId
                  , reqTid :: ThreadId
                  }
    | NewReq      { reqId   :: MV.MVarId
                  , reqTid  :: ThreadId
                  , reqVal  :: Dynamic
                  }
    | TakeReq     { reqId  :: MV.MVarId
                  , reqTid  :: ThreadId
                  }
    | PutReq      { reqId  :: MV.MVarId
                  , reqTid :: ThreadId
                  , reqVal :: Dynamic
                  }
    | ThrowToReq  { reqTid :: ThreadId     -- From whom
                  , reqExp :: E.Exception  -- What exception
                  , reqTo  :: ThreadId     -- To whom
                  }
    | ThrowReq    { reqTid :: ThreadId     -- From whom
                  , reqExp :: E.Exception  -- What exception
                  }

instance Show Req where
    show ( NewEmptyReq { reqId = r } )               = "NE " ++ show r
    show ( NewReq      { reqId = r, reqVal = val } ) =
       "N " ++ show r ++ " = " ++ show val
    show ( TakeReq     { reqId = r, reqTid = t   } ) =
       "T " ++ show r ++ " / " ++ show t
    show ( PutReq      { reqId = r, reqTid = p, reqVal = val } ) =
       "P " ++ show r ++ " / " ++ show p ++ "=" ++ show val
    show ( ThrowReq    { reqTid = t, reqExp = e } ) =
       "E " ++ show t ++ " / " ++ show e
    show ( ThrowToReq  { reqTid = t, reqExp = e } ) =
       "E " ++ show t ++ " / " ++ show e

-- Each request has a target
reqTarget :: Req -> ThreadId
reqTarget ( NewEmptyReq { reqId = r } ) = MV.tid r
reqTarget ( NewReq { reqId = r } )      = MV.tid r
reqTarget ( PutReq { reqId = r } )      = MV.tid r
reqTarget ( TakeReq { reqId = r } )     = MV.tid r
reqTarget ( ThrowReq { reqTid = t } )   = t
reqTarget ( ThrowToReq { reqTid = t } ) = t

-- Get the tid of an MVar in an MVar request
reqMVarTid :: Req -> ThreadId
reqMVarTid = MV.tid . reqId

-- Make a take request
mkTakeReq :: MV.MVarId -> ThreadId -> Req
mkTakeReq = TakeReq

mkPutReq :: MV.MVarId -> ThreadId -> Dynamic -> Req
mkPutReq = PutReq

-- The MVar responses
data Rsp
    = NewEmptyRsp { rspTid :: ThreadId, rspId :: MV.MVarId }
    | NewRsp      { rspTid :: ThreadId, rspId :: MV.MVarId }
    | TakeRsp     { rspTid :: ThreadId, rspId :: MV.MVarId, rspVal :: Dynamic }
    | PutRsp      { rspTid :: ThreadId, rspId :: MV.MVarId }
    | ThrowRsp    { rspTid :: ThreadId }
    | ThrowToRsp  { rspTid :: ThreadId }

instance Show Rsp where
    show ( NewEmptyRsp { rspId = r } ) = "NE " ++ show r
    show ( NewRsp      { rspId = r } ) = "N " ++ show r
    show ( TakeRsp     { rspTid = t, rspId = r, rspVal = v } ) =
      "T " ++ show t ++ "," ++ show r ++ "," ++ show v
    show ( PutRsp      { rspTid = t, rspId = r } ) =
      "P " ++ show t ++ "," ++ show r
    show ( ThrowRsp    { rspTid = t } ) = "T " ++ show t
    show ( ThrowToRsp  { rspTid = t } ) = "TT " ++ show t

-- Each response has a target
rspTarget :: Rsp -> ThreadId
rspTarget ( NewEmptyRsp { rspId = r } ) = MV.tid r
rspTarget ( NewRsp { rspId = r } )      = MV.tid r
rspTarget ( PutRsp { rspId = r } )      = MV.tid r
rspTarget ( TakeRsp { rspId = r } )     = MV.tid r
rspTarget ( ThrowRsp { rspTid = t } )   = t
rspTarget ( ThrowToRsp { rspTid = t } ) = t

-- Raise a predicate on MVarIds to a predicate on Rsp
liftPRsp :: (ThreadId -> Bool) -> (Rsp -> Bool)
liftPRsp p = p . rspTarget

-- Raise a predicate on MVarIds to a predicate on Rsp
liftPReq :: (ThreadId -> Bool) -> (Req -> Bool)
liftPReq p = p . reqTarget

-- A relay for MVar operations, which is a pair of queues,
-- one for requests, and one for responses
data Relay = Relay { requests  :: [Req], responses :: [Rsp] }

instance Show Relay where
  show mvop = "(? " ++ show (requests mvop) ++
              ",! " ++ show (responses mvop) ++ ")"

instance Null Relay where
    mkNull = Relay { requests = mkNull, responses = mkNull }

-- Null the requests
nullRequests :: Relay -> Relay
nullRequests relay = relay { requests = mkNull }

-- Null the responses
nullResponses :: Relay -> Relay
nullResponses relay = relay { responses = mkNull }

-- Determine if a relay is empty
relayNull :: Relay -> Bool
relayNull r = null (requests r) && null (responses r)

-- Set the responses to a value
setRequests :: [Req] -> Relay -> Relay
setRequests reqs relay = relay { requests = reqs }

-- Set the responses to a value
setResponses :: [Rsp] -> Relay -> Relay
setResponses rsps relay = relay { responses = rsps }

-- Add two relays together
addRelays :: Relay -> Relay -> Relay
addRelays a b = Relay { requests  = requests a ++ requests b
                      , responses = responses a ++ responses b
                      }

-- Add a request to the relay
addReqToRelay :: Req -> Relay -> Relay
addReqToRelay req relay@(Relay { requests = reqs }) =
  relay { requests = req:reqs }

-- Add a request to the relay
addReqsToRelay :: [ Req ] -> Relay -> Relay
addReqsToRelay [] relay = relay
addReqsToRelay [req] relay@(Relay { requests = reqs }) =
  seq reqs (relay { requests = req:reqs })
addReqsToRelay reqs relay@(Relay { requests = reqs' }) =
  seq reqs' (relay { requests = reqs ++ reqs' })

-- Add a response to the relay
addRspToRelay :: Rsp -> Relay -> Relay
addRspToRelay rsp relay@(Relay { responses = rsps }) =
  relay { responses = rsp:rsps}

-- Add lots of responses to the relay
addRspsToRelay :: [Rsp] -> Relay -> Relay
addRspsToRelay [] relay = relay
addRspsToRelay [rsp] relay@(Relay { responses = rsps }) =
  seq rsps (relay { responses = rsp:rsps })
addRspsToRelay rsps relay@(Relay { responses = rsps' }) =
  seq rsps' (relay { responses = rsps ++ rsps' })

-- A locale has a relay for upward bound MVar operations and downward
-- bound MVar operations. The locale also keeps the operations for
-- MVars at the local level.
data Locale = Locale { upward   :: Relay
                     , downward :: Relay
                     , local    :: Relay
                     }

instance Show Locale where
    show mvol = "{U " ++ show (upward mvol) ++
                ",D " ++ show (downward mvol) ++ 
                ",L " ++ show (local mvol) ++ "}"

instance Null Locale where
    mkNull = Locale { upward=mkNull, downward=mkNull, local=mkNull }

data Direction = Up | Down | Local deriving (Eq, Ord, Enum, Show)

----------------------------------------------------------------------
--  Locale level operations
----------------------------------------------------------------------
addLocales :: Locale -> Locale -> Locale
addLocales a b = Locale { upward   = addRelays (upward a) (upward b)
                        , downward = addRelays (downward a) (downward b)
                        , local    = addRelays (local a) (local b)
                        }

-- Add a request of the specified direction
addReq :: Direction -> Req -> Locale -> Locale
addReq Up req loc    = addUpwardReq req loc
addReq Down req loc  = addDownwardReq req loc
addReq Local req loc = addLocalReqs [req] loc

-- Add a response of the specified direction
addRsp :: Direction -> Rsp -> Locale -> Locale
addRsp Up rsp loc    = addUpwardRsp rsp loc
addRsp Down rsp loc  = addDownwardRsp rsp loc
addRsp Local rsp loc = addLocalRsps [rsp] loc

-- Determine if a locale has any upward requests or responses
hasUpward :: Locale -> Bool
hasUpward = not . relayNull . upward

-- Determine if a locale has any downward requests or responses
hasDownward :: Locale -> Bool
hasDownward = not . relayNull . downward

-- Determine if a locale has any local requests or responses
hasLocal :: Locale -> Bool
hasLocal = not . relayNull . local

-- Determine if a locale is null
localeNull :: Locale -> Bool
localeNull l = not (hasUpward l || hasDownward l || hasLocal l)

----------------------------------------------------------------------
--  Upward relay
----------------------------------------------------------------------
nullUpwardRelay :: Locale -> Locale
nullUpwardRelay locale = locale { upward = mkNull }

----------------------------------------------------------------------
--  Upward relay
----------------------------------------------------------------------
nullDownwardRelay :: Locale -> Locale
nullDownwardRelay locale = locale { downward = mkNull }

----------------------------------------------------------------------
--  Upward bound requests
----------------------------------------------------------------------
-- Set the upward responses to specified value
setUpwardReqs :: [Req] -> Locale -> Locale
setUpwardReqs rsps loc@(Locale { upward = u }) =
  loc { upward = setRequests rsps u }

-- Null out the upward requests
nullUpwardReqs :: Locale -> Locale
nullUpwardReqs loc@(Locale { upward = u }) = loc { upward = nullRequests u }

-- Add an upward bound request
addUpwardReq :: Req -> Locale -> Locale
addUpwardReq req loc@(Locale { upward = u }) =
  loc { upward = addReqToRelay req u }

-- Add an upward bound request
addUpwardReqs :: [ Req ] -> Locale -> Locale
addUpwardReqs reqs loc@(Locale { upward = u }) =
  seq u (loc { upward = addReqsToRelay reqs u })

-- Check if there is an upward bound request
isUpwardReq :: Locale -> Bool
isUpwardReq = not . null . requests . upward

-- Get the upward requests
upwardReqs :: Locale -> [Req]
upwardReqs = requests . upward

-- Split the upward reesponsed into those that have found their
-- home, and will become local, and those that need to continue
-- their journey upward
getUpwardReqs :: (Req -> Bool) -> Locale -> ([Req], [Req])
getUpwardReqs p = (partition p) . upwardReqs

----------------------------------------------------------------------
--  Downward bound requests
----------------------------------------------------------------------
-- Set downward responses to specified value
setDownwardReqs :: [Req] -> Locale -> Locale
setDownwardReqs reqs loc@(Locale { downward = d }) =
  loc { downward = setRequests reqs d }

-- Null out the downward requests
nullDownwardReqs :: Locale -> Locale
nullDownwardReqs loc@(Locale { downward = d}) =
  loc { downward = nullRequests d }

-- Add a downward bound request
addDownwardReq :: Req -> Locale -> Locale
addDownwardReq req loc@(Locale { downward = d }) =
  loc { downward = addReqToRelay req d }

-- Add downward bound requests
addDownwardReqs :: [ Req ] -> Locale -> Locale
addDownwardReqs reqs loc@(Locale { downward = d}) =
  seq d ( loc { downward = addReqsToRelay reqs d } )

-- Check if there is an downward bound request
isDownwardReq :: Locale -> Bool
isDownwardReq = not . null . requests . upward

-- Get the downward requests
downwardReqs ::  Locale -> [Req]
downwardReqs = requests . downward

-- Get the downward responses that satisfy a predicate
-- getDownwardReqs :: (Req -> Bool) -> Locale -> ([Req], [Req])
-- getDownwardReqs p = (partition p) . downwardReqs

----------------------------------------------------------------------
--  Upward bound responses
----------------------------------------------------------------------
-- Set the upward responses to specified value
setUpwardRsps :: [Rsp] -> Locale -> Locale
setUpwardRsps rsps loc@(Locale { upward = u }) =
  seq u (loc { upward = setResponses rsps u })

-- Null out the upward responses
nullUpwardRsps :: Locale -> Locale
nullUpwardRsps = setUpwardRsps []

-- Add an upward bound response
addUpwardRsp :: Rsp -> Locale -> Locale
addUpwardRsp rsp loc@(Locale { upward = u }) =
  loc { upward = addRspToRelay rsp u }

-- Add upward bound responses
addUpwardRsps :: [Rsp] -> Locale -> Locale
addUpwardRsps rsps loc@(Locale { upward = u }) =
  seq u (loc { upward = addRspsToRelay rsps u })

-- Check if there is an upward bound response
isUpwardRsp :: Locale -> Bool
isUpwardRsp = not . null . responses . upward

-- Get the upward responses
upwardRsps :: Locale -> [Rsp]
upwardRsps = responses . upward

-- Get the upward responses that satisfy a predicate
getUpwardRsps :: (Rsp -> Bool) -> Locale -> ([Rsp], [Rsp])
getUpwardRsps p = (partition p) . upwardRsps

----------------------------------------------------------------------
--  Downward bound responses
----------------------------------------------------------------------
-- Set downward responses to specified value
setDownwardRsps :: [Rsp] -> Locale -> Locale
setDownwardRsps rsps loc@(Locale { downward = d }) =
  loc { downward = setResponses rsps d }

-- Null the downward responses
nullDownwardRsps :: Locale -> Locale
nullDownwardRsps = setDownwardRsps []

-- Add a downward bound response
addDownwardRsp :: Rsp -> Locale -> Locale
--addDownwardRsp rsp loc = loc { downward = addRspToRelay rsp (downward loc) }
addDownwardRsp rsp loc@(Locale { downward = d }) =
  loc { downward = addRspToRelay rsp d }

-- Add a downward bound response
addDownwardRsps :: [Rsp] -> Locale -> Locale
addDownwardRsps rsps loc@(Locale { downward = d }) =
  seq d (loc { downward = addRspsToRelay rsps d })

-- Check if there is an downward bound response
isDownwardRsp :: Locale -> Bool
isDownwardRsp = not . null . responses . downward

-- Get all the downward responses
downwardRsps :: Locale -> [Rsp]
downwardRsps = responses . downward

-- Get the downward responses that satisfy a predicate
-- getDownwardRsps :: (Rsp -> Bool) -> Locale -> ([Rsp], [Rsp])
-- getDownwardRsps p = (partition p) . downwardRsps

----------------------------------------------------------------------
--  Local MVar operations
----------------------------------------------------------------------

-- Set local responses to the specified value
setLocalResponses :: [Rsp] -> Locale -> Locale
setLocalResponses rsps loc@(Locale { local = l }) =
  seq l (loc { local = setResponses rsps l })

-- Set local requests to the specified value
-- setLocalRequests :: [Req] -> Locale -> Locale
-- setLocalRequests reqs loc@(Locale { local = l }) =
--   loc { local = setRequests reqs l }

-- Null out the local requests
-- nullLocalReqs :: Locale -> Locale
-- nullLocalReqs locale = locale { local = nullRequests (local locale) }

-- Remove the local operations
-- takeLocal :: Locale -> (Locale, Relay)
-- takeLocal locale = ( locale { local = mkNull }, local locale )

-- Get local requests
takeLocalReqs :: Locale -> (Locale, [Req])
takeLocalReqs loc@(Locale { local = l }) =
  ( loc { local = nullRequests l }, requests l )

-- Get local responses
takeLocalRsps :: Locale -> (Locale, [Rsp])
takeLocalRsps loc =
  ( loc { local = nullResponses (local loc) }, responses ( local loc ) )

-- Add an upward bound request
addLocalReqs :: [Req] -> Locale -> Locale
addLocalReqs reqs loc@(Locale { local = l }) =
  loc { local = addReqsToRelay reqs l }

-- Add an upward bound response
addLocalRsps :: [Rsp] -> Locale -> Locale
addLocalRsps rsps loc@(Locale { local = l }) =
  loc { local = addRspsToRelay rsps l }

-- Get first local response
getFirstLocalRsp :: Locale -> (Locale, Maybe Rsp)
getFirstLocalRsp loc@(Locale { local = l }) =
  if null (responses l)
  then ( loc, Nothing )
  else ( setLocalResponses (tail (responses l)) loc
       , Just (head (responses l))
       )

----------------------------------------------------------------------
--  In support of globalization and localization
----------------------------------------------------------------------

-- Raise the requests and responses from a local (lifted) thread
-- into the braid. Those requests and responses that are for MVars
-- at the level of the braid will be placed in the local operations
-- relay, while upward bound requests and responses will be placed
-- in the upward bound relay.
globalize ::
    ThreadId       -> -- Thread id of lifted thread
    Locale         -> -- Lower locale (lifted thread)
    Locale         -> -- Upper locale (braid)
    ( Locale          -- New value for lower (down) locale
    , Locale          -- New value for upper (up) locale
    , [Req]           -- List of local requests
    )
globalize tid down up =
  let p = atLevel tid
      (lrsps, uprsps) = getUpwardRsps (liftPRsp p) down
      (lreqs, upreqs) = getUpwardReqs (liftPReq p) down
      ret = ( mkNull
            , ( setLocalResponses lrsps .
                addUpwardRsps uprsps . addUpwardReqs upreqs ) up
            , lreqs
            )
  in ret

-- Raise the responses from an encapsulated braid, to the level of
-- the encapsulating local thread. Raise also the upward bound
-- requests. This supports twisting a braid into a local thread.
-- Since the down locale is nulled, it is not returned.
raise :: Locale -> Locale -> Locale
raise down up =
  ( addUpwardRsps (upwardRsps down) . addUpwardReqs (upwardReqs down) ) up

-- Lower the requests that satisfy a predicate to the down locale,
-- used in support of localization
localize ::
    ThreadId    -> -- Localize to this thread Id.
    Locale      -> -- Lower locale
    Locale      -> -- Upper locale
    ( Locale       -- Resulting lower locale
    , Locale       -- Resulting upper locales
    )
localize tid down up =
  let (downrsps, notdownrsps) = partition isSubRsp (downwardRsps up)
      (locrsps,  notlocrsps)  = partition eqRsp downrsps
      (downreqs, notdownreqs) = partition isSubReq (downwardReqs up)
      (locreqs,  notlocreqs)  = partition atLevReq downreqs
      down' = ( addDownwardRsps notlocrsps . addDownwardReqs notlocreqs .
                addLocalRsps locrsps . addLocalReqs locreqs ) down
      up'   = ( setDownwardRsps notdownrsps . setDownwardReqs notdownreqs ) up
      res = ( down', up' )
  in res
  where isSubRsp = isSub tid . rspTid
--        atLevRsp = atLevel tid . rspTid
        eqRsp    = \rsp -> tid == rspTid rsp
        isSubReq = isSub tid . reqTid
        atLevReq = atLevel tid . reqTid

-- Lower the requests that satisfy a predicate to the down locale,
-- used in support of twisting. Since the upper locale is nulled,
-- it is not returned.
lower ::
    Locale              -> -- Lower locale
    Locale              -> -- Upper locale
    Locale                 -- Resulting lower locale
lower down up =
  let downrsps = downwardRsps up
      downreqs = downwardReqs up
      res = ( addDownwardRsps downrsps . addDownwardReqs downreqs ) down
  in res
--     if not (null downrsps)
--     then unsafeRet ( "<<< MVO.lower: " ++ show downrsps ) res
--     else res

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE reqMVarTid         #-}
{-# INLINE addLocales         #-}
{-# INLINE addRelays          #-}
{-# INLINE liftPRsp           #-}
{-# INLINE liftPReq           #-}
{-# INLINE nullRequests       #-}
{-# INLINE nullResponses      #-}
{-# INLINE setResponses       #-}
{-# INLINE setRequests        #-}
{-# INLINE addReq             #-}
{-# INLINE addReqToRelay      #-}
{-# INLINE addReqsToRelay     #-}
{-# INLINE addRsp             #-}
{-# INLINE addRspToRelay      #-}
{-# INLINE addRspsToRelay     #-}
{-# INLINE nullUpwardReqs     #-}
{-# INLINE addUpwardReq       #-}
{-# INLINE addUpwardReqs      #-}
{-# INLINE addLocalReqs       #-}
{-# INLINE addLocalRsps       #-}
{-# INLINE isUpwardReq        #-}
{-# INLINE upwardReqs         #-}
{-# INLINE nullDownwardReqs   #-}
{-# INLINE addDownwardReq     #-}
{-# INLINE isDownwardReq      #-}
{-# INLINE downwardReqs       #-}
{-# INLINE setUpwardRsps      #-}
{-# INLINE setUpwardReqs      #-}
{-# INLINE nullUpwardRsps     #-}
{-# INLINE addUpwardRsp       #-}
{-# INLINE addUpwardRsps      #-}
{-# INLINE isUpwardRsp        #-}
{-# INLINE upwardRsps         #-}
{-# INLINE setDownwardRsps    #-}
{-# INLINE nullDownwardRsps   #-}
{-# INLINE addDownwardRsp     #-}
{-# INLINE addDownwardRsps    #-}
{-# INLINE isDownwardRsp      #-}
{-# INLINE downwardRsps       #-}
-- {-# INLINE getDownwardRsps    #-}
-- {-# INLINE getDownwardReqs    #-}
{-# INLINE localeNull         #-}
{-# INLINE hasUpward          #-}
{-# INLINE hasDownward        #-}
{-# INLINE hasLocal           #-}
{-# INLINE relayNull          #-}
{-# INLINE setLocalResponses  #-}
-- {-# INLINE setLocalRequests   #-}
{-# INLINE lower              #-}
{-# INLINE localize           #-}
{-# INLINE globalize          #-}
{-# INLINE raise              #-}
{-# INLINE nullUpwardRelay    #-}
{-# INLINE nullDownwardRelay  #-}
{-# INLINE takeLocalReqs      #-}
{-# INLINE takeLocalRsps      #-}
