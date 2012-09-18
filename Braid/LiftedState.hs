-- Copyright (c) Peter Duncan White, 2003
module LiftedState
    ( -- Lifted state, transformer and abserver
      LiftedState (..)   -- State local to a lifted thread
    , mkLiftedState      -- Constructor for lifted state
    , liftSt             -- Lift transformer of ls to (LiftedState ls)
    , lifta              -- Lift transerver of ls to (LiftedState ls)
    , Transformer        -- Lifted state transformer
    , Observer           -- Observer of lifted state
    , Transerver         -- Transformer and Observer together
    , updateLiftedState  -- Update the ls part of the lifted state
      -- Lifted MVar operations
    , newMVar            -- Lifted version of MVar op, state transformer
    , newEmptyMVar       -- Lifted version of MVar op, state transformer
    , takeMVar           -- Lifted version of MVar op, state transformer
    , putMVar            -- Lifted version of MVar op, state transformer
    , Putting (..)       -- Information about a put operation
    , Taking (..)        -- Information about a take operation
    , Making (..)        -- Information about a newMVar operation
      -- Lifted state manipulations
    , unTake             -- Cancel the take operation
    , made               -- Made a new MVar
    , taken              -- Indicate that MVar has been taken
    , put                -- Indicate that MVar has been put
      -- MVarOp manipulations
    , setLocale          -- Set the whole locale
    , nullDownwardRelay  -- Null the downward relay
    , nullUpwardReqs     -- Null the upward bound requests
    , upwardReqs         -- Get the upward bound requests
    , nullUpwardRsps     -- Null the upward bound responses
    , takeUpwardReqs     -- Get upward requests, and null them
    , setUpwardRsps      -- Set upward responses to specified value
    , localeNull         -- Determine if the locale is null
      -- In support of raise and lower
    , lower              -- Lower from lifted thread to twisted braid
    , raise              -- Raise from twisted braid to lifted thread
    , globalize          -- Raise from lifted thread to braid
    , localize           -- Make a response local
      -- MVar exports
    , L.Req (..)         -- Lifted MVar operation requests
    , L.Rsp (..)         -- Lifted MVar operation requests
    , MV.MVarId          -- Lifted MVarId
    , MV.name            -- Get the name of an MVar
    , MV.tid             -- Get the tid of an MVar
    , TID.ThreadId       -- Re-export definition
      -- Internal implementation of thread delay
    , threadDelay        -- Lifted version of thread delay
    , nullDelay          -- Null out the delay count
    , takeDelay          -- Take the delay count, and null it.
      -- In support of exceptions
    , Catcher            -- Exception catcher type
    , takeFirstCatcher   -- Get the outermost exception catcher
    , addCatcher         -- Add a thread catcher
    , throwTo            -- Add a remote throw request
    , throw              -- Add a local throw request
    ) where

----------------------------------------------------------------------
--
-- The lifted state of a single thread.
-- It always includes a list of MVar operations, which are done
-- locally before they are globalized. This means that MVar ops are
-- local operations, until the braid thread takes a pause. At this
-- time, the braid has the opportunity to examine the lifted state,
-- and globalize the lifted MVar operations.
--
-- Each lifted thread has its own MVar Id generator, so that MVar Ids
-- referenced are local, and there is no covert channel regarding
-- MVar Ids.
--
-- The mvarOp field allows only one lifted MVar Op at a time. It is
-- expected that each MVar op will cause a pause, and the braid will
-- remove the MVar Op from the lifted state during that pause, so
-- there should be only one MVarOp at a time.
--
----------------------------------------------------------------------

-- Haskell imports
import Maybe
import Dynamic (Dynamic)
-- Utility imports
import Null
-- import Unsafe
-- Resumption imports
import qualified Ex as E
-- Local imports
import qualified Locale as L
import qualified MVarId as MVID
import qualified ThreadId as TID
import qualified BraidMVar as MV

-- The information required of a lifted thread waiting to take an MVar
data Taking
    = NotTaking           -- No take operation
    | Taking MVID.MVarId  -- Take operation in progress
    | Taken Dynamic       -- Take operation complete
    deriving ( Show )

instance Null Taking where
    mkNull = NotTaking

-- The information required of a lifted thread waiting to put an MVar
data Putting
    = NotPutting                  -- No put operation
    | Putting MVID.MVarId MV.MVal -- Putting operation in progress
    | Put MVID.MVarId             -- Put operation complete
    deriving (Show)

instance Null Putting where
    mkNull = NotPutting

-- The information required of a lifted thread making a new MVar
data Making
    = NotMaking
    | Making String (Maybe Dynamic)
    | Made MVID.MVarId
      deriving (Show)

instance Null Making where
    mkNull = NotMaking

-- The lifted state, which includes support for lifted MVar operations
data LiftedState ls t =
    LiftedState
    { liftedName  :: String        -- Used in debugging
    , liftedState :: ls            -- The parameterized information
    , liftedId    :: TID.ThreadId  -- The lifted thread Id
      -- In support of MVar operations
    , locale      :: L.Locale      -- MVar ops to be globalized
    , taking      :: Taking        -- Information for taking an MVar
    , putting     :: Putting       -- Information for putting an MVar
    , making      :: Making        -- Information for making an MVar
      -- In support of threadDelay
    , delayCount  :: Maybe Int     -- If the thread wants a delay
      -- In support of exception handling
    , catchers    :: [ Catcher t ] -- List of nested exception catchers
    }

-- A thread catcher
type Catcher t = E.Exception -> t

-- A show instance for the lifted state.
instance Show (LiftedState ls t) where
    show ls = "\n\t\t<<<" ++ liftedName ls ++ "," ++ show (liftedId ls) ++
              "," ++ show (locale ls) ++ ",DC=" ++ show (delayCount ls) ++
              "," ++ show (taking ls) ++ "," ++ show (putting ls) ++ ">>>"

-- Update the lifted state
updateLiftedState :: ls -> Transformer ls t
updateLiftedState ls s = s { liftedState = ls }

-- A constructor for LiftedState
mkLiftedState :: (Null ls) => String -> TID.ThreadId -> LiftedState ls t
mkLiftedState name tid = LiftedState { liftedName  = name
                                     , liftedState = mkNull
                                     , liftedId    = tid
                                     , locale      = mkNull
                                     , taking      = mkNull
                                     , putting     = mkNull
                                     , making      = mkNull
                                     , delayCount  = mkNull
                                     , catchers    = mkNull
                                     }

-- A transformer on the lifted state
type Transformer ls t  = LiftedState ls t -> LiftedState ls t
type Observer ls t a   = LiftedState ls t -> a
type Transerver ls t a = LiftedState ls t -> (LiftedState ls t, a)

-- Lift a transformer of the internal lifted state (ls) to
-- a transformer of the lifted state.
liftSt :: (ls -> ls) -> Transformer ls t
liftSt f ls = ls { liftedState = f (liftedState ls) }

-- Lift a transerver of the internal lifted state (ls) to
-- a transformer of the lifted state.
lifta :: (ls -> (ls, a)) -> Transerver ls t a
lifta f ls = let (ls', a) = f ( liftedState ls )
             in (ls { liftedState = ls' }, a)

instance (Null ls) => Null (LiftedState ls t) where
    mkNull = mkLiftedState mkNull mkNull

----------------------------------------------------------------------
-- MVar operations at the lifted level
----------------------------------------------------------------------

-- Set the locale
setLocale :: L.Locale -> Transformer ls t
setLocale loc ls = ls { locale = loc }

-- Null the locale
nullLocale :: Transformer ls t
nullLocale ls = setLocale mkNull ls

-- Null the downward relay
nullDownwardRelay :: Transformer ls t
nullDownwardRelay ls@(LiftedState { locale = l }) =
  ls { locale = (L.nullDownwardRelay l) }

-- Set the upward responses
setUpwardRsps :: [ L.Rsp ] -> Transformer ls t
setUpwardRsps rsps ls@(LiftedState { locale = l }) =
  ls { locale = L.setUpwardRsps rsps l }

-- Null out the upward responses
nullUpwardRsps :: Transformer ls t
nullUpwardRsps = setUpwardRsps []

-- Remove all the mvar ops
nullUpwardReqs :: Transformer ls t
nullUpwardReqs ls@(LiftedState { locale = l }) =
  ls { locale = L.nullUpwardReqs l }

-- take the upward requests out, and null them.
takeUpwardReqs :: Transerver ls t [ L.Req ]
takeUpwardReqs ls = ( nullUpwardReqs ls, upwardReqs ls )

-- Add an upward bound Req to the lifted state
addUpwardReq :: L.Req -> Transformer ls t
addUpwardReq req ls@(LiftedState { locale = l }) =
  ls { locale = L.addUpwardReq req l }

-- Add a downward bound Req to the lifted state
addDownwardReq :: L.Req -> Transformer ls t
addDownwardReq req ls@(LiftedState { locale = l }) =
  ls { locale = L.addDownwardReq req l }

-- Add an upward bound Req to the lifted state
-- addLocalReqs :: [ L.Req ] -> Transformer ls t
-- addLocalReqs reqs ls@(LiftedState { locale = l }) =
--   ls { locale = L.addLocalReqs reqs l }

-- Get the upward bound requests
upwardReqs :: Observer ls t [ L.Req ]
upwardReqs = L.upwardReqs . locale

-- Determine if the locale in the state is null
localeNull :: Observer ls t Bool
localeNull ls = L.localeNull (locale ls)

-- Turn the MVarOp lower operator into a lifted state transerver
lower ::
    L.Locale              -> -- The locale to which MVars will be lowered
    Transerver ls t L.Locale -- Transforms the lifted state.
lower down ls = ( nullLocale ls, L.lower down (locale ls) )

-- Turn the MVarOp raise operator into a lifted state transerver
raise ::
    L.Locale         -> -- The locale to which MVars will be lowered
    Transformer ls t    -- Transforms the lifted state.
raise down ls = setLocale (L.raise down (locale ls)) ls

-- Raise operations from lifted thread to braid
globalize :: L.Locale -> Transerver ls t (Maybe Int)
globalize down = takeDelay . setLocale down

-- Localize a response from a downward relay in the enclosing braid
-- The response is moved out of the local portion of the locale, and
-- placed in the taking / putting variables.
localize :: L.Rsp -> Transformer ls t
localize (L.NewEmptyRsp { L.rspId = rid }) ls =
  error ( "L.localizeRsp / NewEmptyRsp: " ++ show rid ++ show ls )
localize (L.NewRsp { L.rspId = rid }) ls =
  error ( "L.localizeRsp / NewEmptyRsp: " ++ show rid ++ show ls )
localize (L.ThrowRsp { L.rspTid = tid }) ls =
  error ( "L.localizeRsp / ThrowRsp: " ++ show tid ++ show ls )
localize (L.ThrowToRsp { L.rspTid = tid }) ls =
  error ( "L.localizeRsp / ThrowToRsp: " ++ show tid ++ show ls )
localize (L.TakeRsp { L.rspVal = val }) ls = ls { taking = Taken val }
localize (L.PutRsp { L.rspId = rid }) ls   = ls { putting = Put rid }

----------------------------------------------------------------------
--
-- Lifted versions of the MVar operations, at the level of internal
-- state transformations.
--
----------------------------------------------------------------------

-- Create a new MVar
newMVar :: String -> Dynamic -> Transerver ls t (MV.MVar a)
newMVar name dyn ls =
  let mvid = MVID.mkMVarId (liftedId ls) name
  in ( addUpwardReq ( L.NewReq { L.reqId  = mvid
                               , L.reqTid = liftedId ls
                               , L.reqVal = dyn
                               }
                    ) ls
     , MV.mkMVar mvid
     )

-- Set the makeing variable when it is made
made :: MV.MVarId -> Transformer ls t
made mvid ls = ls { making = Made mvid }

-- Create a new empty MVar
newEmptyMVar :: String -> Transerver ls t (MV.MVar a)
newEmptyMVar name ls =
  let mvid = MVID.mkMVarId (liftedId ls) name
  in ( addUpwardReq ( L.NewEmptyReq { L.reqId  = mvid
                                    , L.reqTid = liftedId ls
                                    }
                    ) ls,
       MV.mkMVar mvid
     )

-- Take a value from an MVar
-- A local defensive check is made regarding the existance of the MVar
takeMVar :: MV.MVarId -> Transformer ls t
takeMVar mvid ls =
  let ls' = addUpwardReq ( L.mkTakeReq mvid (liftedId ls) ) ls
  in ls' { taking = Taking mvid }

-- Cancel the take of the MVar
unTake :: Transformer ls t
unTake ls = ls { taking = NotTaking }

-- Update the taking information
taken :: MV.MVal -> Transformer ls t
taken mval ls =
  case mval of
     Nothing  -> ls
     Just val -> ls { taking = Taken val }

-- Put a value in an MVar
putMVar :: MV.MVarId -> Dynamic -> Transformer ls t
putMVar mvid dyn ls =
  let ls' = addUpwardReq ( L.mkPutReq mvid (liftedId ls) dyn ) ls
  in ls' { putting = Putting mvid (Just dyn) }

-- Update the taking information
put :: MV.MVarId -> Transformer ls t
put mvid ls = ls { putting = Put mvid }

----------------------------------------------------------------------
-- Internal implementation of thread delay
----------------------------------------------------------------------
threadDelay :: Int -> Transformer ls t
threadDelay n ls = ls { delayCount = Just n }

-- Zero out the delay count
nullDelay :: Transformer ls t
nullDelay ls = ls { delayCount = Nothing }

-- Get the delay count, and zero it out
takeDelay :: Transerver ls t ( Maybe Int )
takeDelay ls = ( nullDelay ls, delayCount ls )

----------------------------------------------------------------------
--  Support for exceptions
----------------------------------------------------------------------

-- Return the outermost thread catcher
takeFirstCatcher :: Transerver ls t (Maybe (Catcher t))
takeFirstCatcher ls =
  if null (catchers ls)
  then (ls, Nothing)
  else ( ls { catchers = tail (catchers ls) }
       , Just ( head ( catchers ls ) )
       )

-- Add the outermost exception catcher
addCatcher :: Catcher t -> Transformer ls t
addCatcher catcher ls = ls { catchers = catcher:catchers ls }

-- Add a local exception to the locale
throw :: E.Exception -> Transformer ls t
throw e ls = addUpwardReq ( L.ThrowReq { L.reqTid = liftedId ls
                                       , L.reqExp = e
                                       }
                          ) ls

-- Add a remote exception to the locale
throwTo :: TID.ThreadId -> E.Exception -> Transformer ls t
throwTo tid e ls =
  let req = L.ThrowToReq { L.reqTid = liftedId ls
                         , L.reqExp = e
                         , L.reqTo  = tid
                         }
  in if isSub ls tid
     then addDownwardReq req ls
     else addUpwardReq req ls

-- Determine if thread id is in a sub-braid of this local thread
isSub :: LiftedState ls t -> TID.ThreadId -> Bool
isSub ls tid = TID.isSub (liftedId ls) tid

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE isSub              #-}
{-# INLINE mkLiftedState      #-}
{-# INLINE updateLiftedState  #-}
{-# INLINE setLocale          #-}
{-# INLINE setUpwardRsps      #-}
{-# INLINE nullUpwardRsps     #-}
{-# INLINE nullUpwardReqs     #-}
{-# INLINE addUpwardReq       #-}
{-# INLINE addDownwardReq     #-}
-- {-# INLINE addLocalReqs       #-}
{-# INLINE upwardReqs         #-}
{-# INLINE takeUpwardReqs     #-}
{-# INLINE newMVar            #-}
{-# INLINE made               #-}
{-# INLINE newEmptyMVar       #-}
{-# INLINE takeMVar           #-}
{-# INLINE putMVar            #-}
{-# INLINE taken              #-}
{-# INLINE threadDelay        #-}
{-# INLINE nullDelay          #-}
{-# INLINE takeDelay          #-}
{-# INLINE raise              #-}
{-# INLINE lower              #-}
{-# INLINE nullDownwardRelay  #-}
{-# INLINE nullLocale         #-}
{-# INLINE takeFirstCatcher   #-}
{-# INLINE addCatcher         #-}
{-# INLINE throwTo            #-}
{-# INLINE liftSt             #-}
{-# INLINE lifta              #-}
