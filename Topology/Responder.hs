-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Responder
    ( Respondable (..)      -- Class of messages with responder support
    , Responder             -- Responder abstract type
    , mkResponder           -- Constructor for responder
    , PreResponder          -- Thread with bounds
      -- Methods of the responder
    , respond               -- Respond to a message
    , yield                 -- Take a break
    , myThreadId            -- Get local thread Id
    , putStrLn              -- For debugging
    , putStr                -- For debugging
    , module Channels       -- Re-export channel stuff
    , module Message        -- Re-export message stuff
      -- Client server responders
    , responderShell        -- For servers without internal state
    , serverShell           -- For servers with internal state
    ) where

----------------------------------------------------------------------
--
--  A responder spoke, found at the flange of the hub and spoke model
--  A responder can respond to incoming requests, without needing to
--  know the response channel. Knowledge of the response channel is
--  hidden in the constructors of the Responder.
--
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (IO, putStrLn, putStr)
import Dynamic ( Typeable, TyCon, mkTyCon, mkAppTy, typeOf )
-- Resumption imports
import qualified BoundedMonad as B
-- Braid imports
import ThreadId
import qualified BraidLocal as L
import qualified OskerChan as C
-- Local imports
import Channels
import Message
import qualified Spoke as S

----------------------------------------------------------------------
-- Messages to which a response can be given, without knowing the
-- response channel.
----------------------------------------------------------------------

class (Message m) => Respondable m where
    responseChannel :: m -> C.Chan m

data ClientServerMsg x y
    = ReqMsg { reqPay :: x, rspChan :: C.Chan (ClientServerMsg x y) }
    | RspMsg { rspPay :: y }

instance Message (ClientServerMsg x y)

instance (Show y) => Respondable (ClientServerMsg x y) where
    responseChannel (ReqMsg {rspChan = r}) = r
    responseChannel (RspMsg {rspPay = p}) =
      error ( "responseChannel: " ++ show p )

clientServerCon :: TyCon
clientServerCon = mkTyCon "Client Server Message"
instance Typeable (ClientServerMsg x y) where
    typeOf _ = mkAppTy clientServerCon []


-- The responder bounded Monad
type Responder ls m a = B.BdM (L.Thread ls) (ResponderBounds ls m) a
-- Data required to create a responder
type PreResponder ls m = ResponderBounds ls m -> Responder ls m ()

-- The bounds for a responder thread
data (Respondable m) => ResponderBounds ls m = ResponderBounds
    { -- Single input channel
      inputChannel   :: C.Chan m
    , get            :: Responder ls m m
    , respond        :: m -> m -> Responder ls m ()
    , yield          :: Responder ls m ()
    , myThreadId     :: Responder ls m ThreadId
      -- Debugging interfaces
    , putStrLn       :: String -> Responder ls m ()
    , putStr         :: String -> Responder ls m ()
    }

----------------------------------------------------------------------
-- Create the constructors of the Responder
----------------------------------------------------------------------

-- Get the next message from the single input channel
get' :: (Show ls) => C.Chan m -> Responder ls m m
get' chan = B.mTo ( L.readChan chan )

-- Get the local kernel thread Id
myThreadId' :: Responder ls m ThreadId
myThreadId' = B.mTo L.myThreadId

-- Read an allowed channel
respond' ::(Show ls, Typeable m, Respondable m) =>
    m -> m -> Responder ls m ()
respond' inm outm = B.mTo ( L.writeChan ( responseChannel inm ) outm )

-- Yield the processor for a while
yield' :: Responder ls m ()
yield' = B.mTo L.yield

-- Write a line on standard output (for debugging only)
putStrLn' :: String -> Responder ls m ()
putStrLn' s = B.mTo (L.putStrLn s)

-- Write a string on standard output (for debugging only)
putStr' :: String -> Responder ls m ()
putStr' s = B.mTo (L.putStr s)

-- Construct a bounded IO thread from the bounds given
mkResponderBounds :: (Show ls, Typeable m, Respondable m) =>
    C.Chan m -> ResponderBounds ls m
mkResponderBounds chan =
  ResponderBounds { inputChannel      = chan
                  , get               = get' chan
                  , respond           = respond'
                  , yield             = yield'
                  , myThreadId        = myThreadId'
                  , putStrLn          = putStrLn'
                  , putStr            = putStr'
                  }

-- Make a responder from an input channel and a pre-responder
mkResponder :: (Respondable m, Show ls, Typeable m) =>
    C.Chan m -> PreResponder ls m -> L.Thread ls ()
mkResponder chan preResponder = B.toM rspBounds (preResponder rspBounds)
  where rspBounds = mkResponderBounds chan

----------------------------------------------------------------------
--  Build a responder from a function on ClientServer messages 
----------------------------------------------------------------------

-- A monad for the server in the client-server model
type Server ls x y a = Responder ls (ClientServerMsg x y) a

-- A response function to a single message that does not depend
-- on the internal state of the monad
respondSingle :: (Show y) =>
    (x -> y) -> ClientServerMsg x y -> ClientServerMsg x y
respondSingle f (ReqMsg {reqPay = x}) = RspMsg { rspPay = f x }
respondSingle _f (RspMsg {rspPay = y}) = error ( "respondSingle: " ++ show y )

-- A shell for a responder that does not care about its internal state,
-- i.e. each input message has sufficient data to determine the response
responderShell :: (Show y, Show ls) =>
   String            -> -- Name of this responder shell
   (x -> y)          -> -- Function of a single input message
   L.Thread ls ()       -- Resulting lifted braid thread
responderShell name f =
  do { chan <- L.newChan (name ++ " responder input channel")
     ; let serverBounds = mkResponderBounds chan
     ; responderLoop f serverBounds
     }

-- Helper function for the responder loop
responderLoop :: (Show y) =>
    (x -> y)                                 -> -- Process single message
    ResponderBounds ls (ClientServerMsg x y) -> -- Bounds on the server
    L.Thread ls ()                              -- Resulting lifted thread
responderLoop f serverBounds =
     B.toM serverBounds
           ( do { m <- get serverBounds
                ; respond serverBounds m (respondSingle f m)
                }
           )

-- A response function to a single message that does depend on
-- the internal state of the monad
respondSingleLs :: (Show y) =>
  (x -> ls -> (ls, y))   -> -- The function on state and payload
  ClientServerMsg x y    -> -- The incoming message
  Server ls x y (ClientServerMsg x y) -- The outgoing message
respondSingleLs f (ReqMsg {reqPay = x}) =
  do { y <- B.mTo (L.lifta (f x))
     ; return (RspMsg {rspPay = y})
     }
respondSingleLs _f (RspMsg {rspPay = p}) = error ( "respondSingleLs: " ++ show p )

-- A shell for a responder that does not care about its internal state,
-- i.e. each input message has sufficient data to determine the response
serverShell :: (Show y, Show ls) =>
    String                 -> -- Name of this server shell
    (x -> ls -> (ls, y))   -> -- The function on state and payload
    L.Thread ls ()            -- Resulting lifted braid thread
serverShell name f =
  do { chan <- L.newChan (name ++ " server input channel")
     ; let serverBounds = mkResponderBounds chan
     ; serverLoop f serverBounds
     }

-- Helper function for the responder loop
serverLoop :: (Show y) =>
    (x -> ls -> (ls, y))                     -> -- Function on state and payload
    ResponderBounds ls (ClientServerMsg x y) -> -- Bounds on the server
    L.Thread ls ()                              -- Resulting lifted thread
serverLoop f serverBounds =
     B.toM serverBounds
           ( do { m  <- get serverBounds
                ; m' <- respondSingleLs f m
                ; respond serverBounds m m'
                }
           )

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkResponderBounds   #-}
{-# INLINE get'                #-}
{-# INLINE respond'            #-}
{-# INLINE yield'              #-}
{-# INLINE myThreadId'         #-}
{-# INLINE putStrLn'           #-}
{-# INLINE putStr'             #-}
{-# INLINE respondSingle       #-}
