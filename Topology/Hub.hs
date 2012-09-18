-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Hub
    ( Hub             -- Hub in hub and spoke topology
    , mkHub           -- Constructor for Hub
    , toBraid         -- Convert hub to an unlifted braid thread
    ) where

----------------------------------------------------------------------
--  The hub and spoke model
--  Hub: The initial thread of the model, able to communicate to
--       any other thread. The hub is an unlifted thread. The hub
--       can also respond to a message.
--  Spoke: At the periphery, able to communicate to the flanges and
--         the hub. A spoke is a lifted thread.
--  Flange: A responder thread, used to implement a service that
--          is not in the hub. This is a lifted thread. Implementing
--          a service in a lifted thread provides better separation
--          support. The flange can communicate to the hub, and it
--          can respond to any incoming message, without knowing the
--          the channel for the response. The channel is hidden
--          inside the message, and inside the constructors of
--          the Flange.
--  NOTE: Even if a spoke or a flange thread could learn the response
--        channel of the incoming message, it has no way to use it.
----------------------------------------------------------------------

-- Haskell imports
import Ix
import Dynamic (Typeable)
-- Utility imports
import qualified MonadUtilities as M
import Null
-- Braid imports
import qualified BraidExternal as B
import qualified OskerChan as C
import qualified BraidLocal as L
-- Local imports
import Channels
import qualified Responder as R
import qualified Spoke as S

-- The system half actor bounded Monad
data Hub gs ls = Hub ( B.Braid gs ls () )

-- Make a responder from the input data specified
mkRsp :: ( Typeable m, R.Respondable m, Show ls ) =>
   ( ChannelInit c m, R.PreResponder ls m ) -> L.Thread ls ()
mkRsp ( chinit, rsp ) = R.mkResponder (chanOf chinit) rsp

-- Get the braid out of the hub
toBraid :: Hub gs ls -> B.Braid gs ls ()
toBraid (Hub b) = b

----------------------------------------------------------------------
-- Constructor for the hub
----------------------------------------------------------------------
mkHub :: ( Null c, Ix c, Null ls, Show ls
         , Typeable m, R.Respondable m, Show gs) =>
  c                     -> -- Channel id of the hub
  [ ( ChannelInit c m      -- Channel initializer for input to a flange
    , R.PreResponder ls m  -- Local spoke to server as responder flange
    )
  ] ->
  [ S.PreSpoke ls c m ] -> -- Initial set of spokes (Hub can fork more)
  B.Braid gs ls ()      -> -- What to run after initialization of spokes
  Hub gs ls
mkHub hubChanId flanges prespokes prog =
  let rspChanInits = map fst flanges
  in Hub ( do { -- Make input channel for the hub
              ; chan <- C.newChan "Hub input"
              ; let hubChan = mkChannelInit hubChanId chan
                    channels = S.mkChannels (hubChan:rspChanInits)
                -- Create the flanges of the hub
              ; let responders = map mkRsp flanges
              ; rtids <- M.listM ( map (B.forkLift "Flange") responders )
              ; B.putStrLn ( "=== mkHub: responder tids = " ++ show rtids )
                -- Create the spokes in the model
              ; let spokes = map (S.mkSpoke channels) prespokes
              ; stids <- M.listM ( map (B.forkLift "Spoke") spokes )
                -- Now run the post-initialization program
              ; B.putStrLn ( "=== mkHub: spoke tids = " ++ show stids )
              ; prog
              }
         )

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkHub     #-}
