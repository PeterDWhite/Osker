-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Channels
    ( Channels      -- Channels available for the spoke
    , mkChannels    -- Constructor for channels
    , ChannelInit   -- Initializer for a channel structure
    , mkChannelInit -- Constructor for ChannelInit
    , chanOf        -- Channel of a channel initializer
    , initChannels  -- Initializer
    , (!)           -- Access the underlying array
    ) where

----------------------------------------------------------------------
-- Array of channels for the spoke
----------------------------------------------------------------------

-- Haskell imports
import qualified Array as A
import Ix
-- Utility imports
import Null
-- Braid imports
import qualified OskerChan as C

-- At this level of abstraction, the channels are polymorphic in the
-- type of message. The channel id is also abstract.
-- Note that all the channels must have the same type of message
newtype (Ix c) => Channels c m = Channels (A.Array c (C.Chan m))
-- Type of initializer for channels
data ChannelInit c m = ChannelInit { chanId :: c, chanOf :: C.Chan m }

-- Constructor for ChannelInit
mkChannelInit :: c -> C.Chan m -> ChannelInit c m
mkChannelInit = ChannelInit

-- Convert channel init to a pair
toPair :: ChannelInit c m -> (c, C.Chan m)
toPair chinit = (chanId chinit, chanOf chinit)

-- Project out the array
-- proj :: (Ix c) => Channels c m -> A.Array c (C.Chan m)
-- proj (Channels a) = a

-- Initializer for Channels
initChannels :: (Null c, Ix c) => C.Chan m -> Channels c m
initChannels ch = Channels ( A.array (mkNull, mkNull) [(mkNull, ch)] )

-- Constructor for Channels
mkChannels :: (Null c, Ix c) => [ ChannelInit c m ] -> Channels c m
mkChannels [] = error ( "mkChannels" )
mkChannels chans =
  let pairs = map toPair chans
  in Channels ( A.array (chanId (head chans), chanId (last chans)) pairs )

-- Access the underlying array
(!) :: (Ix c) => Channels c m -> c -> C.Chan m
(Channels a) ! c = a A.! c

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkChannels    #-}
{-# INLINE initChannels  #-}
{-# INLINE mkChannelInit #-}
{-# INLINE toPair        #-}
{-# INLINE (!)           #-}
