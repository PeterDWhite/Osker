-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Spoke
    ( Spoke               -- Abstract data type
    , mkSpoke             -- Constructor for Spoke
    , SpokeBounds         -- Structure of constructors for Spoke
    , PreSpoke            -- Spoke without bounds
    , readChan            -- Get input from input channel
    , writeChan           -- Write to a channel
    , yield               -- Take a break
    , myThreadId          -- Get local thread Id
    , putStrLn            -- For debugging
    , putStr              -- For debugging
    , module Channels     -- Re-export channel stuff
    ) where

----------------------------------------------------------------------
--  The standard bounds for a spoke
----------------------------------------------------------------------

-- Haskell imports
import Prelude hiding (IO, putStrLn, putStr)
import Ix
import Dynamic (Typeable)
-- Resumption imports
import qualified BoundedMonad as B
-- Braid imports
import ThreadId
import qualified BraidLocal as L
-- Local imports
import Channels

-- The spoke bounded Monad
type Spoke ls c m a = B.BdM (L.Thread ls) (SpokeBounds ls c m) a
-- Data required to create a spoke
type PreSpoke ls c m = SpokeBounds ls c m -> Spoke ls c m ()

----------------------------------------------------------------------
-- Package the constructors for the kernel thread bounded monad
----------------------------------------------------------------------
data SpokeBounds ls c m =
    SpokeBounds
    { -- Allowed channels
      channels          :: Channels c m
      -- Allowed interfaces
    , writeChan         :: c -> m -> Spoke ls c m ()
    , readChan          :: c -> Spoke ls c m m
    , yield             :: Spoke ls c m ()
    , myThreadId        :: Spoke ls c m ThreadId
      -- Debugging interfaces
    , putStrLn          :: String -> Spoke ls c m ()
    , putStr            :: String -> Spoke ls c m ()
    }

----------------------------------------------------------------------
-- Create the constructors of the Spoke
----------------------------------------------------------------------

-- Get the local kernel thread Id
myThreadId' :: Spoke ls c m ThreadId
myThreadId' = B.mTo L.myThreadId

-- Read an allowed channel
readChan' ::(Ix c, Show ls) => Channels c m -> c -> Spoke ls c m m
readChan' chs c = B.mTo ( L.readChan ( chs!c ) )

-- Write an allowed channel
writeChan' :: (Typeable m, Show ls, Ix c) =>
    Channels c m -> c -> m -> Spoke ls c m ()
writeChan' chs c m = B.mTo ( L.writeChan ( chs!c ) m )

-- Yield the processor for a while
yield' :: Spoke ls c m ()
yield' = B.mTo L.yield

-- Write a line on standard output (for debugging only)
putStrLn' :: String -> Spoke ls c m ()
putStrLn' s = B.mTo (L.putStrLn s)

-- Write a string on standard output (for debugging only)
putStr' :: String -> Spoke ls c m ()
putStr' s = B.mTo (L.putStr s)

-- Construct a bounded IO thread from the bounds given
mkSpokeBounds :: (Show ls, Ix c, Typeable m) =>
    Channels c m -> SpokeBounds ls c m
mkSpokeBounds chs =
  SpokeBounds { channels          = chs
              , writeChan         = writeChan' chs
              , readChan          = readChan'  chs
              , yield             = yield'
              , myThreadId        = myThreadId'
              , putStrLn          = putStrLn'
              , putStr            = putStr'
              }

----------------------------------------------------------------------
-- Renamings into Spoke language
----------------------------------------------------------------------

-- Convert a spoke into a braid (safely)
-- toBraid :: SpokeBounds ls c m -> Spoke ls c m a -> L.Thread ls a
-- toBraid = B.toM

-- Make a spoke from some channels and a pre-spoke
mkSpoke :: (Ix c, Show ls, Typeable m) =>
    Channels c m -> PreSpoke ls c m -> L.Thread ls ()
mkSpoke chans preSpoke = B.toM spokeBounds (preSpoke spokeBounds)
  where spokeBounds = mkSpokeBounds chans

----------------------------------------------------------------------
--  Inline some functions
----------------------------------------------------------------------
{-# INLINE mkSpokeBounds       #-}
{-# INLINE mkSpoke             #-}
{-# INLINE readChan'           #-}
{-# INLINE writeChan'          #-}
{-# INLINE yield'              #-}
{-# INLINE myThreadId'         #-}
{-# INLINE putStrLn'           #-}
{-# INLINE putStr'             #-}
