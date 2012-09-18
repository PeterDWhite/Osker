-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module DomainThread
    ( DomainThread     -- A domain thread in the domain braid
    , projDB           -- Project out of the domain thread
    , inDB             -- Lift into the domain thread
      -- Domain thread versions of channel primitives
    , writeChan        -- Write to a channel
    , readChan         -- Read from a channel
    , isEmptyChan      -- Test if channel is empty
    , LC.Chan          -- Channel abstract type
      -- Domain thread versions of IO primitives
    , putStrLn         -- Write a line to the screen
    , putStr           -- Write a string to the screen
      -- Domain thread versions in support of fork
    , myThreadId       -- Get the local thread Id
    , yield            -- Yield the domainor for a while
      -- In support of IO actions
    , liftIO           -- Raise an IO action to the bounded thread monad
    , liftDB           -- Lift IO action to the domain thread
    ) where

-- Haskell imports
import Prelude hiding ( putStrLn, putStr )
import qualified Dynamic as DYN
-- Braid imports
import qualified BraidExternal as B
import qualified BraidLocal as L
import qualified LocalChan as LC
import qualified ThreadId as TID
import qualified BoundedMonad as BdM
-- Posix imports
import qualified ProcessName as PN
-- Local imports
import qualified DomainState as PS

----------------------------------------------------------------------
-- A domain thread within the domain braid
----------------------------------------------------------------------

-- A domain thread is a local thread within the domain braid
-- When a domain thread is lifted, it will have the same type as
-- the domain braid. This is the whole point, that a local thread
-- (in this case a domain thread), can be lifted into the braid
-- safely
newtype DomainThread a = DomainThread (L.Thread PS.DomainState a)

-- Project out of a domain thread
projDB :: DomainThread a -> L.Thread PS.DomainState a
projDB (DomainThread t) = t

-- Inject into a domain thread
inDB :: L.Thread PS.DomainState a -> DomainThread a
inDB t = DomainThread t

-- A wrapper on a monad is still a monad
instance Monad DomainThread where
    return = DomainThread . return
    DomainThread p >>= k =
      DomainThread ( do { a <- p
                         ; let DomainThread k' = k a
                         ; k'
                         }
                    )

----------------------------------------------------------------------
-- Domain thread channel primitives
----------------------------------------------------------------------

-- Domain thread version of writeChan
writeChan :: (DYN.Typeable a) => LC.Chan a -> a -> DomainThread ()
writeChan chan = inDB . LC.writeChan chan

-- Domain thread version of readChan
readChan :: (DYN.Typeable a) => LC.Chan a -> DomainThread a
readChan = inDB . LC.readChan

-- Test if a channel is empty
isEmptyChan :: (DYN.Typeable a) => LC.Chan a -> DomainThread Bool
isEmptyChan = inDB . LC.isEmptyChan

----------------------------------------------------------------------
-- Domain thread screen IO primitives
----------------------------------------------------------------------
-- Write a line to the screen
putStrLn :: String -> DomainThread ()
putStrLn = inDB . L.putStrLn

-- Write a line to the screen
putStr :: String -> DomainThread ()
putStr = inDB . L.putStr

----------------------------------------------------------------------
-- Domain thread forking primitives
----------------------------------------------------------------------
-- Get the local thread Id
myThreadId :: DomainThread PN.ProcessName
myThreadId = inDB L.myThreadId

-- Yield the domainor for a while
yield :: DomainThread ()
yield = inDB L.yield

----------------------------------------------------------------------
-- Support for IO actions
----------------------------------------------------------------------
-- Lift to the bounded monad around domain thread
liftIO :: IO a -> BdM.BdM DomainThread b a
liftIO io = BdM.BdM ( \b -> BdM.pairM b (DomainThread (L.liftIO io)) )

-- Lift IO to the domain thread
liftDB :: IO a -> DomainThread a
liftDB = DomainThread . L.liftIO
