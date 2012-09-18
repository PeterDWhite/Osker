-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module ActorThread
    ( ActorThread      -- A actor thread in the domain braid
    , ActorProgram     -- A complete program for an actor
    , AS.ActorRet (..) -- Return value of the complete program
    , projActor        -- Project out of the actor thread
    , liftActor        -- Lift an actor thread to the bounded monad
    , inActor          -- Lift into the actor thread
      -- Actor thread versions of channel primitives
    , writeChan        -- Write to a channel
    , readChan         -- Read from a channel
    , isEmptyChan      -- Test if channel is empty
    , LC.Chan          -- Channel abstract type
      -- Actor thread versions of IO primitives
    , putStrLn         -- Write a line to the screen
    , putStr           -- Write a string to the screen
      -- Actor thread versions in support of fork
    , myThreadId       -- Get the local thread Id
    , yield            -- Yield the actoror for a while
      -- In support of IO actions
    , liftIO           -- Raise an IO action to the bounded thread monad
    , liftAT           -- Lift IO action to the actor thread
      -- State access actions
    , actorId          -- Get the local process name
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
import qualified ActorState as AS

----------------------------------------------------------------------
-- A actor thread within the process braid
----------------------------------------------------------------------

-- A actor thread is a local thread within the process braid
-- When a actor thread is lifted, it will have the same type as
-- the process braid.
newtype ActorThread s a = ActorThread (L.Thread (AS.ActorState s) a)

-- A complet actor program is an actor thread that returns the
-- return value specified for actor programs
type ActorProgram s = ActorThread s AS.ActorRet

-- Project out of a actor thread
projActor :: ActorThread s a -> L.Thread (AS.ActorState s) a 
projActor (ActorThread t) = t

-- Inject into a actor thread
inActor :: L.Thread (AS.ActorState s) a -> ActorThread s a
inActor t = ActorThread t

-- A wrapper on a monad is still a monad
instance Monad (ActorThread s) where
    return = ActorThread . return
    ActorThread p >>= k =
      ActorThread ( do { a <- p
                       ; let ActorThread k' = k a
                       ; k'
                       }
                  )

----------------------------------------------------------------------
-- Actor thread channel primitives
----------------------------------------------------------------------

-- Actor thread version of writeChan
writeChan :: (DYN.Typeable a) => LC.Chan a -> a -> ActorThread s ()
writeChan chan = inActor . LC.writeChan chan

-- Actor thread version of readChan
readChan :: (DYN.Typeable a) => LC.Chan a -> ActorThread s a
readChan = inActor . LC.readChan

-- Test if a channel is empty
isEmptyChan :: (DYN.Typeable a) => LC.Chan a -> ActorThread s Bool
isEmptyChan = inActor . LC.isEmptyChan

----------------------------------------------------------------------
-- Actor thread screen IO primitives
----------------------------------------------------------------------
-- Write a line to the screen
putStrLn :: String -> ActorThread s ()
putStrLn = inActor . L.putStrLn

-- Write a line to the screen
putStr :: String -> ActorThread s ()
putStr = inActor . L.putStr

----------------------------------------------------------------------
-- Actor thread forking primitives
----------------------------------------------------------------------
-- Get the local thread Id
myThreadId :: ActorThread s PN.ProcessName
myThreadId = inActor L.myThreadId

-- Yield the actoror for a while
yield :: ActorThread s ()
yield = inActor L.yield

----------------------------------------------------------------------
-- Support for IO actions
----------------------------------------------------------------------
-- Lift to the bounded monad around actor thread
liftIO :: IO a -> BdM.BdM (ActorThread s) b a
liftIO io = BdM.BdM ( \b -> BdM.pairM b (ActorThread (L.liftIO io)) )

-- Lift IO to the actor thread
liftAT :: IO a -> ActorThread s a
liftAT = ActorThread . L.liftIO

----------------------------------------------------------------------
-- State access actions
----------------------------------------------------------------------
-- Get the process name of the local process
actorId :: ActorThread s PN.ProcessName
actorId = ActorThread ( L.observe AS.actorId )

-- Lift an actor thread into the bounded monad
liftActor :: ActorThread s a -> BdM.BdM (ActorThread s) b a
liftActor act = BdM.BdM ( \b -> BdM.pairM b act )
