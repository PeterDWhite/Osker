-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
-- However, note the following copyright by University of Glasgow
-----------------------------------------------------------------------
-- 
-- Module      :  Control.Concurrent.Chan
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Standard, unbounded channel abstraction.
--
-----------------------------------------------------------------------
-- Osker modifications: Use Braid instead of GHC IO
-----------------------------------------------------------------------

module OskerChan
	( Chan			-- abstract
	  -- creator
	, newChan	 	-- :: Braid (Chan a)
	  -- operators
	, writeChan	 	-- :: Chan a -> a -> Braid ()
	, readChan	 	-- :: Chan a -> Braid a
	, isEmptyChan		-- :: Chan a -> Braid Bool
	  -- stream interface
	, getChanContents	-- :: Chan a -> Braid [a]
	, writeList2Chan	-- :: Chan a -> [a] -> Braid ()
          -- Debugging
        , outChan               -- :: BraidSt -> Chan a -> String
        ) where

-- Haskell imports
import Dynamic (Typeable)
-- Local imports
import qualified BraidExternal as B
import qualified BraidInternal as BI
import PreChan ( Chan (..), Stream (..), ChItem (..), writeEnd, readEnd )

-----------------------------------------------------------------------
-- Debugging functions
-----------------------------------------------------------------------

outStream :: (Show a) => Stream a -> B.Braid gs ls String
outStream stream =
  do { mhole <- BI.dereferenceMVar (streamVar stream)
     ; case mhole of
         Nothing -> return "X"
         Just hole ->
           let ChItem a stream' = hole
           in do { rest <- outStream stream'
                 ; return ( "[" ++ show a ++ "]->" ++ rest )
                 }
     }

outChan :: (Show a) => Chan a -> B.Braid gs ls String
outChan chan =
  do { mreadStream  <- BI.dereferenceMVar (chanRead chan)
     ; mwriteStream <- BI.dereferenceMVar (chanWrite chan)
     ; streamsOut   <-
         case mreadStream of
           Nothing -> error "outChan: empty read stream"
           Just readStream ->
             case mwriteStream of
               Nothing          -> outStream readStream
               Just writeStream ->
                 do { readOut  <- outStream readStream
                    ; writeOut <- outStream writeStream
                    ; return (readOut ++ writeOut)
                    }
     ; return ( chanName chan ++ " " ++ streamsOut )
     }

-- newChan sets up the read and write end of a channel by initialising
-- these two MVars with an empty MVar.
newChan :: (Show gs, Show ls) => String -> B.Braid gs ls (Chan a)
newChan name =
  do { hole      <- B.newEmptyMVar name
     ; readHole  <- B.newMVar (readEnd name) (Stream hole)
     ; writeHole <- B.newMVar (writeEnd name) (Stream hole)
     ; return ( Chan { chanName  = name,
                       chanRead  = readHole
                     , chanWrite = writeHole
                     }
              )
     }

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.
-- Osker: modification to remove old write end
writeChan :: (Typeable a, Show ls, Show gs) =>
    Chan a -> a -> B.Braid gs ls () --
writeChan ( Chan { chanName  = name, chanWrite = writeHole }) val =
  do { new_hole <- B.newEmptyMVar (writeEnd name)
     ; Stream old_hole <- B.takeMVar writeHole
     ; B.putMVar writeHole (Stream new_hole)
     ; B.putMVar old_hole (ChItem val (Stream new_hole))
     }

-- We need to delete the variable at the read end of the channel
-- This will make a "dupChan" impossible
readChan :: (Show ls, Show gs) => Chan a -> B.Braid gs ls a --
readChan ( Chan { chanRead = readHole } ) =
  do { Stream read_end         <- B.takeMVar readHole
     ; ChItem val new_read_end <- B.takeMVar read_end
     ; B.putMVar readHole new_read_end
     ; B.deleteMVar read_end
     ; return val
     }

-- This one is too hard for our little simulation
--dupChan :: (Typeable a) => Chan a -> B.Braid (Chan a)
--dupChan (Chan name _readHole write) = do
--   hole     <- B.readMVar write
--   new_read <- B.newMVar (readEnd name) hole
--   return (Chan name new_read write)

-- This one is too hard for our little simulation
--unGetChan :: Chan a -> a -> B.Braid ()
--unGetChan (Chan name readHole _write) val = do
--   new_read_end <- B.newEmptyMVar (readEnd name)
--   B.modifyMVar_ readHole $ \read_end -> do
--     B.putMVar new_read_end (ChItem val read_end)
--     return (Stream new_read_end)

isEmptyChan :: (Typeable a, Show ls, Show gs) =>
    Chan a -> B.Braid gs ls Bool --
isEmptyChan ( Chan { chanRead = readHole, chanWrite = writeHole } ) =
  do { B.withMVar readHole $ \r -> do
         w <- B.readMVar writeHole
         let eq = r == w
         eq `seq` return eq
     }

-- Operators for interfacing with functional streams.
getChanContents :: (Show ls, Show gs) => Chan a -> B.Braid gs ls [a] --
getChanContents ch
  = do { x  <- readChan ch
       ; xs <- getChanContents ch
       ; return (x:xs)
       }

-- Write a list of items to the channel
writeList2Chan :: (Typeable a, Show ls, Show gs) =>
    Chan a -> [a] -> B.Braid gs ls () --
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
