-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module Assocs
    ( AssocPair (..) -- A pair for the association list
    , Assocs         -- The association list type
    , aladd          -- Add a pair to the association list
    , aladdhard      -- Add a pair, fail if duplicate
    , aladdlist      -- Add a list to the association list
    , aldel          -- Delete a pair from the association list
    , almap          -- Map a function over the association list
    , alkeys         -- Get the list of keys from the association list
    , allist         -- Get the list of (key, val) from the association list
    , alinit         -- Make an empty association list
    , allookup       -- Look up a key in the association list
    , allookupBy     -- Look up using function of key, return key
    , allookhard     -- Look up a key, don't take no for an answer
    , alupdate       -- Update a pair in the association list
    , aloutput       -- Indented print out of an association list
    , alempty        -- Test if an association list is empty
    , alsingle       -- Create a singleton association list
    , aladdafter     -- Add a new element after the first element
    , alfilterkey    -- Filter the association list, with key selection
    )
    where
	
import List

----------------------------------------------------------------------
--  Assoc list data type
----------------------------------------------------------------------

data (Eq key) => AssocPair key pay =
    AssocPair
    { apkey :: key -- Key for distinguishing entries of association list
    , appay :: pay -- The data accessed by the key
    } deriving (Show)

-- Convert an AssocPair to a plain old pair
tuple :: (Eq key) => AssocPair key pay -> (key, pay)
tuple ap = (apkey ap, appay ap)

type Assocs key pay = [AssocPair key pay]

-- Map a function over the payloads of the association list.
almap :: (Eq key) =>
    (key -> pay -> pay') -> -- Function of key and payload
    Assocs key pay       -> -- Input assoclist
    Assocs key pay'         -- Output assoclist
almap _f [] = []
almap f (AssocPair k p:az) =
  let newpair = AssocPair k (f k p)
      newlist = almap f az
  in newpair:newlist

-- Get the list of keys in the association list
alkeys :: (Eq key) =>
    Assocs key pay -> -- Input association list
    [key]             -- Keys from the list
alkeys = map apkey

-- Get the list of keys in the association list
allist :: (Eq key) =>
    Assocs key pay -> -- Input association list
    [(key,pay)]       -- Key / pay pairs from the list
allist = map tuple

-- Filter an association list, with a selection on keys
alfilterkey :: (Eq key) =>
    Assocs key pay -> -- The list to filter
    (key -> Bool)  -> -- The selection function
    Assocs key pay    -- The filtered association list
alfilterkey [] _f = []
alfilterkey (AssocPair k p:az) f =
  if f k
  then (AssocPair k p):(alfilterkey az f)
  else alfilterkey az f

-- Output an association list
aloutput :: (Eq key) =>
    Int                    -> -- Amount to indent
    Assocs key pay         -> -- Assoc list to output
    (Int -> key -> String) -> -- Func to output a key
    (Int -> pay -> String) -> -- Func to output a payload
    String                    -- Output Assoc list
aloutput _n [] _outkey _outpay = "Empty Assoc List"
aloutput n (AssocPair k p:az) outkey outpay =
  let rest = if null az
             then "End Assoc List Output\n"
             else "\n" ++ aloutput n az outkey outpay
  in indent n ++ "(key = " ++ outkey (n + 1) k ++ "\n" ++
     indent n ++ " pay = " ++ outpay (n + 1) p ++ "\n" ++
     indent n ++ ")" ++ rest

-- Indent a string, tabs at 3
indent :: Int -> String
indent n = replicate (n * 3) ' '

-- Create a singleton association list
alsingle :: (Eq key) =>
  key ->            -- Key identifying the data
  pay ->            -- The data to store
  Assocs key pay    -- The resulting association list
alsingle key pay = [AssocPair key pay]

-- Add a new (key, pay) element to the assocation list.
-- The new element is added at the front of the list.
aladd :: (Eq key) =>
  Assocs key pay -> -- Association list to be added to
  key            -> -- Key identifying the data
  pay            -> -- The data to store
  Assocs key pay    -- The resulting association list
aladd assocs key pay =
  let melem = find (\x -> apkey x == key) assocs
  in case melem of
       Nothing ->    (AssocPair key pay : assocs)
       Just _elem -> assocs

-- Add a pair to the association list
aladdpair :: (Eq key) =>
  Assocs key pay -> -- Association list to be added to
  (key, pay)     -> -- Pair to add to the association list
  Assocs key pay    -- The resulting association list
aladdpair assocs pair = aladd assocs (fst pair) (snd pair)

-- Add a list of pairs to the association list
aladdlist :: (Eq key) =>
  Assocs key pay -> -- Association list to be added to
  [(key, pay)] ->   -- Items to be added
  Assocs key pay    -- The resulting association list
aladdlist assocs pairs = foldl aladdpair assocs pairs
  
-- Add a new (key, pay) element to the assocation list.
-- The new element is added after the first element in
-- the list
aladdafter :: (Eq key) =>
  Assocs key pay -> -- Association list to be added to
  key            -> -- Key identifying the data
  pay            -> -- The data to store
  Assocs key pay    -- The resulting association list
aladdafter assocs key pay =
  let melem = find (\x -> apkey x == key) assocs
      newEntry = AssocPair key pay
  in case melem of
       Nothing ->
         case assocs of
           [] -> [newEntry]
           [onlyOne] -> onlyOne : [newEntry]
           (a1:a2:az) -> a1:newEntry:a2:az
       Just _elem -> assocs

-- Add a ndw (key, pay) element to the assocation list.
-- Fail if there is a collision
aladdhard :: (Eq key) =>
  Assocs key pay -> -- Association list to be added to
  key            -> -- Key identifying the data
  pay            -> -- The data to store
  Assocs key pay    -- The resulting association list
aladdhard assocs key pay =
  let melem = find (\x -> apkey x == key) assocs
  in case melem of
       Nothing ->    (AssocPair key pay : assocs)
       Just _elem -> error "aladd: duplicate"
	 
-- Delete a (key, pay) element (if found)
aldel :: (Eq key) =>
  Assocs key pay -> -- The association list to be deleted from
  key            -> -- Identify the pair to delete
  Assocs key pay    -- The deleted association list
aldel [] _key = []
aldel (assoc:assocs) key =
  if key == apkey assoc
  then aldel assocs key         -- In case of duplicate key
  else assoc:(aldel assocs key)

-- Loodup the key in the assocation list, and return the corresponding
-- payload (if found)
allookup :: (Eq key) =>
  Assocs key pay -> -- Association list in which to look
  key            -> -- What to look for
  Maybe pay         -- The data found
allookup assocs key =
  let me = find (\x -> apkey x == key) assocs
  in case me of
       Nothing -> Nothing
       Just e  -> Just (appay e)

-- Loodup the key in the assocation list, using a function on the key,
-- and return the corresponding key (if found)
allookupBy :: (Eq key) =>
  Assocs key pay -> -- Association list in which to look
  (key -> Bool)  -> -- What to look for
  Maybe key         -- The data found
allookupBy assocs f =
  let me = find (\x -> f . apkey $ x) assocs
  in case me of
       Nothing -> Nothing
       Just e  -> Just (apkey e)

-- Loodup the key in the assocation list, and return the corresponding
-- payload. This version will not take no for an answer
allookhard :: (Eq key, Show key) =>
  Assocs key pay -> -- Association list in which to look
  key            -> -- What to look for
  pay               -- The data found
allookhard assocs key =
  let me = find (\x -> apkey x == key) assocs
  in case me of
       Nothing -> error ("allookhard: " ++ show key)
       Just e  -> appay e

-- Update an element of an assocation list (If found), otherwise
-- insert the element into the assocation list.
alupdate :: (Eq key) =>
  Assocs key pay -> -- Association list to update
  key            -> -- Key value of pair to update
  pay            -> -- New data to place there
  Assocs key pay    -- Updated association list
alupdate [] key pay = [AssocPair key pay]
alupdate (assoc:assocs) key pay =
  if apkey assoc == key
  then (AssocPair key pay):assocs
  else let rest = alupdate assocs key pay
       in assoc:rest
	 
-- Initialize an association list
alinit :: (Eq key) => Assocs key pay
alinit = []

-- Check if an association list is empty
alempty :: (Eq key) =>
  Assocs key pay -> -- Association list to query
  Bool             -- True if empty
alempty pairs = null pairs
