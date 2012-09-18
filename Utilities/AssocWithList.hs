-- Copyright (c) Peter Duncan White, 2002, 2003
-- Copyright (c) OHSU, 2002, 2003
module AssocWithList
    ( AssocWithList -- An association list, where the payload is a list
    , insert        -- Insert into list associated with key
    , active        -- Determine if there is any association to a key
    , get           -- Get a payload associated with the key (if any)
    , empty         -- Create an empty AssocWithList
    , nullAWL       -- See if there is anything in any list
    )
    where
	
import qualified Assocs as A

----------------------------------------------------------------------
--  Assoc list data type, where the payload is also a list
--  This is a specialization of the Assocs data type.
----------------------------------------------------------------------

type AssocWithList key pay = A.Assocs key [pay]

-- Insert into list associated with key
insert :: (Eq key) =>
    AssocWithList key pay -> -- Association list in which to insert
    key                   -> -- Key of the new association
    pay                   -> -- New item to add to list with key
    AssocWithList key pay    -- Updated association list
insert assocs key pay=
  let mpaylist = A.allookup assocs key
  in case mpaylist of
       Nothing      -> A.aladd assocs key [pay]
       Just paylist -> A.alupdate assocs key (pay:paylist)

-- Determine if a key has anything associated with it
active :: (Eq key) =>
    AssocWithList key pay -> -- Association list to search
    key                   -> -- Key to query
    Bool                     -- Is there associated element?
active assocs key =
  let mpaylist = A.allookup assocs key
  in case mpaylist of
       Nothing      -> False
       Just paylist -> not (null paylist)

-- Get the last (oldest) element associated with a key.
get :: (Eq key) =>
    AssocWithList key pay ->           -- Association list to search
    key                   ->           -- Key to query
    (Maybe pay, AssocWithList key pay) -- Payload found, and updated assocs
get assocs key =
  let mpaylist = A.allookup assocs key
  in case mpaylist of
       Nothing -> (Nothing, assocs)
       Just paylist ->
         if null paylist
         then (Nothing, assocs)
         else let assocs' = A.alupdate assocs key (init paylist)
              in (Just (last paylist), assocs')

-- Create an empty AssocWithList
empty :: (Eq key) => AssocWithList key pay
empty = A.alinit

-- See if there is anything in any of the lists
nullAWL :: (Eq key) => AssocWithList key pay -> Bool
nullAWL [] = True
nullAWL (a:az) =
  let A.AssocPair _k l = a
  in null l && nullAWL az
