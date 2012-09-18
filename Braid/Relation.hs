-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Relation where

----------------------------------------------------------------------
--  Properties of relations
----------------------------------------------------------------------

type Relation x = x -> x -> Bool

{-P: {-< Properties of relations >-}
property Reflexive =
    {| r :: Relation x | All a :: x. { r a a } === { True } |}

property Symmetric =
    {| r :: Relation x | All a :: x. All b :: x.
       { r a b } === { True } ==>
       { r b a } === { True }
     |}

property Transitive =
  {| r :: Relation x | All a :: x. All b :: x. All c :: x.
     ( { r a b } === { True } ) /\
     ( { r b c } === { True } ) ==>
     ( { r a c } === { True } )
   |}

property EquivalenceRelation =
  {| r :: Relation x | r ::: Reflexive /\ r ::: Symmetric /\
                       r ::: Transitive |}

-- The equivalence class should not include bottom.
property EquivalenceClass r a =
    Lfp X. {| x | Exist y. ( { x } === { a } ) \/
                           ( y ::: X /\ { r x y } === { True } ) |}
 -}
