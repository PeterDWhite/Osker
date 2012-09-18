-- Copyright (c) Peter Duncan White, 2003
-- Copyright (c) OHSU, 2003
module Main where
----------------------------------------------------------------------
--
-- Test some miscellaneous low level functions
--
----------------------------------------------------------------------

--Haskell imports
import Monad
-- Braid imports
import ThreadId ( ThreadId, mkLifted, mkUnlifted )
import MVarId ( MVarId, mkMVarId )
import qualified Locale as L
import qualified LiftedState as LS
import qualified LiftedThread as LT
import qualified BraidState as BST

-- Prepare some thread ids.
-- l0 :: ThreadId
-- l0    = mkLifted [0]
l00 :: ThreadId
l00   = mkLifted  [0, 0]
-- l000 :: ThreadId
-- l000  = mkLifted  [0, 0, 0]
g0 :: ThreadId
g0   = mkUnlifted [0]
g00 :: ThreadId
g00  = mkUnlifted [0, 0]
-- g000 :: ThreadId
-- g000 = mkUnlifted [0, 0, 0]
-- mvid0 :: MVarId
-- mvid0     = mkMVarId l0 "[0]"
mvidl00 :: MVarId
mvidl00    = mkMVarId l00 "mvid00"
mvidg00 :: MVarId
mvidg00    = mkMVarId g00 "mvid00"
-- mvid000 :: MVarId
-- mvid000   = mkMVarId l000 "[0,0,0]"

-- Prepare some dynamic strings
-- v1 :: Dynamic
-- v1 = toDyn "String1"
-- Prepare some MVar requests
-- t1 :: L.Req
-- t1 = L.TakeReq { L.reqId = mvid00, L.taker = l00 }
-- p1 :: L.Req
-- p1 = L.PutReq { L.reqId = mvid00, L.putter = l00, L.reqVal = v1 }
-- Prepare some MVar locales
-- c0 :: L.Locale
-- c0 = mkNull
-- c1 :: L.Locale
-- c1 = L.addUpwardReq t1 c0
-- Prepare some local states
ls0 :: LT.LiftedSt String
ls0 = LT.mkLiftedState "ls0" l00
ls1 :: LT.LiftedSt String
ls1 = LS.takeMVar mvidg00 ls0
ls2 :: LT.LiftedSt String
ls2 = LS.takeMVar mvidl00 ls0
-- Prepare some braid states
bst0 :: BST.BraidState String String String
bst0 = BST.initBraidState "bst0" g0 0

main :: IO ()
main =
  do { putStrLn ( "\nTest locale functions" )
     ; putStrLn ( " ls1 = " ++ show ls1 )
     ; putStrLn ( " bst0 = " ++ show bst0 )
     ; let res1 = L.globalize l00 (LS.locale ls1) (BST.locale bst0)
     ; putStrLn ( "\nL.globalize l00 (LS.locale ls1) (BST.locale bst0) =\n\t" ++ show res1 )
     ; let res2 = L.globalize g0 (LS.locale ls1) (BST.locale bst0)
     ; putStrLn ( "\nL.globalize g0 (LS.locale ls1) (BST.locale bst0) =\n\t" ++ show res2 )
     ; let res3 = L.globalize l00 (LS.locale ls2) (BST.locale bst0)
     ; putStrLn ( "\nL.globalize l00 (LS.locale ls2) (BST.locale bst0) =\n\t" ++ show res3 )
     ; let res4 = L.globalize g0 (LS.locale ls2) (BST.locale bst0)
     ; putStrLn ( "\nL.globalize g0 (LS.locale ls2) (BST.locale bst0) =\n\t" ++ show res4 )
     ; putStrLn ( "\nTest 9 passes" )
  }
