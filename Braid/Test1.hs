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
-- Utility imports
import qualified PartialOrder as PO
-- Braid imports
import ThreadId ( ThreadId,, atLevel, level
                , mkLifted, mkUnlifted, firstLiftedSub, firstUnliftedSub )
import MVarId ( MVarId, mkMVarId, isSub )

-- Prepare some thread ids.
l0 :: ThreadId
l0    = mkLifted [0]
l00 :: ThreadId
l00   = mkLifted  [0, 0]
l000 :: ThreadId
l000  = mkLifted  [0, 0, 0]
g0 :: ThreadId
g0   = mkUnlifted [0]
g00 :: ThreadId
g00  = mkUnlifted [0, 0]
g000 :: ThreadId
g000 = mkUnlifted [0, 0, 0]
mvid0 :: MVarId
mvid0     = mkMVarId l0 "[0]"
mvid00 :: MVarId
mvid00    = mkMVarId l00 "[0,0]"
mvid000 :: MVarId
mvid000   = mkMVarId l000 "[0,0,0]"

main :: IO ()
main =
  do { putStrLn ( "\nTest threadId functions" )
       -- Test isImmediateSubordinate via mkLifted
     ; if atLevel l00 l000
       then putStrLn ( "Step 1a passes" )
       else error ( " Step 1a fails" )
     ; if atLevel l00 (mkLifted [0,0,1])
       then putStrLn ( "Step 2a passes" )
       else error ( " Step 2a fails" )
     ; if atLevel l00 (mkLifted [0,0,1,0])
       then error ( " Step 3a fails" )
       else putStrLn ( "Step 3a passes" )
     ; if atLevel l00 l00
       then error ( " Step 4a fails" )
       else putStrLn ( "Step 4a passes" )
     ; if atLevel l00 l0
       then error ( " Step 5a fails" )
       else putStrLn ( "Step 5a passes" )
     ; if atLevel (mkLifted [0,1]) l0
       then error ( " Step 6a fails" )
       else putStrLn ( "Step 6a passes" )
       -- Test atLevel via mkUnlifted
     ; if atLevel g00 g000
       then putStrLn ( "Step 1b passes" )
       else error ( " Step 1b fails" )
     ; if atLevel g00 (mkUnlifted [0,0,1])
       then putStrLn ( "Step 2b passes" )
       else error ( " Step 2b fails" )
     ; if atLevel (g00) (mkUnlifted [0,0,1,0])
       then error ( " Step 3b fails" )
       else putStrLn ( "Step 3b passes" )
     ; if atLevel (g00) (g00)
       then error ( " Step 4b fails" )
       else putStrLn ( "Step 4b passes" )
     ; if atLevel (g00) (mkUnlifted [0])
       then error ( " Step 5b fails" )
       else putStrLn ( "Step 5b passes" )
     ; if atLevel (mkUnlifted [0,1]) g0
       then error ( " Step 6b fails" )
       else putStrLn ( "Step 6b passes" )
       -- Test level
     ; if level g00 (mkUnlifted [0,0,0]) == PO.EQ
       then putStrLn ( "Step 1c passes" )
       else error ( " Step 1c fails" )
     ; if level l00 (mkLifted [0,0,1]) == PO.EQ
       then putStrLn ( "Step 2c passes" )
       else error ( " Step 2c fails" )
     ; if level g00 (mkUnlifted [0,0,1,0]) == PO.LT
       then putStrLn ( "Step 3c passes" )
       else error ( "Step 3c fails" )
     ; if level g00 g00 == PO.LT -- Weird case
       then putStrLn ( "Step 4c passes" )
       else error ( "Step 4c fails" )
     ; if level g00 g0 == PO.GT
       then putStrLn ( "Step 5c passes" )
       else error ( "Step 5c fails" )
     ; if level (mkUnlifted [0,1]) g0 == PO.GT
       then putStrLn ( "Step 6c passes" )
       else error ( "Step 6c fails" )
     ; if level (mkUnlifted [0,1]) (mkUnlifted [0,2]) == PO.NC
       then putStrLn ( "Step 7c passes" )
       else error ( "Step 7c fails" )
     ; if level (mkUnlifted [0,1,2,3]) (mkUnlifted [1,0,2,1]) == PO.NC
       then putStrLn ( "Step 8c passes" )
       else error ( "Step 8c fails" )
       -- Test nnFirstSub
     ; if firstLiftedSub g0 == l00
       then putStrLn ( "Step 9a passes" )
       else error ( "Step 9a fails: " ++ show (firstLiftedSub g0) )
     ; if firstUnliftedSub (mkUnlifted [0])  == g00
       then putStrLn ( "Step 9b passes" )
       else error ( "Step 9b fails: " ++ show (firstUnliftedSub g0) )
     ; putStrLn ( "\nNow start testing MVarIds vs ThreadIds" );
       if isSub l0 mvid00
       then putStrLn ( "Step 10 passes" )
       else putStrLn ( "Step 10 fails: " ++ show mvid00 )
     ; if isSub l0 mvid0
       then putStrLn ( "Step 11 passes" )
       else putStrLn ( "Step 11 fails: " ++ show mvid0 )
     ; if isSub l0 mvid000
       then putStrLn ( "Step 12 passes" )
       else putStrLn ( "Step 12 fails: " ++ show mvid000 )
     ; if isSub l00 mvid0
       then putStrLn ( "Step 12 fails: " ++ show mvid0 )
       else putStrLn ( "Step 12 passes" )
     ; putStrLn ( "\nTest 1 passes" )
  }
