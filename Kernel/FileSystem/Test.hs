module Main where
import IO
import List
import Monad
import qualified System as SYS
-- DaVinci imports
import qualified DVDeep as DVD
import qualified DaVinciTypes as DT
-- Graph imports
import qualified Deep as D
-- Posix imports
import qualified ParseFileName as PFN
import qualified FileName as FN
-- Local imports
import qualified FileSystem as FS

-- Print out a list, one element per line
outList :: (Show a) => [a] -> String
outList ls = concat (intersperse "\n" (map show ls))

main :: IO ()
main =
  do { args <- SYS.getArgs
     ; progName <- SYS.getProgName
     ; putStrLn ("args = " ++ show args)
     ; when (length args < 1)
            (error (progName ++ ": No file system specified"))
     ; let fsName = head args
     ; fsEntries <- FS.createFileSystem fsName
     ; hout <- openFile "fs.out" WriteMode
     ; hPutStrLn hout (outList fsEntries)
     ; hPutStrLn hout (concat (replicate 80 "="))
     ; let fs = FS.parseFileSystem fsName
           DT.New nodes =
             DVD.outDeepDaVinci
                    fs
                    FN.outFileName
                    outFileNode        -- How to print a node name
                    (DVD.outEdgeMin [] outLinkFreight)
     ; hPutStr hout (FS.outFileSystem 0 fs)
     ; hFlush hout
     ; hClose hout
     ; hdv <- openFile "ghc.dv" WriteMode
     ; hPutStrLn hdv (show nodes)
     ; hFlush hdv
     ; hClose hdv
     }

outLinkFreight :: FS.LinkFreight -> String
outLinkFreight lf = FN.outFileName (FS.linkTo lf)

-- Dump out a node name in the file system
outNodeName :: FN.FileName -> String
outNodeName [s] = s
outNodeName []  = error ("null node name")
--outNodeName ss  = error ("long node name: " ++ FN.outFileName ss)
outNodeName ss  = last ss

-- Print a file system graph node
outFileNode :: FS.FileNode -> [DT.Attribute]
outFileNode n =
  [ DT.A "OBJECT" (FN.outFileName (D.nodeName n))
  , DT.A "_GO"    "box"
  , DT.A "COLOR"  "salmon1"
  ]
