-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
-- Copyright (c) OHSU, 2001, 2002, 2003
module UtilOut where

testOut :: String -> IO ()
testOut s = putStrLn ("~~~[Test Script]...\t\t" ++ s)

fail :: Int -> IO ()
fail n = putStrLn ("\nTest " ++ show n ++ " fails\n")

pass :: Int -> IO ()
pass n = putStrLn ("\nTest " ++ show n ++ " passes\n")

commence :: Int -> IO ()
commence n = putStrLn ("\nTest " ++ show n ++ " commences\n")
