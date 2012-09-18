-- Copyright (c) Peter Duncan White, 2001, 2002, 2003
module BraidOutput
    ( indent        -- Create an indentation
    , showIndent    -- Show with indentation
    , showNoIndent  -- Show without indentation
    , pad           -- Pad a string to n characters
    , outTid        -- For outputing a thread Id
    ) where

tabstop :: Int
tabstop = 3

indent :: Int -> String
indent n = replicate (n * tabstop) ' '

showIndent :: (Show a) => Int -> a -> String
showIndent n a = indent n ++ show a

showNoIndent :: (Show a) => Int -> a -> String
showNoIndent _n a = show a

pad :: Int -> String -> String
pad n s =
  if length s >= n
  then s
  else pad n (s ++ " ")

outTid :: (Show tid) => tid -> String
outTid tid = "Tid" ++ show tid
