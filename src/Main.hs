module Main where

import System.IO

main :: IO ()
main = do
   handle <- openFile "input.txt" ReadMode
   contents <- hGetContents handle
   putStr contents
   hClose handle