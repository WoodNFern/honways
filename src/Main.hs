module Main where

import System.IO

main :: IO ()
main = do
   contents <- readFile "input.txt"
   putStr contents
   writeFile "output.txt" contents