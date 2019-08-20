module Main where

import System.IO

blockifyLines :: [String] -> [String]
blockifyLines (x:xs) = (map blockifyLine x) : (blockifyLines xs)
blockifyLines []     = []

blockifyLine :: Char -> Char
blockifyLine '0' = ' '
blockifyLine '1' = '█'
blockifyLine x = x

main :: IO ()
main = do
   contents <- readFile "input.txt"
   putStr . unlines . blockifyLines $ lines contents
   writeFile "output.txt" contents