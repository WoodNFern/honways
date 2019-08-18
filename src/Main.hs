module Main where

import System.IO

processLines :: [String] -> [String]
processLines (x:xs) = (processLine x) : (processLines xs)
processLines []     = []

processLine :: String -> String
processLine x = "Processed one line\n"

main :: IO ()
main = do
   contents <- readFile "input.txt"
   putStr . foldr (++) "" $ processLines $ lines contents
   writeFile "output.txt" contents