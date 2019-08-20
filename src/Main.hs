module Main where

import System.IO

processLines :: [String] -> [String]
processLines (x:xs) = (processLine x) : (processLines xs)
processLines []     = []

processLine :: String -> String
processLine x = "Processed one line\n"

replaceChar :: Char -> Char
replaceChar '0' = ' '
replaceChar '1' = '█'
replaceChar x = x

main :: IO ()
main = do
   contents <- readFile "input.txt"
   putStr . foldr (++) "" $ processLines $ lines contents
   writeFile "output.txt" contents