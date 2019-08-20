module Main where

import System.IO
import Data.Char

blockifyLines :: [String] -> [String]
blockifyLines (x:xs) = (map blockifyLine x) : (blockifyLines xs)
blockifyLines []     = []

blockifyLine :: Char -> Char
blockifyLine x
    -- 0/1 encoding
    | x == '0'      = ' '
    | x == '1'      = '█'
    -- Whitespace/Non-Whitespace encoding
    -- | isSpace x     = ' '
    -- | isAlphaNum x  = '█'

main :: IO ()
main = do
   contents <- readFile "input.txt"
   putStr . unlines . blockifyLines $ lines contents
   writeFile "output.txt" contents