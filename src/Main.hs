module Main where

import System.IO
import Data.Char
import Data.Matrix

numerifyTextLines :: [String] -> [[Double]]
numerifyTextLines (x:xs)    = (map numerifyTextLine x) : (numerifyTextLines xs)
numerifyTextLines []        = []

numerifyTextLine :: Char -> Double
numerifyTextLine x
    | x == '0'  = 0
    | x == '1'  = 1

textifyMatrix :: [[Double]] -> [String]
textifyMatrix (x:xs)    = (map textifyElement x) : (textifyMatrix xs)
textifyMatrix []        = []

textifyElement :: Double -> Char
textifyElement x
    | x <= 0.5  = ' '
    | x > 0.5   = '█'

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let matrix = fromLists . numerifyTextLines $ lines contents
   putStr . unlines . textifyMatrix $ toLists matrix
   writeFile "output.txt" contents