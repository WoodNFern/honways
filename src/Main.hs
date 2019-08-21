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

sumOfNeighbors :: (Num a, Show a) => (Int, Int) -> Matrix a -> a
sumOfNeighbors (x, y) m = foldr (+) 0 $ getNeighborElements (x, y) m

getNeighborElements :: (Int, Int) -> Matrix a -> a
getNeighborElements (x, y) m = map (\(r, c) -> getElem r c m) $ getNeighborIndices (x, y) m

getNeighborIndices :: (Int, Int) -> Matrix a -> [(Int, Int)]
getNeighborIndices (x, y) m = [ (r, c) | r <- [1 .. nrows m], c <- [1 .. ncols m],
                                         (abs (x - r)) <= 1, (abs (y - c)) <= 1,       -- all elements in radius 1
                                         not $ (x == r) && (y == c)]                   -- not element itself

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let matrix = fromLists . numerifyTextLines $ lines contents
   putStr . unlines . textifyMatrix $ toLists matrix
   writeFile "output.txt" contents