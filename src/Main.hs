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

simulateLife :: (Num a, Show a, Ord a) => Matrix a -> Matrix a
simulateLife m = mapPos (isAlive m) $ neighborCountMatrix m

isAlive :: (Num a, Show a, Ord a) => Matrix a -> (Int, Int) -> a -> a
isAlive m (x, y) v
    | v < 2 || v > 3    = 0
    | v == 2            = getElem x y m
    | v == 3            = 1

neighborCountMatrix :: (Num a, Show a, Ord a) => Matrix a -> Matrix a
neighborCountMatrix m = mapPos (sumOfNeighbors m) m

sumOfNeighbors :: (Num a, Show a, Ord a) => Matrix a -> (Int, Int) -> a -> a
sumOfNeighbors m (x, y) _ = foldr (+) 0 $ getNeighborElements (x, y) m

getNeighborElements :: (Int, Int) -> Matrix a -> [a]
getNeighborElements (x, y) m = map (\(r, c) -> getElem r c m) $ getNeighborIndices (x, y) m

getNeighborIndices :: (Int, Int) -> Matrix a -> [(Int, Int)]
getNeighborIndices (x, y) m = [ (r, c) | r <- [1 .. nrows m], c <- [1 .. ncols m],
                                         (abs (x - r)) <= 1, (abs (y - c)) <= 1,       -- all elements in radius 1
                                         not $ (x == r) && (y == c)]                   -- not element itself

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let matrix = fromLists . numerifyTextLines $ lines contents
   let steppedMatrix = unlines . textifyMatrix . toLists $ simulateLife matrix
   putStr steppedMatrix
   writeFile "output.txt" steppedMatrix