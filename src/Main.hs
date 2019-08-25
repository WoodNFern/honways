module Main where

import System.IO
import Data.Char
import Data.Matrix (fromLists, toLists)
import Input.Text
import Output.Text
import Output.File (saveMatrix)
import Simulation.GameOfLife


main :: IO ()
main = do
   contents <- readFile "input.txt"
   let matrix = fromLists . numerifyTextLines $ lines contents
   let iterations = iterate simulateLife matrix
   let filenames = [ "out/" ++ (show x) ++ ".gol" | x <- [1..100]]
   let ioActions = map (uncurry saveMatrix) $ zip filenames iterations
   foldr (>>) (putStr "") ioActions