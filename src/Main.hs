module Main where

import System.IO
import Data.Char
import Data.Matrix (fromLists, toLists)
import Input.Text
import Output.Text
import Simulation.GameOfLife


main :: IO ()
main = do
   contents <- readFile "input.txt"
   let matrix = fromLists . numerifyTextLines $ lines contents
   let steppedMatrix = unlines . textifyMatrix . toLists $ simulateLife matrix
   putStr steppedMatrix
   writeFile "output.txt" steppedMatrix