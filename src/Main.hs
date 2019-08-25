module Main where

import System.IO
import Input.Text (loadMatrix)
import Output.Text (saveSimulation)
import Simulation.GameOfLife


main :: IO ()
main = do
   fileContents <- readFile "input.txt"
   let matrix = loadMatrix fileContents
   let iterations = iterate simulateLife matrix
   let filenames = [ "out/" ++ (show x) ++ ".gol" | x <- [1..100]]
   saveSimulation filenames iterations