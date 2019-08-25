module Input.Text
( loadMatrix
, numerifyTextLines
) where

import Data.Matrix (Matrix, fromLists)



loadMatrix :: String -> Matrix Double
loadMatrix c = fromLists . numerifyTextLines $ lines c

numerifyTextLines :: [String] -> [[Double]]
numerifyTextLines (x:xs)    = (map numerifyTextLine x) : (numerifyTextLines xs)
numerifyTextLines []        = []

numerifyTextLine :: Char -> Double
numerifyTextLine x
    | x == '0'  = 0
    | x == '1'  = 1
    | x == ' '  = 0
    | x == '█'  = 1