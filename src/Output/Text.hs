module Output.Text
( stringifyMatrix
) where

import Data.Matrix (Matrix, mapPos, toLists)

stringifyMatrix :: (Fractional a, Ord a) => Matrix a  -> String
stringifyMatrix m = unlines . toLists $ textifyMatrix m

textifyMatrix :: (Fractional a, Ord a) => Matrix a -> Matrix Char
textifyMatrix m = mapPos textifyElement m

textifyElement :: (Fractional a, Ord a) => (Int, Int) -> a -> Char
textifyElement _ x
    | x <= 0.5  = ' '
    | x > 0.5   = '█'
