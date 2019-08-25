module Output.Text
( stringifyMatrix
, saveSimulation
, saveMatrix
) where

import Data.Matrix (Matrix, mapPos, toLists)

saveSimulation :: (Fractional a, Ord a) => [String] -> [Matrix a] -> IO()
saveSimulation fs ms = foldr (>>) (putStr "") $ map (uncurry saveMatrix) $ zip fs ms

saveMatrix :: (Fractional a, Ord a) => String -> Matrix a -> IO()
saveMatrix f m = writeFile f (stringifyMatrix m)

stringifyMatrix :: (Fractional a, Ord a) => Matrix a  -> String
stringifyMatrix m = unlines . toLists $ textifyMatrix m

textifyMatrix :: (Fractional a, Ord a) => Matrix a -> Matrix Char
textifyMatrix m = mapPos textifyElement m

textifyElement :: (Fractional a, Ord a) => (Int, Int) -> a -> Char
textifyElement _ x
    | x <= 0.5  = ' '
    | x > 0.5   = '█'
