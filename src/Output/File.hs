module Output.File
( saveSimulation
) where

import Data.Matrix (Matrix)
import Output.Text (stringifyMatrix)

saveSimulation :: (Fractional a, Ord a) => [String] -> [Matrix a] -> IO()
saveSimulation fs ms = foldr (>>) (putStr "") $ map (uncurry saveMatrix) $ zip fs ms

saveMatrix :: (Fractional a, Ord a) => String -> Matrix a -> IO()
saveMatrix f m = writeFile f (stringifyMatrix m)