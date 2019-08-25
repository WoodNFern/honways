module Output.File
( saveMatrix
) where

import Data.Matrix (Matrix)
import Output.Text (stringifyMatrix)

saveMatrix :: (Fractional a, Ord a) => String -> Matrix a -> IO()
saveMatrix f m = writeFile f (stringifyMatrix m)