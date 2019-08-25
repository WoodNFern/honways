module Output.Text
( textifyMatrix
) where



textifyMatrix :: [[Double]] -> [String]
textifyMatrix (x:xs)    = (map textifyElement x) : (textifyMatrix xs)
textifyMatrix []        = []

textifyElement :: Double -> Char
textifyElement x
    | x <= 0.5  = ' '
    | x > 0.5   = '█'
