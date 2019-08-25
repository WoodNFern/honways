module Input.Text
( numerifyTextLines
) where



numerifyTextLines :: [String] -> [[Double]]
numerifyTextLines (x:xs)    = (map numerifyTextLine x) : (numerifyTextLines xs)
numerifyTextLines []        = []

numerifyTextLine :: Char -> Double
numerifyTextLine x
    | x == '0'  = 0
    | x == '1'  = 1
    | x == ' '  = 0
    | x == '█'  = 1