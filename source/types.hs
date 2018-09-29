-- String is equivalent to [char]
removeNonUpper :: String -> String
removeNonUpper st = [c | c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Our factorial functions uses Haskell's pattern matching to define it's
-- behaviour with different inputs
-- Integral includes Int and Integer (Integer is an unbounded Int)
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
