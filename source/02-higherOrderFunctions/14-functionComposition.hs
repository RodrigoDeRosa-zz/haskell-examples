-- Composition is done with the dot function (.). It's definition is

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- For example, let's pass every number in a list to negative
allToNegativeLambda = map (\x -> negate (abs x)) [5, -3, 12, -1, 0]
-- This is the same as doing
allToNegativeComposed = map (negate . abs) [5, -3, 12, -1, 0]

-- As composition is right associative, we can compose many functions at a time
negateSums = map (negate . sum . tail) [[1..5], [3..6], [1..7]]
-- Here we are keeping only the tail ([2,3,4,5] in the first one), then summing
-- the elements and finally negating them

-- It can be also mixed with application
longFoo = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- Can be written as
longFoo' = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

-- Another example
fn x = ceiling (negate (tan (cos (max 50 x))))
-- Can be written as
fn' = ceiling . negate . tan . cos . max 50

-- The odd squares problem could be written as
oddSquareSum :: Integer
oddSquareSum =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit

-- And also, less readable
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
