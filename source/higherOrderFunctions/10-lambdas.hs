-- A lambda expression is defined with an inverted slash '\'. First we put the
-- params and with an arrow '->' we put the definition.
-- For example \a b -> a + b

-- Collatz from previous section
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd  n = n : collatz (n*3 + 1)

-- We can replace the where clause with a lambda expression
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))
-- We can also use pattern matching in lambdas
addCouples :: (Num a) => [(a, a)] -> [a]
addCouples array = map (\(a, b) -> a + b) array
