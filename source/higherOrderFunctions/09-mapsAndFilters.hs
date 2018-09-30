-- The map function is the most common higher order function of 'em all
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Filter, another one of those
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs
-- QuickSort with filter!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggetSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggetSorted

-- Find largest number under 100.000 that's divisible by 3829
largestDivisible :: Int
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- takeWhile use example. It takes elements of an array until an element is
-- found that does not match the condition.
-- let elephants = takeWhile (\=' ') "elephants know how to party"

-- Sum of all odd squares that are smaller than 10.000
sumOfSquares :: (Integral a) => a
sumOfSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- Another way of doing it
sumOfSquares' :: (Integral a) => a
sumOfSquares' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz sequences. If a number is even, we divide it by two; if it is odd,
-- we multiply it by 3 and add 1. We repeat until we get to 1.
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd  n = n : collatz (n*3 + 1)

-- For numbers between 1 and 100, how many chains have a length greater than 15?
numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
  where isLong xs = length xs > 15
