-- Scan works like fold but it also adds every intermediate accumulator status'
-- We have scanl, scanr and also scanl1 and scanr1
-- This function is used to monitor de progression of a function that can be
-- implemented as a fold.

-- For example: How many elements does it take for the
-- sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
