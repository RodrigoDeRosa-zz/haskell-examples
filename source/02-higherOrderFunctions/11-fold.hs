-- Folding. It is reduce for Haskell.
-- We have an accumulator, a binary function and an array (which is
-- folded). Particularly, foldl starts folding from the left and foldr
-- starts from the right

-- FOLDL
-- Sum elements of a list
sum' :: (Num a) => [a] -> a
-- The lambda is the binary foo, 0 is the accumulator and xs the array
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Now this can be done more magically
sum'' :: (Num a) => [a] -> a
-- + is equivalent to the previous lambda
-- We don't need to put the param as the foo already expectes it
sum'' = foldl (+) 0

-- Existence of element in array
elem' :: (Eq a) => a -> [a] -> Bool
-- Here, the accumulator is False
elem' x xs = foldl (\acc y -> if x == y then True else acc) False xs

-- FOLDR. Here, we will need to put the accumulator as the second param of the
-- binary function

-- Reverse a list
-- Right fold
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []
-- Left fold
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

-- foldl1 and foldr1 assume that the accumulator can be the first item of the
-- array. For example:
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)
product' :: (Num a)
product' = foldr1 (*)
-- The following are not good implementations but work anyway
head' :: [a] -> a
head' = foldr1 (\x _ -> x)
last' :: [a] -> a
last' = foldl1 (\_ x -> x)
