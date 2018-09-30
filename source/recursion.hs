-- Max of an ordered array
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum in an empty list"
-- Only one element, then that element is the maximum
maximum' [x] = x
-- Search for the maximum recursively
maximum' (x:rest) = max x (maximum' rest)


-- Replicate element n times
replicate' :: (Integral i, Ord a) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

-- Take elements from an array
take' :: (Integral i, Ord a) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:rest) = x : take' (n-1) rest

-- Reverse an array
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:rest) = reverse' rest ++ [x]

-- Infinite array producer
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- Array pair generator --> [1,5] [2, 3, 4] = [(1,2), (5,3)]
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- Presence of element in array
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (e:rest)
  | x == e = True
  | otherwise = elem' x rest

-- QuickSort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:rest) =
      -- Sort the list of elements smaller than X
  let smallerSorted = quicksort [a | a <- rest, a <= x]
      -- Sort the list of elements bigger than x
      biggerSorted = quicksort [a | a <- rest, a > x]
     -- Join results
  in smallerSorted ++ [x] ++ biggerSorted
