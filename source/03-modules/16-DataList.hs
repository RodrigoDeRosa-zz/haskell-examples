import Data.List

-- Once you imported a module, you can use the functions that are defined
-- inside it
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- The nub function removes all duplicates from an array

-- A look at some Data.List functions

-- intersperse. This function adds an element between every pair of elements in
-- a given array.
showIntersperseString = intersperse '.' "MONKEY"
-- => "M.O.N.K.E.Y"
showIntersperseNums = intersperse 0 [1,2,3,4]
-- => [1,0,2,0,3,0,4]

-- intercalate. This function takes an array and an array of arrays; it inserts
-- the given array between every pair of arrays in the second one.
showIntercalateString = intercalate " " ["hey","there","guys"]
-- => "hey there guys"
showIntercalateNums = intercalate [0,0] [[1,2], [3,4], [5,6]]
-- => [1,2,0,0,3,4,0,0,5,6]

-- transpose. Transposes a list of lists as if it was a matrix.
showTransposeString = transpose ["hey","there","guys"]
-- => ["htg", "ehu", "yey", "rs", "e"]
showTransposeNums = transpose [[1,2,3], [4,5,6], [7,8,9]]
-- => [[1,4,7], [2,5,8], [3,6,9]]

-- A good example of use for transpose is the sum of polinmials. Say we have
-- 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 and we want to sum them.
-- We can express them as arrays and sum them that way
sumPolinomials = map sum $ transpose [[0,3,5,9], [10,0,0,9], [8,5,1,-1]]

-- foldl' and foldl1' do the same as the non prime functions but ARE NOT LAZY,
-- so this functions are used when infinite lists may be very long and you want
-- to avoid stack overflows
overflowSum = foldl1 (+) [1..20000000]
nonOverflowSum = foldl1' (+) [1..20000000]

-- concat. This flattens a list.
flatString = concat ["foo", "bar", "cat"]
-- => "foobarcat"
flatNums = concat [[3,4,5], [1,2]]
-- => [3,4,5,1,2]

-- concatMap. This is a flatMap
flatMapNums = concatMap (replicate 2) [1..3]
-- => [1,1,2,2,3,3]

-- any | all. Returns True or False if all or any elements in an array match a
-- condition.
exampleAny = any (==4) [1,2,3,4]
-- => True
exampleAll = all (==4) [1,2,3,4]
-- => False

-- iterate. Takes a function and a starting value. It applies the function to
--the starting value, then it applies that function to the result, then it
-- applies the function to that result again, etc. It returns all the results
-- in the form of an infinite list.
iteratePows = take 10 $ iterate (*2) 2

-- splitAt. Takes a number and a list and splits the list in two; the first part
-- is until the index=number and the second part is the rest of the list
splitString = splitAt 3 "hey man"
-- => ("hey", " man")
splitString' = splitAt 100 "hey man"
-- => ("hey man")

-- takeWhile. We know this one!
-- dropWhile. This is kind of the inverse of the previous one; it removes
-- elements from a list until it finds the first one that doesn't match the
-- condition.
exampleDropWhile = dropWhile (<3) [1,1,1,2,2,3,4,5]
-- => [3,4,5]

-- span. This is kind of a reloaded takeWhile. It returns two lists: one with
-- the result of the takeWhile and another one with the rest.
exampleSpan = span (/=' ') "This is a sentence"
-- => ("This", " is a sentence")

-- break. This is the dropWhile of the span
exampleBreak = break (==' ') "This is a sentence"
-- => ("This", " is a sentence")

-- sort. Sort is, well, sort.
exampleSort = sort [3,1,2,4,2]
-- => [1,2,2,3,4]

-- group. Group takes a list and returns a list of lists of elements that are
-- equal and adjacent in the original one
exampleGroup = group [1,1,1,1,2,2,3,4,4,4,1,1]
-- => [[1,1,1,1], [2,2], [3], [4,4,4], [1,1]]

-- inits and tails. These are like init and tail but they recursively apply
-- those functions to a list until there is nothing left.
exampleInits = inits "w00t"
-- => ["","w","w0","w00","w00t"]
exampleTails = tails "w00t"
-- => ["w00t","00t","0t","t",""]
pairInitTail = let w = "w00t" in zip (inits w) (tails w)

-- We can use these two to search in an array
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x ->
          if take nlen x == needle then True else acc) False (tails haystack)
-- We check if the n (length of needle) first elements of each tail of the
-- haystack is exactly the needle.

-- isPrefixOf | isSuffixOf | isInfixOf. These two check if one array is suffix
-- or infix of another array
examplePrefixOf = "hey" `isPrefixOf` "Oh, hey there!"
-- => False
exampleInfixOf = "hey" `isInfixOf` "Oh, hey there!"
-- => True
exampleSuffixOf = "there!" `isInfixOf` "Oh, hey there!"
-- => True

-- elem | notElem. These check for presence in an array

-- partition. Takes a boolean function and an array and returns a pair of arrays.
-- In the first one are the elements of the given array that match the condition
-- and in the second one the ones that don't
examplePartition = partition (>3) [1..6]
-- => ([4,5,6], [1,2,3])

-- find. Takes a list and a predicate and returns the first element that matches
-- the predicate, wrapped in a Maybe object.
exampleFind = find (>2) [1,2,3,4]
-- => Just 4
exampleFind' = find (>3) [1,2,3]
-- => Nothing

-- elemIndex. It is like elem but instead of a Boolean, it maybe returns the
-- index.
elemIndexIn123 x = x `elemIndex` [1,2,3]

-- elemIndices. The same as before but for a list of elements. Instead of Nothing,
-- it returns an empty list

-- findIndex and findIndices are analog to elemIndex and elemIndices but they
-- receive a predicate
exampleFindIndex = findIndex (==4) [1,2,3,6,5,4]
-- => Just 5

-- zip3,zip4,...,zip7. These are just like zip but with n lists as parameter.
-- The same applies for zipWith.
exampleZipWith3 = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
-- => [7,9,8]
exampleZip4 = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
-- => [(2,2,5,2),(3,2,5,2),(3,2,3,2)]

-- lines. This is for text manipulation, returns a list of strings separated
-- by '\n's
exampleLines = lines "first line\nsecond line\nthird line"
-- => ["first line", "second line", "third line"]

-- unlines. This is the inverse of the previous one
exampleUnlines = ["first line", "second line", "third line"]
-- => "first line\nsecond line\nthird line"

-- word, words and unwords. This is the same as lines but with words and spaces
exampleWords = words "hey these are the words in this sentence"
-- => ["hey","these","are","the","words","in","this","sentence"]

-- delete. Deletes a given element from an array.
exampleDelete = delete 'h' "hey there"
-- => "ey there"
exampleDelete' = delete 'h' . delete 'h' $ "hey there"
-- => "ey tere"

-- \\. List difference function, this removes the elements from the second
-- array from the first one.
exampleDifference = [1..10] \\ [2,5,9]
-- => [1,3,4,6,7,8,10]

-- union. Works like a function on sets but with arrays. It's like a join
-- but it removes duplicates
exampleUnion = [1..7] `union` [4..9]
-- => [1,2,3,4,5,6,7,8,9]
exampleUnion' = "hey man" `union` "man what's up"
-- => "hey manwt'sup"

-- intersect. It's like a set intersection. Just that.

-- insert. This is a simple insert but it inserts ordered
exampleInsert = insert 4 [1,2,3,4,5]
-- => [1,2,3,4,4,5]

-- length, take, drop, splitAt, index and replicate all take Int as parameters.
-- Because of this, we have genericXXX for all of the functions, that receive
-- any type of parameter.

-- Also, we have nubBy, deleteBy, unionBy, intersectBy and gropuBy. The difference
-- here is that the non-By cases use (==) as comparator and with the By cases
-- we can define the equality function
exampleGroupBy = groupBy (\x y -> (x > 0) == (y > 0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
-- => Groups of negatives, then positives, then negatives

-- on. This is an infix function that creates a function.
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)
-- So, for example, (==) on (>0) is the same as \x y -> (x>0) == (y>0)
exampleGroupByOn = groupBy ((==) `on` (>0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

-- Analog to these we have sortBy, insertBy, maximumBy and minimumBy.
exampleSortBy = sortBy (compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
-- => [[],[2],[2,2],[1,2,3],[3,4,5,3],[5,4,5,4,4]
