-- High order functions are functions that return other functions.
-- In Haskell, all functions are uniparameter functions. For example, the
-- following case is a actually a function that returns a function, which, at
-- the same time returns another function, that finally returns the result.
-- This is a -> (a -> (a -> a)).
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
-- We can pass less arguments in order to get a function instead of a result

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9 -- Now this is a -> (a -> a)

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2 -- Now this is a -> a

--multThree 2 9 10 -- Three parameter function
--multTwoWithNine 2 10 -- Two parameter function
--multWithEighteen 10 -- One parameter function

-- The following two things are equivalent
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
-- Here we are returning the higher order function that expects a second param
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

-- We can also use infix functions
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperChar :: Char -> Bool
isUpperChar = (`elem` ['A'..'Z'])

subtractFour :: (Num a) => a -> a
subtractFour = (subtract 4)

-- IMPORTANT! Functions are not instances of Show, so they cannot be printed.
-- We need to always assign them with a let clause

-- Super higher order
-- We have here a function that receives a function and a parameter and executes
-- the function recursively twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Obviously, as the signature says, the function has to be Int -> Int, or
-- String -> String, or Ord -> Ord, etc...

-- This functions zips arrays but applying the received function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
-- We apply f to the head of each array and call our function recursively
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- This function returns a function with parameters inversed
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
