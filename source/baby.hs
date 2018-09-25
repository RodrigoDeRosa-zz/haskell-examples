-- A set of basic Haskell functions

doubleMe x = x*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

babyConcat x y = x ++ y -- x = "ho", y = "la" ||| x = [1,2], y = [3,4]

range x y = [x..y] -- x = 1, y = 10 ||| x = 'a', y = 'z'

byTwo lim = [x*2 | x <- [1..lim]]

onlyEvens lim = [x*2 | x <- [1..lim], mod x 2 == 0, mod x 10 /= 0]

boomBang array = [if mod x 2 == 0 then "BOOM!" else "BANG!" | x <- array]

arrayProduct a1 a2 = [x*y | x <- a1, y <- a2]

length' xs = sum [1 | _ <- xs]

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

-- Using pattern matching to make better functions:
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
-- Now with pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- It is possible to use pattern matching with list comprehensions too
sumParts :: (Num a) => [(a, a)] -> [a]
sumParts xs = [a+b | (a,b) <- xs]

-- Matching elements of a list by taking as variables only the ones we want
head' :: [a] -> a
head' [] = error "Can't call head on an empty list!!!"
head' (x:_) = x -- We are ignoring the rest of the list

-- Another example
sumFirstTwo :: (Num a) => [a] -> a
sumFirstTwo [] = error "Need at least two elements"
sumFirstTwo (x:[]) = error "Need at least two elements"
sumFirstTwo (x:y:_) = x + y

-- We can also get "The rest of the list"
length'' :: (Integral b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length' xs

-- Another example
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- It is also possible to eat the cake and have it too
capital :: String -> String
capital "" = "This is an empty string :("
capital word@(c:rest) = "The first letter of " ++ word ++ " is " ++ [c]

-- Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= underweight = "Underweigth"
  | bmi <= normal = "Normal"
  | bmi <= overweight = "Overweight"
  | otherwise = "You are a lovely whale :)"
  where bmi = weight / (height ^ 2)
        underweight = 18.5
        normal = 25
        overweight = 30

-- We can define a max function with guards
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- It is also valid to do it inline
min' :: (Ord a) => a -> a -> a
min' a b | a < b = a | otherwise = b


-- Using where nicely
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- It is also possible to declare functions in wheres !!!
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis list = [bmi w h | (w,h) <- list]
  where bmi weight height = weight / height ^ 2

-- Let | In
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2*topArea

-- You can put a let wherever you want to
-- In the middle of an operation
letShow = 4 * (let a = 9 in a + 1) + 2
-- For lists creation
letShow' = [let square x = x * x in (square 2, square 4, square 5)]
-- With multiple definitions
letShow'' = (let a = 100; b = 25 in a*b, let foo="Hey"; bar="You!" in foo ++ bar)
-- And this is equal to the last one
letShow''' = (let (a,b)=(100,25) in a*b, let (foo,bar)=("Hey", "You!") in foo ++ bar)

-- We can merge it all and do some unreadable shit
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' list = [bmi | (w,h) <- list, let bmi = w/h^2, bmi>=25.0]

-- CASE -> We can use this for pattern matching too. Let's see the head foo
head'' :: [a] -> a
head'' list = case list of [] -> error "Can't call head on an empty list!!!"
                           (x:_) -> x
