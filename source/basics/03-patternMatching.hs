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
