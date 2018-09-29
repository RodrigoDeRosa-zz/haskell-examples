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
