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
