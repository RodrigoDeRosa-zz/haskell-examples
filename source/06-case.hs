-- CASE -> We can use this for pattern matching too. Let's see the head foo
head'' :: [a] -> a
head'' list = case list of [] -> error "Can't call head on an empty list!!!"
                           (x:_) -> x
