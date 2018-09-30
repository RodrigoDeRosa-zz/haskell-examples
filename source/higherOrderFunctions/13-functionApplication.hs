-- $ function is called function application, the following is the definition:

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- This symbol is used because it has the lowest precedence of all. It is used
-- to avoid a lot of partentheses. For example:
-- sum (map sqrt [1..130]) is the same as sum $ map sqrt [1..130]

-- We can put it wherever we want, using it as a function. For example, in a map
mapWithApplication = map ($ 3) [(4+), (10*), (^2), sqrt]
-- Here we are expecting to map an array of functions that need a parameter, so
-- we will pass 3 as that parameter
