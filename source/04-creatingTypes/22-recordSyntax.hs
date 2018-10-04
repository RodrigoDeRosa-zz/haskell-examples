-- We can also define types this way:
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

-- Then we can do the following:
-- :t flavor ==> flavor :: Person -> String

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
-- When we create data types with record syntax, we can instantiate them like:
showCar = Car {company="Ford", model="Mustang", year=1967}
-- It is not necessary to mantain the order, as long as we put everything
