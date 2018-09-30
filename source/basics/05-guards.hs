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
