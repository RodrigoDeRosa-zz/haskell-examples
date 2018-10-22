-- Imagine we have a guy with a pole walking in a rope and he makes birds land
-- on both sides of them. The rule is that he can't have a big difference between
-- one side and the other, otherwise he would fall!

-- We define our new types
type Birds = Int
type Pole = (Birds, Birds)

-- If he steps on a banana, he falls
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- Now we make a function to land on the left
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing
-- And one to land on the right
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

-- What will this do?
example1 = return (0, 0) >>= landRight 1 >>= landLeft 2 >>= landRight 3
-- What will this do?
example2 = return (0, 0) >>= landRight 3 >>= landRight 3
-- What will this do?
example3 = return (0, 0) >>= landRight 1 >>= landLeft 3 >>= landRight 3 >>= landRight 3
-- What will this do?
example4 = return (0, 0) >>= landRight 2 >> banana >>= landLeft 2
