-- The polish calculator receives a String (the sequence of elements to be analyzed) and
-- returns a number (which is the result)

-- We will represent the stack with a list, being the top of the stack the head of the list
-- (because it is faster to insert at the beginning than at the end).
-- So our function will receive a string like "10 3 2 + 2 * -", it will pass it to something like
-- ["10", "3", "2", "+", "2", "*", "-"].

-- [] is the starting accumulator
-- 'words expression' generates the list we talked about
-- head will return the result, as it will be the first element of the stack

import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*"    = (x * y):ys
        foldingFunction (x:y:ys) "+"    = (x + y):ys
        foldingFunction (x:y:ys) "-"    = (x - y):ys
        foldingFunction (x:y:ys) "/"    = (x / y):ys
        foldingFunction (x:y:ys) "^"    = (x ** y):ys
        foldingFunction (x:xs)   "ln"   = log x:xs
        foldingFunction xs       "sum"  = [sum xs]
        foldingFunction xs numberString = read numberString:xs

main = do
  putStrLn "Please insert the expression to be calculated: "
  expression <- getLine
  putStrLn $ "Result: " ++ show (solveRPN expression)

-- Example:
-- 10 4 3 + 2 * -
-- [] 10 -> [10]
-- [10] 4 -> [4, 10] (BECAUSE OF 'number:xs'!!!!)
-- [4, 10] 3 -> [3, 4, 10]
-- [3, 4, 10] + -> [7, 10]
-- [7, 10] 2 -> [2, 7, 10]
-- [2, 7, 10] * -> [14, 10]
-- [14, 10] - -> [4]
-- head [4] --> 4