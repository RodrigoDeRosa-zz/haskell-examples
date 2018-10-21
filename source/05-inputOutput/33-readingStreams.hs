-- getContents is an IO Action that read everything from the standard
-- input until it encounters an end of file character.
-- The cool thing about it is that it is lazy, so it won't store
-- all the input in the variable but will read it as it goes.
import Control.Monad
import Data.Char

-- We will write to equivalent functions:

-- The first one with getLine
main1 = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l

-- And the second one with getContents
main2 = do
  contents <- getContents
  putStr (map toUpper contents)

-- Both of these should be run doing:
--    ghc --make 33-filesAndStreams
--    cat haiku.txt | runhaskell 33-filesAndStreams.hs
-- And both will do the same. We just encapsulated the 'forever' inside
-- the getContents


-- Let's make a program that takes some input and prints out only those lines
-- that are shorter than 10 chars
main3 = do
  contents <- getContents
  putStr $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

-- Now this can be even happier, we can use the function 'interact'
-- interact takes a function of type String -> String and returns an
-- IO Action that will take some input, run the function and print it
main4 = interact shortLinesOnly

-- And we can do it even shorter
main5 = interact $ unlines . filter ((<10) . length) . lines
-- Of course this is not very happy, but it works

-- Let's make a palindrome detector
respondPalindromes =
  unlines .
  map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") .
  lines
  where isPalindrome xs = xs == reverse xs

main = interact respondPalindromes