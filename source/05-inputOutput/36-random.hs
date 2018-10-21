import System.Random
import Control.Monad(when)

-- The problem with randomness is that, in theory, a function that receives
-- the same parameters should ALWAYS return the same result. But anyway,
-- Haskell has this solved.

-- This program returns a random string of length 20
main1 = do
  gen <- getStdGen -- This is an IO Action that returns a random number generator!
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

-- IMPORTANT! Once we ask for the random generator, we are not able to
-- get a new one ('... should ALWAYS return the same result... '). So
-- if we want another set of elements, given that we have an infinite
-- array, we just need to get the next 20s

main2 = do
  gen <- getStdGen
  let randomChars = randomRs ('a', 'z') gen
      (first20, rest) = splitAt 20 randomChars
      (second20, _) = splitAt 20 rest
  putStrLn $ first20
  putStrLn $ second20

-- We could get a different generator with newStdGen (after using get),
-- the problem is that the global generator is also modified and so
-- it won't return the same random chars as it did in the beginning.

-- Here is a program that lets you guess a number
main3 = do
  gen <- getStdGen
  askForNumber gen
-- Lets define our asking function
askForNumber :: StdGen -> IO()
askForNumber gen = do
  -- We get our random number and our new generator
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  -- We ask for a number
  putStr "Which number in the range from 1 to 10 am I thinking of?"
  numberString <- getLine
  when (not $ null numberString) $ do
    -- Now we verify if it was correct
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    -- Generate a new one and ask again!
    askForNumber newGen

-- We could also do this with:
main = do
  gen <- getStdGen
  -- Get our random number and ignore the returned generator
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of?"
  numberString <- getLine
  when (not $ null numberString) $ do
    -- Now we verify if it was correct
    let number = read numberString
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    -- Regenerate generators
    newStdGen
    -- Run again!
    main