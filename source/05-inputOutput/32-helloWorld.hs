import Data.Char

-- This will be our first actual Haskell program!
-- To compile it, we need to execute (in a shell located in our current folder):
--      ghc --make 32-helloWorld
-- Once it is compiled, we can execute it with:
--      ./32-helloWorld
-- Also, you can run it without compiling with:
--      runhaskell 32-helloWorld.hs
-- Doing this will always execute a function called 'main' which should be defined.
main1 = putStrLn "Hello, World!"

-- The function's definition is:
--      putStrLn :: String -> IO ()
-- An I/O action is something that, when performed, will carry
-- out an action with a side-effect (that's usually either
-- reading from the input or printing stuff to the screen) and
-- will also contain some kind of return value inside it.

-- We use 'do' syntax to glue IO Actions together:
main2 = do
  putStrLn "Hello, what's your name?"
  name <- getLine -- This means "perform the I/O action getLine and then bind its result value to name"
  putStrLn ("Hey " ++ name ++ " you rock!")

-- We can also add 'let in' syntax inside our 'do':
main3 = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hello " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- Now we will make a program that continuously read from console and prints the input reversed.
-- It will stop when it gets a blank line.
reverseWords :: String -> String
reverseWords = unwords . map reverse .words

main4 = do
  line <- getLine
  if null line then return ()
  else do
    putStrLn $ reverseWords line
    main

-- return DOES NOT FINISH THE EXECUTION. It just generates an IO Action by wrapping
-- the value it receives! The following will execute 'till the end:
main5 = do
  return ()
  return "HAHAHAHA"
  line <- getLine
  return "BLAH BLAH"
  return 4
  putStrLn line

-- return is kind of the opposite of <-
main6 = do
  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

-- And this last thing is equivalent to:
main = do
  let a = "hell"
      b = "yeah"
  putStrLn $ a ++ " " ++ b