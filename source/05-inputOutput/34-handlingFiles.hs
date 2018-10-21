-- Now we will, instead of receiving a stream of chars, handle
-- files within our program.
-- Our first program will read and print the lines of a file.

import System.IO
import System.Directory
import Data.List
import Data.Char

main1 = do
  handle <- openFile "data/girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- We can also this with 'withFile'. This functions receives a FilePath
-- and a lambda and it applies the lambda to the file's handler
main2 = do
  withFile "data/girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
-- It does the opening and closing job for us :)

-- Of course, all of this could be done with 'readFile'
main3 = do
  contents <- readFile "data/girlfriend.txt"
  putStr contents

-- And with 'writeFile' we could output the result to another file
main4 = do
  contents <- readFile "data/girlfriend.txt"
  writeFile "data/girlfriendCaps.txt" (map toUpper contents)

-- There's a very similar function called appendFile and, well, it
-- does what you would expect it to
main5 = do
  todoItem <- getLine
  appendFile "data/todo.txt" (todoItem ++ "\n")


-- Let's now do a program that allows us to delete from de To Do list
main = do
  -- Open file
  handle <- openFile "data/todo.txt" ReadMode
  -- Create temp file to store the new contents
  (tempName, tempHandle) <- openTempFile "." "temp" -- directory, fileName
  -- Read contents
  contents <- hGetContents handle
  -- Assign a number to each line
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  -- Show tasks
  putStrLn "These are your TO-DO items: "
  putStr $ unlines numberedTasks
  -- Ask for a number
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  -- Delete from the element with given index from the list of tasks
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  -- Write the new contents in the temp file
  hPutStr tempHandle $ unlines newTodoItems
  -- Close handles
  hClose handle
  hClose tempHandle
  -- Delete old file and rename the temp
  removeFile "data/todo.txt"
  renameFile tempName "data/todo.txt"