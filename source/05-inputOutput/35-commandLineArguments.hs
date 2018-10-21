import System.Environment
import System.Directory
import System.IO
import Data.List

main1 = do
  -- Accessing command line arguments is very easy:
  args <- getArgs
  putStrLn "The arguments are: "
  mapM putStrLn args
  -- And also getting the program name, if we needed it
  progName <- getProgName
  putStrLn "The program name is: "
  putStrLn progName

-- We will use our To Do list example to expand on this.
-- Our program will now give us the option to add tasks,
-- remove tasks and view the current tasks.
-- This will be done with 'add', 'remove' and 'view'.
-- We will also tell the program what file should it look.

-- This is a list that will match the received parameter with the function
-- that should be executed.
dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add), ("view", view), ("remove", remove)]
-- Add function
add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
-- View functions
view :: [String] -> IO()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks
-- Delete function
remove :: [String] -> IO()
remove [fileName, numberString] = do
  -- Open file
  handle <- openFile fileName ReadMode
  -- Create and open temp file to write
  (tempName, tempHandle) <- openTempFile "." "temp"
  -- Read file
  contents <- hGetContents handle
  -- Read number and delete the associated task
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  -- Write temp file
  hPutStr tempHandle $ unlines newTodoItems
  -- Close handlers
  hClose handle
  hClose tempHandle
  -- Remove original and rename temp to original
  removeFile fileName
  renameFile tempName fileName

-- Now we will run the needed functions with the received args
main = do
  -- We assign the first arg to 'command' and all the rest to args
  (command:args) <- getArgs
  -- We maybe get an executable action
  let (Just action) = lookup command dispatch
  -- We execute the corresponding function with the args
  action args

-- To run it we should do:
--      runhaskell 35-commandLineArguments.hs add FILENAME "a task we want to add"
--      runhaskell 35-commandLineArguments.hs view FILENAME
--      runhaskell 35-commandLineArguments.hs remove FILENAME INDEX_OF_TASK_TO_REMOVE
