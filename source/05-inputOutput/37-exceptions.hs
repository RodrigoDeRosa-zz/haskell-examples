-- Pure code can throw exceptions, but it they can only be caught
-- in the I/O part of our code (when we're inside a do block that
-- goes into main). That's because you don't know when (or if)
-- anything will be evaluated in pure code, because it is lazy and
-- doesn't have a well-defined order of execution, whereas I/O code does

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception

main = toTry `catch` handler
-- Function that could fail
toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
-- Way to handle failure
handler :: IOError -> IO ()
handler e
 -- Print error if it is a does not exists
 | isDoesNotExistError e = putStrLn "Whoops, had some trouble!"
 -- Explode otherwise
 | otherwise = ioError e