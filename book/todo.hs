import System.IO
import Data.List
import System.Directory

main = do
    withFile "todo.txt" ReadWriteMode todoShell


todoShell fileHandle = do
    putStrLn "What do you want to do (add/delete/exit/show)?"
    action <- getLine
    case action of 
                   "add" -> do
                                addTodo
                   "delete" -> do
                                deleteTodo fileHandle
                   "show" -> do 
                                showTodos fileHandle
                   "exit" -> return ()


showTodos fileHandle = do
    contents <- hGetContents fileHandle
    let fileLines = lines contents
        numberedTasks = zipWith (\line n -> show n ++ " " ++ line) fileLines [0..]
    putStrLn "These are your todos:"
    mapM_ putStrLn numberedTasks

deleteTodo fileHandle = do
    putStrLn "Which one you want to delete?"
    line <- getLine
    contents <- hGetContents fileHandle
    let fileLines = lines contents
        lineNumber = read line
        newTodos = unlines $ delete (fileLines !! lineNumber) fileLines
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodos
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

addTodo = do
    putStrLn "Write your todo:"
    line <- getLine
    appendFile "todo.txt" (line ++ "\n")


