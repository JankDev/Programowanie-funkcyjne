import System.IO
import System.Environment

main = do
    (command:args) <- getArgs
    putStrLn command