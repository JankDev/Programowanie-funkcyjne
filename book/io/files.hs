import System.IO
import Control.Exception

main = do 
    handle <- openFile "girlfriend.txt" ReadMode --returns a Handle
    contents <- hGetContents handle
    putStr contents
    hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' fname mode f = bracket (openFile fname mode) close (\handle -> f handle)
        where close handle = hClose handle

{-
main = do
contents <- readFile "girlfriend.txt"
putStr contents
-} -- readFile closes handle for us