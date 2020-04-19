import System.Environment
import System.IO
import System.Directory
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = do
    (source:dest:_) <- getArgs
    copy source dest

copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "/tmp" "temp")
        (\tempName, tempHandle -> do
                hClose tempHandle
                removeFile tempName)
        (\tempName, tempHandle -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)