main = do
    line <- getLine
    if null line
        then return ()
    else do
        putStrLn $ reverseWords line
        main
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- return makes an I/O action out of a pure value.echo