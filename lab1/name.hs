main::IO()
main = do
    putStrLn "Tell your name"
    name <- getLine
    putStrLn ("Welcome " ++ name)
