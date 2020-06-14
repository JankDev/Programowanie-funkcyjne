import System.Random

main = do
    num <- randomIO :: IO Int
    sequence [guess num | i <- [1..3]]

guess num = do
    putStrLn "What's your guess"
    line <- getLine
    let userGuess = read line :: Int
    putStrLn $ compare' num userGuess
    
compare' :: Int -> Int -> String
compare' num guess
    | comparison == LT = "Too low"
    | comparison == GT = "Too high"
    | otherwise = "You got it"
        where comparison = num `compare` guess