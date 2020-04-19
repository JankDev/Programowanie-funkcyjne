collatz :: Int -> [Int]

collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | otherwise = n(digitToInt next) :: Double:collatz (n*3 +1)