import Data.Char

rpn :: String -> Double
rpn = head . foldl parse [] . words

parse :: [Double] -> String -> [Double]
parse (x:y:xs) "*"  = (y * x):xs
parse (x:y:xs) "+"  = (y + x):xs
parse (x:y:xs) "-"  = (y - x):xs
parse (0:y:xs) "/"  = error "Cannot devide by 0"
parse (x:y:xs) "/"  = (y / x):xs
parse acc next = (read next :: Double):acc