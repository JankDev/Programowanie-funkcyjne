import System.Random

permutacje :: [a] -> [a]
permutacje = foldl (\acc x -> x)