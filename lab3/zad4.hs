import Data.List

koduj_rle ::(Eq a) => [a] -> [(Int, a)]
koduj_rle = map (\l -> (length l, head l)) . group