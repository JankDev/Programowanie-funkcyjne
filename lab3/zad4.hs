import Data.List

koduj_rle ::(Eq a) => [a] -> [(Int, a)]
koduj_rle xs = map (\l -> (length l, head l)) $ group xs