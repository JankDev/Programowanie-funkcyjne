import Data.List

lsortuj :: [[a]] -> [[a]]
lsortuj = sortBy (\a b -> compare (length a) (length b) )