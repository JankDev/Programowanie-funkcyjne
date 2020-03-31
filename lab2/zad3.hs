usunduplikaty ::(Eq a) => [a] -> [a]
usunduplikaty = foldl (\acc next -> if next `elem` acc then acc else acc ++ [next]) []