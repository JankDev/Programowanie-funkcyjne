usunduplikaty ::(Eq a) => [a] -> [a]
usunduplikaty [] = []
usunduplikaty [x] = [x]
usunduplikaty (x:xs)
    | x `elem` xs = usunduplikaty xs
    | otherwise = x:(usunduplikaty xs)
