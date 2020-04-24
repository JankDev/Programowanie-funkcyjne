sum' :: Num a => [a] -> a
sum' = foldl (+) 0

product' :: Num a => [a] -> a
product' = foldl (*) 1

reverse' :: [a] -> [a]
reverse' = foldl (\acc next -> next:acc) []

and' :: [Bool] -> Bool
and' = foldl (&&) True 

or' :: [Bool] -> Bool
or' = foldl (||) False

head' :: [a] -> a
head' [] = error "No head on empty list"
head' (x:xs) = foldl (\acc next -> acc) x xs

last' :: [a] -> a
last' [] = error "No tail on empty list"
last' (x:xs) = foldl (\acc next -> next) x xs