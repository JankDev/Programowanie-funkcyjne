tell:: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

replicate' :: Int -> a -> [a]
replicate' n a 
    | n <= 0 = []
    | otherwise = [a]++replicate' (n-1) a

maxL :: (Ord a) => [a] -> a
maxL [] = error "Cant tell max on empty list"
maxL [x] = x
maxL (x:l) = max x (maxL l) 

take' :: Int -> [a] -> [a]
take' n l
    | null l = []
    | n <= 0 = []
    | otherwise = let (x:xs)=l in x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' ::(Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = if e ==x then True else elem' e xs

zip' :: [a] -> [b] -> [(a,b)]
zip' l r 
    | null l || null r = []
zip' (a:l) (b:r) = (a,b):zip' l r

quicksort ::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = [y | y <- xs, y < x ]
        larger = [y|y<-xs,y >=x]
    in quicksort smaller ++ [x] ++ quicksort larger

