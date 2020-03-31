import Data.List 

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (l:ls) (r:rs) = f l r:zipWith' f ls rs 

flip' :: (a->b->c)->b->a->c
flip' f x y = f y x

map' :: (a->b)->[a]->[b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs) = if (f x) == True then x:filter' f xs else filter' f xs

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0


sum' :: (Num a) => [a] -> a
sum' l = foldl add 0 l -- or (\acc x -> acc + x) in foldl the acc is first in the accumulator function
    where add x acc = acc + x


reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) [] -- important in the accumulator function the first arg is the next value and the second tha accumulated value

-- : is much faster than ++

filter2' :: (a -> Bool) -> [a] -> [a]
filter2' p = foldr (\x acc -> if p x then x : acc else acc) []

group' :: (Ord a) => [a] -> [[a]]
group' l  = foldl func [] (sort l)
        where
            func acc next
                    | null acc = acc ++ [[next]]
                    | otherwise = if (head (last acc)) == next then (init acc) ++ [(last acc ++ [next])] else acc ++ [[next]]