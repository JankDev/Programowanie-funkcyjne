import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map

howMany :: String -> [(String, Int)]
howMany = map (\l->(head l,length l)) . group . sort . words 

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)


encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift = encode $ negate shift

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

findNatural :: Int -> Maybe Int
findNatural n = find (\x -> digitSum x == n) [1..]

findKey :: (Eq a) => a -> [(a,b)] -> Maybe b
findKey _ [] = Nothing
findKey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

returnTwoThings :: [a] -> Either String Int
returnTwoThings l
            | null l = Left "List empty"
            | otherwise = Right $ length l
