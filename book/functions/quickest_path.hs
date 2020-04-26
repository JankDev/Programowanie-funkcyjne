import Data.List

data CrossRoad = CrossRoad Int Int Int deriving(Show)

main = do
      contents <- getContents
      let groups = map (\[a,b,c] -> CrossRoad a b c) . groupsOf 3 $ map read $ lines contents
      putStrLn $ show groups
      putStrLn $ show (quickestPath groups)

quickestPath :: [CrossRoad] -> [Int]
quickestPath = minimumBy (\u d -> compare (sum u) (sum d)) . foldl func [[],[]]
            where func [u,d] (CrossRoad a b c)
                              | min ((sum u) + a) ((sum d) + b+c) == ((sum u) + a) && min ((sum d) + b) ((sum u)+ a+c) == (sum d) + b = [u++[a],d++[b]]
                              | min ((sum u) + a) ((sum d) + b+c) == (sum d) +b+c && min ((sum d) + b) ((sum u)+ a+c) == (sum d) + b = [d++[b,c],d++[b]]
                              | min ((sum u) + a) ((sum d) + b+c) == (sum d) +b+c && min ((sum d) + b) ((sum u)+ a+c) == (sum u) + a + c = [d++[b,c],u++[a,c]]
                              | otherwise = [u++[a],u++[a,c]]
 
            


groupsOf :: Int -> [Int] -> [[Int]]
groupsOf 0 _ = undefined
groupsOf 1 l = [l]
groupsOf _ [] = []
groupsOf n l = (take n l):(groupsOf n $ drop n l)