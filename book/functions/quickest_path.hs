import Data.List

main = do
      contents <- getContents
      let groups = groupsOf 3 $ map read $ lines contents
      putStrLn $ show groups
      putStrLn $ show (quickestPath groups)

quickestPath :: [[Int]] -> [Int]
quickestPath = minimumBy (\u d -> compare (sum u) (sum d)) . foldl func [[],[]]
      where func [u,d] (a:b:c:xs) = case minimum [a,b,a+c,b+c] of a = u++[a]
                                                                  b = d++[]


groupsOf :: Int -> [Int] -> [[Int]]
groupsOf 0 _ = undefined
groupsOf 1 l = [l]
groupsOf _ [] = []
groupsOf n l = (take n l):(groupsOf n $ drop n l)