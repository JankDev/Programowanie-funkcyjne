quickestPath :: [(a,b,c)] -> [Int]
quickestPath cr = foldl func [] cr
        where func [] n = [n]
              func _ (a,b,c)
              
