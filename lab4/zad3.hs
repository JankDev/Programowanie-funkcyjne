findFirst :: (a -> Bool) -> [a] -> a
findFirst f = head . filter f