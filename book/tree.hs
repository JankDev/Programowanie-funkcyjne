data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)

singleton' :: a -> Tree a
singleton' elem = Node elem (EmptyTree) (EmptyTree)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert elem EmptyTree = singleton' elem
treeInsert elem (Node n l r)
                | elem == n = Node elem l r
                | elem > n = Node n l (treeInsert elem r)
                | otherwise = Node n (treeInsert elem l) r

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem elem (Node n l r)
                | elem == n = True
                | elem > n = treeElem elem r
                | otherwise = treeElem elem l