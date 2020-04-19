data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

isEmpty :: Tree a-> Bool
isEmpty EmptyTree = True
isEmpty _ = False

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

leaves :: Tree a -> [a]
leaves EmptyTree = []
leaves (Node n EmptyTree EmptyTree) = [n]
leaves (Node n l r) = (leaves l) ++ (leaves r)

nnodes :: Tree a -> Int
nnodes EmptyTree = 0
nnodes (Node n l r) = (nnodes l) + (nnodes r) + 1


nsum :: (Num a) => Tree a -> a 
nsum EmptyTree = 0;
nsum (Node n l r) = n + (nsum l) + nsum r

toString :: (Show a) => Tree a -> String
toString EmptyTree = ""
toString (Node n EmptyTree EmptyTree) = show n ++ ""
toString (Node n l r) = show n ++"("++ toString l ++ "," ++ toString r ++ ")"


instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)


tree = Node 4 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 6 (Node 5 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree))