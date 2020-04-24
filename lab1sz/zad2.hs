data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving Show

empty' :: BinaryTree a-> Bool
empty' EmptyTree = True
empty' _ = False

singleton' :: a -> BinaryTree a
singleton' elem = Node elem (EmptyTree) (EmptyTree)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' elem EmptyTree = singleton' elem
insert' elem (Node n l r)
                | elem == n = Node elem l r
                | elem > n = Node n l (insert' elem r)
                | otherwise = Node n (insert' elem l) r

-- A binary tree is always binary
floors :: BinaryTree a -> Int
floors EmptyTree = 0
floors (Node _ EmptyTree EmptyTree) = 1
floors (Node n l r) = max ((floors l) + 1) ((floors r) + 1)

isBalanced :: BinaryTree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node n EmptyTree EmptyTree) = True
isBalanced (Node n l r) = (abs (floors l - floors r) < 2) && isBalanced l && isBalanced r

treeElem :: (Ord a) => a -> BinaryTree a -> Bool
treeElem _ EmptyTree = False
treeElem elem (Node n l r)
                | elem == n = True
                | elem > n = treeElem elem r
                | otherwise = treeElem elem l

tmap :: (Num a) => (a -> a) -> BinaryTree a-> BinaryTree a
tmap = fmap

leaves :: BinaryTree a -> [a]
leaves EmptyTree = []
leaves (Node n EmptyTree EmptyTree) = [n]
leaves (Node n l r) = (leaves l) ++ (leaves r)

nnodes :: BinaryTree a -> Int
nnodes EmptyTree = 0
nnodes (Node n l r) = (nnodes l) + (nnodes r) + 1


nsum :: (Num a) => BinaryTree a -> a 
nsum EmptyTree = 0;
nsum (Node n l r) = n + (nsum l) + nsum r

toString :: (Show a) => BinaryTree a -> String
toString EmptyTree = ""
toString (Node n EmptyTree EmptyTree) = show n ++ ""
toString (Node n l r) = show n ++"("++ toString l ++ "," ++ toString r ++ ")"

remove :: Tree a -> a -> Tree a
remove (Node n EmptyTree EmptyTree) n = EmptyTree
remove (Node n l r) n 
    | empty' l = r
    | empty' r = l
    | otherwise = merge l r

-- Because it wasn't stated that the tree should balanced we just merge without balancing it
merge :: Tree a -> Tree a -> Tree a
merge (Node n l r) (Node n2 l2 r2) = Node n l2 r2 --TODO

instance Functor BinaryTree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

tree = Node 4 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) (Node 6 (Node 5 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree))