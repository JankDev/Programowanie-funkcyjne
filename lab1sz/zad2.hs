data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving Show
instance Functor BinaryTree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node n l r) = Node (f n) (fmap f l) (fmap f r)

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

-- is Binary Search tree
isBst :: (Ord a) => BinaryTree a -> Bool
isBst EmptyTree = True
isBst (Node n EmptyTree EmptyTree) = True
isBst (Node n (Node nl ll rl) (Node nr lr rr))
    | n <= nl || n > nr = False
    | otherwise = isBst ll && isBst rl && isBst lr && isBst rr

floors :: BinaryTree a -> Int
floors EmptyTree = 0
floors (Node _ EmptyTree EmptyTree) = 1
floors (Node n l r) = max ((floors l) + 1) ((floors r) + 1)

isBalanced :: BinaryTree a -> Bool
isBalanced EmptyTree = True
isBalanced (Node n EmptyTree EmptyTree) = True
isBalanced (Node n l r) = (abs (floors l - floors r) < 2) && isBalanced l && isBalanced r

search :: (Ord a) => a -> BinaryTree a -> Bool
search _ EmptyTree = False
search elem (Node n l r)
                | elem == n = True
                | elem > n = search elem r
                | otherwise = search elem l

tmap :: (Num a) => (a -> a) -> BinaryTree a-> BinaryTree a
tmap = fmap

data Kind = VLR | LVR | LRV | VRL | RVL | RLV deriving(Eq)

traverse' :: (Ord a) => BinaryTree a -> Kind -> [a]
traverse' (Node n EmptyTree EmptyTree) _ = [n]
traverse' EmptyTree _ = []
traverse' (Node n l r) kind
    | kind == VLR = [n]++(traverse' l kind) ++ (traverse' r kind)
    | kind == LVR = (traverse' l kind) ++ [n] ++ (traverse' r kind)
    | kind == LRV = (traverse' l kind) ++ (traverse' r kind) ++ [n]
    | kind == VRL = [n] ++ (traverse' r kind) ++ (traverse' l kind)
    | kind == RVL = (traverse' r kind) ++ [n] ++ (traverse' l kind)
    | kind == RLV = (traverse' r kind) ++ (traverse' l kind) ++ [n]
    | otherwise = error "Not known traversal"

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

findMin :: (Ord a) => BinaryTree a -> a
findMin (Node n EmptyTree _) = n
findMin (Node n l r) = findMin l

findMax :: (Ord a) => BinaryTree a -> a
findMax (Node n _ EmptyTree) = n
findMax (Node n l r) = findMax r

root :: (Ord a) => BinaryTree a -> a
root (Node n _ _) = n

remove ::(Ord a) => BinaryTree a -> a -> BinaryTree a
remove (Node n EmptyTree EmptyTree) elem | n == elem = EmptyTree
remove (Node n EmptyTree (Node n2 l r)) elem | n == elem = Node n2 l r
remove (Node n (Node n2 l r) EmptyTree) elem | n == elem = Node n2 l r
remove (Node n l r) elem
    | n == elem = let tmin = findMin r in Node tmin l (remove r tmin)
    | elem > root l = Node n l (remove r elem)
    | otherwise = Node n (remove l elem) r


merge ::(Ord a) => BinaryTree a -> BinaryTree a -> BinaryTree a
merge (Node n l r) (Node n2 l2 r2)
    | lmax < rmin = Node rmin (Node n l r) (remove (Node n2 l2 r2) rmin)
    | lmin > rmax = Node lmin (Node n2 l2 r2) (remove (Node n l r) lmin)
    | (lmin == rmax)  = Node lmin (remove (Node n l r) lmin) (remove (Node n2 l2 r2) lmin)
    | (lmax == rmin)  = Node lmax (remove (Node n l r) lmax) (remove (Node n2 l2 r2) lmax)
    | otherwise = (Node (max n n2) (merge l r2) (merge l2 r)) 
        where lmax = (findMax (Node n l r)) 
              rmin = (findMin (Node n2 l2 r2))
              lmin = (findMin (Node n l r)) 
              rmax = (findMax (Node n2 l2 r2))
              

tree = Node 4 treeL treeR
treeL = (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree)
treeR = (Node 6 (Node 5 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree))