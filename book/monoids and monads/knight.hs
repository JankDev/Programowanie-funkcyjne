type Position = (Int, Int)

moveKnight :: Position -> [Position]
moveKnight (c, r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: Position -> [Position]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second
{-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
--}
canReachIn3 :: Position -> Position -> Bool
canReachIn3 start end = end `elem` in3 start