bmi :: Double -> String
bmi v
    | v <= 18.5 = "You are skinny as hell"
    | v <= 25 = "You a normie"
    | otherwise = "You fat"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

power :: Double -> Double -> String
power mass accel
    | power < 50.0 = "You are weak"
    | power > 50.0 = "You are strong"
    where power = mass * accel

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

maxL :: (Ord a) => [a] -> a
maxL [] = error "Cant tell max on empty list"
maxL [x] = x
maxL (x:l) = max x (maxL l) 