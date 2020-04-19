module Shapes
(
    Point(..), -- (..) export all value constructorsS
    Shape(..),
    area,
    baseRect,
    baseCircle
) where
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving(Show) -- value constrcutors are functions so we can mape them too

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r