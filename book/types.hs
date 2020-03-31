import Data.Map as Map

returnTwoThings :: [a] -> Either String Int
returnTwoThings l
            | Prelude.null l = Left "List empty"
            | otherwise = Right $ length l


phoneBook :: PhoneBook
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

type PhoneNumber = String
type Name = String
type PhoneBook = Map.Map Name PhoneNumber

type IntMap = Map.Map Int


infixr 5 :-: -- how tightly the operator binds and if it is left or right associative; Infix constructors must begin with a colon
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

{-
Because pattern matching works (only) on constructors, we can match for
normal prefix constructors or stuff like 8 or 'a' , which are basically construc-
tors for the numeric and character types, respectively.
-}
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
-- class (Eq a) => Num a where
{-
That’s all there is to subclassing—it’s just a class constraint on a class
declaration!
-}