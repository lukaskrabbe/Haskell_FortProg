import Prelude hiding (elem)

-- Predicate wich is satisfied if an element occures in a list
-- elem ::  Char -> [Char] -> Bool
-- elem ::  Bool -> [Bool] -> Bool
-- elem ::  Int -> [Int] -> Bool
elem :: Eq a => a -> [a] -> Bool
elem y []       = False
elem y (x:xs)   =  y == x || elem y xs


data BW = Black | White
 --   deriving (Eq, Ord, Show) -- Standart Klassen welche das Haskel kennt

instance Eq BW where
    Black == Black = True
    White == White = True
    _     == _     = False
    x /= y         = not(x == y)
    
instance Ord BW where
    compare Black White = LT
    compare Black Black = EQ
    compare White Black = GT
    compare White White = EQ

instance Show BW where
    show Black = "schwarz"
    show White = "weiß"
    
------------------------------------------------------------

data IntTree = Empty | Node IntTree Int IntTree

instance Eq IntTree where
    Empty == Empty                  = True
    Node x1 n1 y1 == Node x2 n2 y2  = n1 == n2 && x1 == x2 && y1 == y2
    _    == _                       = False
    t1 /= t2                        = not(t1==t2)

testTree = elem (Node Empty 1 Empty) [Node Empty 1 Empty]

------------------------------------------------------------

--data Tree a = Empty | Node (Tree a) Int (Tree a)

--instance Eq => Eq (Tree a) where
  --  Empty == Empty                  = True
--    Node x1 n1 y1 == Node x2 n2 y2  = n1 == n2 && x1 == x2 && y1 == y2
  --  _    == _                       = False
--    t1 /= t2                        = not(t1==t2)

--testTree' = elem (Node Empty 1 Empty) [Node Empty 1 Empty]


