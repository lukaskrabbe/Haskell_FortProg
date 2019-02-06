-- VL 12-11-18
-- Beispiele aus der Vorlesung, abgetippt.

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude hiding(length, last, head, tail, concat)


-- Compute the length of a list of numbers
lengthI :: [Int] -> Int
lengthI []       = 0  
lengthI (x:xs)   = 1 + lengthI xs


-- Compute the length of a list of charakters
lengthC :: [Char] -> Int
lengthC []       = 0  
lengthC (x:xs)   = 1 + lengthC xs


-- polymorpher Typ 
-- length: Für Alle x [x] -> Int
length :: [a] -> Int
length []       = 0
length (x:xs)   = 1 + length xs


-- Compute the last element of a list
last :: [a] -> a
last []         = error "last on empty list!"
last [x]        =  x
last (x : xs)   =  last xs


-- Datatype for partial values (auskommentiert weil Maybe a berits defineirt ist)
-- data Maybe a = Nothing | Just a

-- Compute the last element of a list
lastP :: [a] -> Maybe a
lastP []         = Nothing
lastP [x]        = Just x
lastP (x : xs)   =  lastP xs

-- Is a partial vlaue really no value
isNothing :: Maybe a -> Bool
isNothing Nothing   = True 
isNothing (Just x)  = False

fromJust :: Maybe a -> a
fromJust (Just a) = a


-- Binary Tree: a leaf contains a value, a node has two subtrees
--              Node
--            1     Node
--                  2   3

data BTree a = Leaf a | Node (BTree a) (BTree a)
    deriving Show

btree123 :: BTree Int
btree123 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

heigth :: BTree a -> Int
heigth (Leaf _)     = 1   -- _ <- ist ein annonymer Datentyp da er uns nicht interressiert
heigth (Node t1 t2) = 1 + max (heigth t1) (heigth t2)

-- First element of  a non empty(!) list:
head :: [a] -> a
head (x:xs) = x

-- First element of  a non empty(!) list:
tail :: [a] -> [a]
tail (_:xs) = xs


-- Concateneates a list of list to a singel list:
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss)   = xs ++ concat xss


-- Select the n-th elemet og a list (n=0 : First element)
-- predefined as !!
nth :: [a] -> Int -> a
nth (x:xs) n = if n == 0 then x
                    else nth xs (n-1)


-- sum up all integers in a list of Either Int ...:
sumUp :: [Either Int a] -> Int
sumUp []                = 0
sumUp (Left x : xs)     = x + sumUp xs
sumUp (Right _ : xs)    = sumUp xs



