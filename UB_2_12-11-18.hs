-- UB 2 12-11-18
-- Beispiele aus der Vorlesung, abgetippt.

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude 

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show

tree123 :: Tree Int
tree123 = Node (Node (Node Empty 1 Empty) 2 (Node Empty 4 Empty)) 5 (Node (Node Empty 2 Empty) 9 (Node Empty 6 Empty))
-- Example of tree123 :
--                                   5
--               2                                             9
--      1                      4                      2                     6
--  E       E              E       E               E     E               E     E


--Aufgabe 3.2.a)
sumTree1 :: Tree Int -> Int
sumTree1 (Empty) = 0
sumTree1 (Node t1 a t2) = sumTree1 t1 + a + sumTree1 t2

-- !!!
sumTree2 :: Tree Int -> Int -> Int -> Int
sumTree2 (Node Empty a Empty) sumr suml = sumr + a + suml
sumTree2 (Node Empty a t2 ) sumr suml = (sumTree2 t2 (sumr)  (suml))
sumTree2 (Node t1 a Empty ) sumr suml = (sumTree2 t1 (sumr)  (suml))
sumTree2 (Node t1 a t2) sumr suml = (sumTree2 t1 (sumr) suml) +  (sumTree2 t2 sumr (suml))




-- Aufgabe 3.2.b)
mirrorTree :: Tree a -> Tree a
mirrorTree (Empty) = Empty
mirrorTree (Node t1 a t2) = Node (mirrorTree t2) a (mirrorTree t1)




toList :: Tree a -> [a]
toList Empty        = []
toList (Node t1 a t2) = [a] ++ toList t1  ++ toList t2



-- Aufgabe 3.3.a)
reverse1 :: [Int] -> [Int]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]
 


reverse2 :: [Int] -> [Int]
reverse2 oL = func [] oL 
    where
        func nL [] = nL
        func nL (x:xs) = func (x:nL) xs
        
-- reverse1 mit Liste von 1000 Elementen : (0.06 secs, 31,275,496 bytes)
-- reverse2 mit Liste von 60 Elementen : (0.04 secs, 3,293,728 bytes) 
-- Die Lösung mit Akkumulatortechnik ist erheblich schneller als die "Einfache Lösung" 


-- Aufgabe 3.3.b)
indexOf :: [Int] -> Int -> Maybe Int 
indexOf (x:xs) n = func (x:xs) n 0
    where func (x:xs) n t = if n == x then Just t 
                            else if null xs then Nothing
                            else func xs n (t+1) 


-- Aufgabe 3.3.c)
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xss@(x:xs) = xss:(tails xs)  -- @ = y wird zum gesamt ausdruck x + xs



-- Aufgabe 3.3.d)
insert :: a -> [a] -> [[a]]
insert val [] = [[val]]
insert val xss@(x:xs) = (val : xss) : map (x :) (insert val xs)


-- Aufgabe 3.3.e)
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [x:ys | x <- xs, ys <- perms (delete x xs) ]
                    where 
                        delete _ []                 = []
                        delete x (y:ys) | x == y    = delete x ys
                                        | otherwise = y : delete x ys
                                        
--perms xs = [ y:zs | (y,ys) <- select xs, zs <- perms ys]
--  where select []     = []
--        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]







