import SimplePrelude 

-- Aufgabe 3.4)
data SearchTree a = Empty | Node (SearchTree a) Int (SearchTree a)
    deriving Show

sTree :: SearchTree Int
sTree = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty) 


-- Aufgabe 3.4.a)
insert :: SearchTree a -> Int -> SearchTree a
insert (Empty)          val = Node (Empty) val (Empty)  
insert (Node t1 y t2)   val = if y == val then Node t1 y t2
                                else if y < val then Node t1 y (insert t2 val) 
                                    else  Node (insert t1 val) y t2

-- Aufgabe 3.4.b)
isElem :: SearchTree a -> Int -> Bool
isElem (Empty)          val = False
isElem (Node t1 y t2)   val = if val == y then True
                                else if val < y then isElem t1 val
                                    else isElem t2 val
                                 
-- Aufgabe 3.4.c)                 
delete :: Int -> SearchTree a -> SearchTree a                               
delete _ Empty = Empty
delete val (Node t1 x t2) | x > val  = (Node (delete val t1) x t2) 
                          | x < val  = (Node t1 x (delete val t2))
                          | x == val = delete' (Node t1 x t2)
                            where
                                delete' (Node Empty _ Empty)   = Empty
                                delete' (Node t1 _ Empty)      = t1
                                delete' (Node Empty _ t2)      = t2
                                delete' (Node t1 _ t2)         = let new_x  = (minVal t2)
                                                                     t2'    = delete new_x t2
                                                                     -- Um den Minimum Wert zu finden:
                                                                     minVal (Node Empty val _)   = val
                                                                     minVal (Node l val _)       = minVal l
                                                                 
                                                                 in (Node t1 new_x t2') 
                                    
