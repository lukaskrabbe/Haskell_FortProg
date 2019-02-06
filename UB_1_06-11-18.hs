-- Präsenz und Übungsaufgaben Fortgeschritte Programmierkonzepte vom 05.11.18
-- Abgabefrist 12.11.18
-- 3) RMI und erste Haskell-Funktionen

import SimplePrelude


-- 3.2 a)
intSum1 :: Int -> Int
intSum1 n = div (n*(n+1)) 2


-- 3.2 b)
intSum2 :: Int -> Int
intSum2 n = if n == 0 
    then 0 
    else n + intSum2(n-1)


-- 3.2 c)
intSum3 :: Int -> Int
intSum3  n = intSum3' 0 n
    where 
     intSum3' val n = if n == 0 
         then val 
         else intSum3' (val+n) (n-1)



-- 3.5 a)
binom :: Int -> Int -> Int
binom n k =  div (fac n) ((fac k) * fac (n - k))
 where
     fac n = if n == 0 then 1 else n * fac (n - 1)
     
-- 3.5 b) 
pascal1 :: Int -> Int -> Int
pascal1 row pos = div (fac row) ((fac pos) * fac (row - pos))
 where
     fac n = if n == 0 then 1 else n * fac (n - 1)

-- 3.5 b) 
pascal2 :: Int -> Int -> Int
pascal2 row pos = if row < pos then -1                       -- Error, Wenn pos ≤ row != true
                        else if row == pos then 1           
                            else if pos == 0 then 1
                                else
                                    pascal2 (row-1) (pos-1) + pascal2 (row-1) (pos)