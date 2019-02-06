-- VL 19-11-18
-- Beispiele aus der Vorlesung, abgetippt.
-- es muss noch der Fortprog-ghci Interpreter Installiert werden. Soll in ILearn erklärt werden

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude hiding (map, foldr, filter, foldl, sum)

-- Increments all Emlements in a list
incList :: [Int] -> [Int]
incList [] = []
incList (x:xs) = (x+1) : incList xs


-- Squares all Emlements in a List
sqList :: [Int] -> [Int]
sqList [] = []
sqList (x:xs) = (x*x) : sqList xs

-- change a charakter (letter) by incrementing its code
code :: Char -> Char
code c | ord c == ord 'Z' = 'A'
       | ord c == ord 'z' = 'a'
       | otherwise        =  chr (ord c + 1)
       
-- encode every charcter in a String
codeStr :: String -> String
codeStr [] = []
codeStr (c:cs) =  code c : codeStr cs





-- Apply a function f on every Emlements in a List
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

incList'   = map (+1) 
sqList'    = map (\x -> x*x)  -- waht the fuck is \x -> ...
codeStr'   = map code

---------------------------------------------------------------------------------------

-- Sums up all elements in a list
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
 
-- Sums up  the codes of all elements in a String
checkSum :: String -> Int
checkSum [] = 0
checkSum (c:cs) = ord c + checkSum cs
 
-- Multiplies all elements in a list
prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

 
-- 
foldr :: (a -> b -> b ) -> b -> [a] -> b
foldr f e [] = e
foldr f e (x : xs) = f x (foldr f e xs)
  
sum'       = foldr (+) 0
prod'      = foldr (*) 1
checkSum'  = foldr (\x y -> ord x + y) 0

---------------------------------------------------------------------------------------

-- Filter Elements in a list statisfying a predicate
filter :: (a -> Bool ) -> [a] -> [a]
filter _ []       = [] 
filter p (x:xs)   | p x         =  x : filter p xs
                  | otherwise   = filter p xs    
                  
-- Transform a list into a set, i.e., delete duplicate elements
nub :: [Int] -> [Int]
nub []      = []
nub (x:xs)  = x : (filter (/=x) xs)


-- Sorts a list with the quicksort algorithm
qsort :: [Int] -> [Int]
qsort []        = []
qsort (x:xs)    = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (>x) xs)

--------------------------------------------------------------------------------------- 

sumAcc xs = sum' xs 0
    where sum' []       e = e
          sum' (x:xs)   e = sum' xs (e+x)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []       = e
foldl f e (x:xs)   = foldl f (f e x) xs
  
 
--------------------------------------------------------------------------------------- 
 
 
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x | p x          = while p f (f x)
             | otherwise    = x
 
 -- Zwoerpotenz größer gleich 1000
potGT1000 = while (<1000) (*2) 1
 
 
 
 
 
 
--------------------------------------------------------------------------------------- 

type Array a =  (Int -> a)
    
emptyArray :: Array a 
emptyArray i = error ("Access to non-initialized component " )

getIndex :: Array a -> Int -> a
getIndex a i = a i

putIndex :: Array a -> Int -> a -> Array a 
putIndex a i v = a'
  where a' j | i == j    = v
             | otherwise = a j

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 









