-- VL 05-11-18
-- Beispiele aus der Vorlesung, abgetippt.
-- es muss noch der Fortprog-ghci Interpreter Installiert werden. Soll in ILearn erklärt werden

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude hiding (fst, snd, zip, unzip, last, lines, take)


-- THE FIRST Componnent of a pair:
fst :: (a,b) -> a
fst (x,y) = x

-- THE SECOND Componnent of a pair:
snd :: (a,b) -> b
snd (x,y) = y


-- Zip two Lists into one List
zip :: [a] -> [b] -> [(a,b)]
zip  [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys


unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y) : xys) =  let (xs,ys) = unzip xys
                        in (x:xs, y:ys) 
                        
                        
-- Compute the last element of a list
last :: [a] -> a
last (x : xs@(_:_))   =  last xs
last [x]        =  x

lines :: String -> [String]
lines "" = []
lines ('\n':cs) = "" : lines cs
lines (c:cs) = case lines cs of
                    []      -> [[c]]
                    (l:ls)  -> (c:l) : ls
                    
                    
-- Computes the factioral function.
fac:: Int -> Int
fac n | n == 0 = 1
      | otherwise = n * fac(n-1)

-- Returns a prefix of a list
take :: Int -> [a] -> [a]
take n xs | n <= 0 = []
          | otherwise = case xs of 
                              [] -> []
                              (x:xs) -> x : take (n-1) xs







derrive :: (Float -> Float) -> (Float -> Float)
derrive f = f'
    where f' x = ( f(x +. dx) -. f x) /. dx


dx :: Float
dx = 0.0001 

square :: Float -> Float
square x = x *. x




















