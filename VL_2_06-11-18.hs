-- VL 06-11-18
-- Beispiele aus der Vorlesung, abgetippt.

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude


fib2 :: Int -> Int
fib2 n = fib2' 0 1 n
    where 
        fib2' fibn fibnplus1 n = if n == 0 then fibn
        							else fib2' fibnplus1 (fibn+fibnplus1) (n - 1)
                                    
                                    
fib3 :: Int -> Int
fib3 n = 
    let fib3' fibn fibnplus1 n = if n == 0 then fibn
        							else fib3' fibnplus1 (fibn+fibnplus1) (n - 1)
    in fib3' 0 1 n
                                    
                                    
                                    
                                    
                                    
-- Checks wheather a number is prime.
isPrime :: Int -> Bool
isPrime n = n/= 1 && notDivides (n-1)
 where 
    notDivides m = m == 1 || (mod n m /= 0 && notDivides (m-1))







    
-- Data Declaration
-- Enumeration
data Color = Red | Blue | Yellow | Green


--Record type:
data Complex = Complex Float Float
    deriving Show -- Generiert quasi eine toString Methode

c34 :: Complex
c34 = Complex 3.5 4.3

addC :: Complex -> Complex -> Complex
addC (Complex r1 i1) (Complex r2 i2) = Complex (r1 +. r2) (i1 +. i2)



-- Lists of numbers:
data IntList = Nil | Cons Int IntList
    deriving Show

list123 :: IntList 
list123 = Cons 1 (Cons 2 (Cons 3 Nil))


append :: IntList -> IntList -> IntList
append Nil         ys = ys
append (Cons n xs) ys = Cons n (append xs ys)

-- Oder Kompakter:
lis123 :: [Int]
lis123 = 1 : (2 : (3 : []))
--lis123 = 1 : 2 : 3 : []
--lis123 = [1,2,3]





