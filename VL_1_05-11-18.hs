-- VL 05-11-18
-- Beispiele aus der Vorlesung, abgetippt.
-- es muss noch der Fortprog-ghci Interpreter Installiert werden. Soll in ILearn erklärt werden

--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude

--Compute the square of a number
square :: Int -> Int
square n = n*n

--Compute the minimum of two numbers
mini :: Int -> Int -> Int
mini x y = if x < y then x else y


--Compute the Faculty of n
fac :: Int -> Int
fac n = if n == 0 then 1
			else n*fac(n-1)
			
			
--Compute the n-th Fibonacci number.
--Worst Case 2^n
fib1 :: Int -> Int
fib1 n = if n == 0 
	       then 0
		   else if n == 1 
				  then 1 
				  else fib1(n-1) + fib1(n-2)
				  


--Akkumulatortechnik für n-th Fibonacci number.
-- O(n)
fib2' :: Int -> Int -> Int -> Int
fib2' fibn fibnplus1 n = if n == 0 then fibn
							else fib2' fibnplus1 (fibn+fibnplus1) (n-1)

fib2 :: Int -> Int
fib2 n = fib2' 0 1 n