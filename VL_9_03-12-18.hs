import Prelude hiding (take, repeat, iterate)

-- An infinite list of ascending numbers starting from a given number:
from :: Int -> [Int]
from n = n : from (n+1)

-- prefix of a list
take :: Int -> [a] -> [a]
take n _       | n <= 0 = []
take n []               = []
take n (x:xs)           = x : take (n-1) xs

-- Sieve of Eratosthenes:
sieve :: [Int] -> [Int]
sieve (p:xs) = p :  sieve (filter (\n -> n `mod` p > 0) xs)

primes :: [Int]
primes = sieve (from 2)

-- Generator for Fibonacci numbers
fibgen :: Integer -> Integer -> [Integer]
fibgen n1 n2 = n1 : fibgen n2 (n1+n2)

-- All Fibonacci numbers
fibs :: [Integer]
fibs = fibgen 0 1


-- Some generators for infinite lists:

-- An infinite list of identical elements:
repeat :: a -> [a]
repeat x = x : repeat x

-- An infinite list of elements with repeated applications of a function
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)


-- Generators for arithmetic sequences
fromTo :: Int -> Int -> [Int]
fromTo n m = if n>m then [] else n : fromTo (n+1) m

from' :: Int -> [Int]
from' n = iterate (+1) n

fromThen :: Int -> Int -> [Int]
fromThen n1 n2 = iterate (+ (n2 - n1)) n1

fromThenTo :: Int -> Int -> Int -> [Int]
fromThenTo n1 n2 m =
  let d = n2 - n1
  in if d>=0 && n1>m  ||  d<0 && n1<m
       then []
       else n1 : fromThenTo n2 (n2+d) m

isLetter :: Char -> Bool
isLetter c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])


-- an example enumeration type:
data BW = Black | White
 deriving (Show, Eq, Ord, Enum, Bounded)


fac :: Integer -> Integer
fac n = foldr (*) 1 [1 .. n]

numLines :: [String]
numLines =  map (uncurry (++)) (zip (map show [1..]) (repeat ". Zeile"))