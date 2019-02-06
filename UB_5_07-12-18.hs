
import Prelude 
import Data.Ratio



fibgen :: Integer -> Integer -> [Integer]
fibgen n1 n2 = n1 : fibgen n2 (n1+n2)


fibonacci :: [Integer] 
fibonacci = fibgen 0 1


goldenRatio :: [Rational]
goldenRatio = zipWith (%) (drop 2 fibonacci) (tail fibonacci)

approx :: Rational -> [Rational] -> Rational
approx eps (x1:x2:xs) | (abs (x2-x1)) <= eps = x1
                      | otherwise = approx eps xs
                      
                      

avalanche :: [Integer]
avalanche = [3^i*5^j*7^k |    i <- [0..],
                                j <- [0..],
                                    k <- [0..]]
                                    

allCombinations :: [a] -> [[a]]
allCombinations []      = [[]]
allCombinations (x:xs)  = 


f :: [a] -> [[a]]
f xs = iterate genLists [ x | x <- xs]
    where
       genLists yss = [ x : ys | x <- xs , ys <- yss]