--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude hiding ((.), curry, uncurry, const)




type Array a =  (Int -> a)
    
emptyArray :: Array a 
emptyArray i = error ("Access to non-initialized component " )

initArray :: a -> (Array a)
initArray v = \i -> v

getIndex :: Array a -> Int -> a
getIndex a i = a i

putIndex :: Array a -> Int -> a -> Array a 
putIndex a i v = a'
  where a' j | i == j    = v
             | otherwise = a j

a12 = putIndex (putIndex (initArray "") 1 "Hello") 2 "World"


---------------------------------------------------------------------------------------
-- Composition function
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)


ex1 :: String -> Int
ex1 = sum . filter ( \n -> n>=65 && n<=95) . map ord 

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f(x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f(x,y) = f x y

-- Multiplies the pairwise sum of two lists
ex2 :: [Int] -> [Int] -> Int
ex2 xs ys = prod (map (uncurry (+)) (zip xs ys))
    where prod = foldr (*) 1
    
-- The always constant function
const :: a -> b -> a
const x _ = x

---------------------------------------------------------------------------------------



















