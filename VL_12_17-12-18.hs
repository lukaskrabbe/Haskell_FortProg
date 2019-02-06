

----------------------------------------------------------------------------
-- Example: an evaluator for arithmetic expressions

-- Representation of arithmetic expressions in Haskell:
data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
 deriving Show

-- 3 + (4 * 2)
exp1 = Add (Num 3) (Mul (Num 4) (Num 2))

-- 3 + (4 / (2 - 2))
exp2 = Add (Num 3) (Div (Num 4) (Sub (Num 2) (Num 2)))

-- Evaluator with use of Applicative:
eval :: Exp -> Maybe Int
eval (Num x)   = pure x
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = do x1 <- eval x
                    y1 <- eval y
                    safediv x1 y1

safediv :: Int -> Int -> Maybe Int
safediv x y | y == 0    = Nothing
            | otherwise = Just (x `div` y)

-- Check an argument and proceed if it is not an error (Nothing)
processMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
processMaybe Nothing   _  = Nothing
processMaybe (Just x)  mf = mf x

-- processIO :: IO a -> (a -> IO b) -> IO b

{-

class Applicative tc => Monad tc where
  return :: a -> tc a
  return = pure

  (>>=) :: tc a -> (a -> tc b) ->  tc b

  (>>) :: tc a -> tc b -> tc b
  ma >> mb = ma >>= \_ -> mb

instance Monad Maybe where
  ma >>= mf = case ma of Nothing -> Nothing
                         Just x  -> mf x

instance Monad [] where
  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= ndf = [ y | x <- xs, y <- ndf x ]

-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)




