import Prelude hiding (map, filter, replicate, lookup)


import Data.Char

guess :: String -> Char -> Int -> Int -> IO ()
guess word answer guesses trys
                            | answer `elem` word = hangman [if answer == x then toUpper answer else x | x <- word] guesses (trys + 1)
                            | otherwise          = hangman word (guesses - 1) (trys + 1)

anonymize :: String -> String
anonymize word =  "Secret: " ++ [if c `elem` ['a'..'z'] then '*' else c | c <- word]

hangman :: String -> Int -> Int -> IO ()
hangman word guesses trys
        | guesses <= 0                  = putStrLn "Lost"
        | word == (map toUpper word)    = putStrLn  $ "Solved in " ++ show trys ++ " tries."
        | otherwise = do
                        putStrLn $ anonymize word
                        answer <- putStr "Enter a character: " >> getChar
                        putStrLn ""
                        guess word (toLower answer) guesses trys
        

start :: String -> IO ()
start word = hangman word 3 0




-- Aufgabe                       
                                
l1 = [ [i*2+1] | i <- [1 .. 5] ]
l2 = [ (i*5,i `mod` 2 == 0)| i <- [1 .. 5], i > 3 || i == 1]
l3 = [ Just(i*i) | i <- [1 .. 5], (i `mod` 2) == 1]
l4 = [ (i,j) | i <- [1 .. 5] , j <- [5,4,3,2,1], i < j ]


map :: (a -> b) -> [a] -> [b]
map f list = [f x | x <- list]

filter :: (a -> Bool) -> [a] -> [a]
filter cond list = [x | x <- list, cond x]

replicate :: Int -> a -> [a]
replicate anz val = [ val  | _ <- [1 .. anz]]

example :: [(String, String)]
example = [ ("Peter",   "1"),
              ("Lustig",  "2") ]
              
lookup :: Eq a =>  [(a,b)] -> a -> b
lookup list val = head [ b | (a,b) <- list, a == val]



  
  
--getNonEmptyLine :: IO String
getNonEmptyLine =  getLine >>=
                    \s -> if null s then putStrLn "Please enter a non-empty string." >> getNonEmptyLine 
                                    else putStrLn $ show s

