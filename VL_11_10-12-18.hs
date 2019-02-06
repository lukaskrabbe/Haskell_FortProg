import Prelude hiding (print, putStr, putStrLn)

-- Sequential compoisition of IO actions:

main1 = putStr "Hi" >> putStr "Hi"

main2 = let x = putStr "Hi"
        in x >> x

main3 = let his = repeat (putStr "Hi")
        in his !! 3 >> his !! 42

actions :: Int -> IO () -> IO ()
actions n act = if n==1 then act
                        else act >> actions (n-1) act

main4 = actions 2 (putStr "Hi")

santa_claus = actions 3 (putStr "Ho ")


-- duplicate an I/O action:
dupAct :: IO () -> IO ()
dupAct act = act >> act

test1 = dupAct (putStr "Hi") >> putChar '\n'


-- multiblicate an I/O action
multAct :: Int -> IO () -> IO ()
--multAct n act = foldr1 (>>) (take n (repeat act))
multAct n = foldr1 (>>) . take n . repeat 

test2  n = multAct n (putStr "Hi") >> putChar '\n'



fac :: Integer -> Integer
fac n = if n == 0 then 1 else n * (fac (n-1))

test3 n = putStr (show (fac n))


print :: Show a => a -> IO ()
print x = putStr (show x)

test4 n = print (fac n)


-- prints a String
putStr :: String -> IO ()
putStr ""       = return ()
putStr (c:cs)   = putChar c >> putStr cs

-- prints a String
putStrLn :: String -> IO ()
putStrLn x  = putStr x >> putChar '\n'  


-- reads a line from the terminal
getLine :: IO () -> String
getLine = getChar >>= \c -> if c == '\n' 
                                then return ""
                                else getLine >>= \cs -> return (c:cs)
getLine = do c <- getChar
            if c == '\n'
                then return ""
                else xs <- getLine 
                    return (c:cs)
ahllo
