-- List comprehensions

listcomp1 = [x | x <- [0 .. 100], x > 3, x < 50]

listcomp2 = [x | x <- [0.1, 0.2 .. 100], x > 3, x < 50]

listcomp3 = [ (i,j) | i <- [1 .. 5], j <- [5 .. 10], i < j]

allPrefixes = [ [0 .. n] | n <- [0..] ]


data CSV a = CSV [[a]]

csv1 :: CSV Int
csv1 = CSV [[1,2,3],[4,5,6],[7,8]]

instance Show a => Show (CSV a) where
    show (CSV xss) = concat (map (++ "\n") (map separate xss))
        where
            separate []     = ""
            separate [x]    = show x
            separate (x:xs) = show x ++ "," ++ separate xs

instance Read a => Read (CSV a) where
    readsPrec p s = case s of
        [] -> [ (CSV [], "") ]
        '\n':s1  -> [ (CSV ([]:xss), s2) | (CSV xss , s2) <- readsPrec p s1]
        ',' :s1  -> [ (CSV ((x:xs):xss), s3) 
                        | (x,s2) <- readsPrec p s1, 
                          (CSV (xs:xss), s3) <- readsPrec p s2]
        _   :s1  -> [ (CSV ((x:xs):xss), s3) 
                        | (x,s2) <- readsPrec p s, 
                          (CSV (xs:xss), s3) <- readsPrec p s2]