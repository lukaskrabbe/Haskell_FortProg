
import Test.QuickCheck
import Data.List ( (\\), sort )

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =  qsort (filter (<x) xs) ++ [x] ++ qsort( filter (>=x) xs)


prop_idempotence :: [Int] -> Bool
prop_idempotence xs = qsort (qsort xs) == qsort xs



prop_preservation :: [Int] -> Bool
prop_preservation xs = null ( xs \\ qsort xs) && null (qsort xs \\ xs)


prop_smallest_first :: [Int] -> Property
prop_smallest_first xs =
    not (null xs) ==> head (qsort xs) == minimum xs
    
prop_reference :: [Int] -> Bool
prop_reference xs = qsort xs == sort xs


-- Exectues all prop_ tests in a modile
return []
test_All = $quickCheckAll 