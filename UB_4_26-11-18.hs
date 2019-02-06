import Prelude hiding ()
 
    
data Rose a = Rose a [Rose a]
    deriving (Show)

instance Eq a => Eq (Rose a) where
    Rose val1 [] == Rose val2 []            = val1 == val2
    Rose val1 tree1 == Rose val2 tree2      = val1 == val2 && tree1 == tree2
    
instance Ord a => Ord (Rose a) where
    Rose val1 _ `compare` Rose val2 _ = val1 `compare` val2
    

rose_exp = (Rose 4 [Rose 5 [Rose 1 [], Rose 2 [Rose 7 [], Rose 8 []], Rose 3 []], Rose 6 []])


class Pretty a  where
    pretty :: a -> String

instance Pretty Integer where
    pretty val = show val
    
instance Pretty a => Pretty (Rose a) where
    pretty (Rose val [])   = "\n" ++ pretty val 
    pretty (Rose val list) = pretty val ++ "\n +-- " ++ (concat (map pretty list))