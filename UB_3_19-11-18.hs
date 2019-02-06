--USE THE SIMPLE HASKELL PRELUDE
import SimplePrelude



-- Aufgabe 4.4)
data Tree a = Leaf a | Tree a :+: Tree a
    deriving Show

tree_1 = (Leaf 1) :+: ((Leaf 2) :+: (Leaf 3))
tree_2 = (Leaf 4) :+: ((Leaf 5) :+: (Leaf 6))
tree_3 =  (Leaf tree_1) :+: (Leaf tree_2)


flatTree :: Tree (Tree a) -> Tree a
flatTree (Leaf  val) = val
flatTree (t1 :+: t2) = flatTree t1 :+: flatTree t2
-- Example  : flatTree tree_3
-- Output   : (Leaf 1 :+: (Leaf 2 :+: Leaf 3)) :+: (Leaf 4 :+: (Leaf 5 :+: Leaf 6))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f  (Leaf val) = Leaf (f val)
mapTree f  (t1 :+: t2) = mapTree f t1 :+: mapTree f t2
-- Example  : mapTree (+1) tree_1
-- Output   : Leaf 2 :+: (Leaf 3 :+: Leaf 4)


extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Leaf  val) = f val
extendTree f (t1 :+: t2) = extendTree f t1 :+: extendTree f t2
-- Example  : extendTree (\x -> Leaf x :+: Leaf 2) tree_2
-- Output   : (Leaf 4 :+: Leaf 2) :+: ((Leaf 5 :+: Leaf 2) :+: (Leaf 6 :+: Leaf 2))


foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f1 f2 (Leaf  val) = f1 val
foldTree f1 f2 (t1 :+: t2) = foldTree f1 f2 t1 `f2` foldTree f1 f2 t2
-- Example  : foldTree (+0) (+) tree_1
-- Output   : 6


-- Aufgabe 4.5.a)
--foldr (:) []    -- Hierbei handelt es sich um die Identitätsfunktion auf Listen
--foldl (*) 1     -- Berechnet das Produkt der Liste (((1 * x1 ) * x2) * x3)...* xn
--foldr (-) 1     -- Berechnet x1 - (x2 - (x3 - (... - xn)))
--foldl (-) 1     -- Berechnet (((x1 - x2) - x3) - x4) ... - xn

-- Aufgabe 4.5.b)
mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr ((:) . f) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\xs x -> xs ++ [f x]) []


reverser :: [a] -> [a]
reverser xs = foldr (\x acc -> acc ++ [x]) [] xs

reversel :: [a] -> [a]
reversel xs = foldl (\x acc -> acc : x) [] xs


unzipr :: [(Int, Int)] -> ([Int], [Int])
unzipr = foldr (\(x,y) (xs,ys) -> (x : xs, y : ys)) ([],[])

unzipl :: [(Int, Int)] -> ([Int], [Int])
unzipl = foldl (\(xs,ys) (x,y) -> (xs ++ [x], ys ++ [x])) ([],[])

nubr :: [Int] -> [Int]
nubr list = foldr (\x xs -> x : (filter (/= x) xs)) [] list

nubl :: [Int] -> [Int]
nubl list = foldl (\xs x -> x : (filter (/= x) xs)) [] list





data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show
  
  
jsonExample :: JSON
jsonExample = JArray
  [ JObject [ ("name", JString "meier")
            , ("besuchte_kurse", JArray
                [ JString "Logik"
                , JString "Programmierung"
                , JString "Compilerbau"
                ]
              )
            , ("bachelor_note", JNull)
            , ("zugelassen", JBool True)
            ]
  , JObject [ ("name", JString "schmidt")
            , ("besuchte_kurse", JArray 
                [ JString "Programmierung"
                , JString "Informationssysteme"
                ]
              )
            , ("bachelor_note", JFloat 2.7)
            , ("zugelassen", JBool False)
            ]
  ]

easyjsonExample :: JSON
easyjsonExample = JObject [("name", JString "Lukas")]


foldJson :: x    
                -> (Bool -> x) 
                    -> (Int -> x) 
                        -> (Float -> x) 
                            -> (String -> x) 
                                 -> ([x] -> x) 
                                      -> ([(String, x)] -> x) 
                                        -> JSON
                                            -> x
foldJson    jnull   
                jbool       
                    jint        
                        jfloat      
                            jstring   
                                jarray
                                    jobject
                                        json  
                                            = case json of
                        JNull                   -> jnull
                        JBool      val          -> jbool val
                        JInt       val          -> jint val
                        JFloat     val          -> jfloat val
                        JString    val          -> jstring val
                        JArray     vals         -> jarray (map (foldJson jnull jbool jint jfloat jstring jarray jobject) vals )
                        JObject    vals         -> jobject $ (map (\(s, js) -> (s, (foldJson jnull jbool jint jfloat jstring jarray jobject)  js)) vals)
                                                
                        
   
-- Transform a list of strings into a comma separated string
commaSep :: [String] -> String
commaSep s = foldr1 (\s1 s2 -> s1 ++ ", " ++ s2) s  


foldExample :: JSON -> String
foldExample = foldJson ("null") 
                        (\x -> if x then "true" else "false")
                          (\x -> showInt x)
                           (\x -> showFloat x)
                            (\x -> x)
                             (\x -> '[' : commaSep x ++ "]")
                              (\(x,y) -> '{' : commaSep (x ++ " : " ++  y)) ++ "}"
                                      -- Leider nicht funktionsfähig da er hier ein [Char] liefert und [String] fordert, finde leider die Lösung nicht
    


data Tree' a b = Empty
              | Leaf' a
              | Node' b [Tree' a b]
  deriving Show


foldTree' :: c -> (a -> c) -> (b -> [c] -> c) -> Tree' a b -> c
foldTree' empty leaf node tree = case tree of
  Empty     -> empty
  Leaf'    x -> leaf x
  Node' n ts -> node n (map (foldTree' empty leaf node) ts)











