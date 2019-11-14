data Tree a = Node a [Tree a] deriving (Eq, Show)

--70C)
nnodes :: Tree a -> Int
nnodes (Node _ []) = 1
nnodes (Node _ x) = 1 + (sum $ map nnodes x)

--70)
treeToString  :: Tree Char -> String
treeToString (Node x []) = x : "^"
treeToString (Node x xs) = x : (concatMap treeToString xs) ++ "^"

stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x (fst $ stringToTree' xs)
 where
 stringToTree' :: String -> ([Tree Char], String)
 stringToTree' "" = ([],"")
 stringToTree' (x:xs)
  |x == '^' = ([],xs)
  |otherwise = let (lst, cont) = stringToTree' xs; (n,m) = applyUntil [] stringToTree' cont in ((Node x lst) : n,m)
 applyUntil :: (Eq b) => [b] -> (a -> ([b],a)) -> a -> ([b],a)  
 applyUntil b f a = let (x,y) = f a; in if b == x then (b,y) else let (k,l) = applyUntil b f y in (x ++ k,l)
 
stringToTree2 :: String -> Tree Char
stringToTree2 (x:xs) = Node x (fst $ stringToTree' xs)
 where
 stringToTree' :: String -> ([Tree Char], String)
 stringToTree' "" = ([],"")
 stringToTree' (x:xs)
  |x == '^' = ([],xs)
  |otherwise = let (lst, cont) = stringToTree' xs; 
                   (n,m) = unzip $ takeWhile ((/=) [] . fst) $ iterate (stringToTree' . snd) ([], cont) in ((Node x lst) : (concat n), last m)
  