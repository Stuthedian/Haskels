--46)
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

{-

infixr 5  .++  
(.++) :: [a] -> [a] -> [a]
[] .++ ys = ys  -}

infixr 9  .!
(.!) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.!) f g = \a b -> f (g a b)

nand :: Bool -> Bool -> Bool
nand = not .! (&&) 

nor :: Bool -> Bool -> Bool
nor = not .! (||)

apply_on :: (a -> b -> c) -> (d -> e -> a) -> (d -> e -> b) -> d -> e -> c
apply_on f g h = \a b -> (g a b) `f` (h a b)

xor :: Bool -> Bool -> Bool
xor = apply_on and' or' nand

impl :: Bool -> Bool -> Bool
impl = or' . not 

equ :: Bool -> Bool -> Bool
equ  = not .! xor

table :: (Bool -> Bool -> Bool) -> [String]
table f = map (\[a,b] -> show a ++ " " ++ show b ++ " " ++ (show $ f a b)) $ perm False True 2
 where 
 perm :: a -> a -> Int -> [[a]]
 perm a b 1 = [[a],[b]]
 perm a b x = let lst = perm a b $ x-1 in (map (\e -> a : e) lst) ++ (map (\e -> b : e) lst)

 
infixl 3 `equ`
infixl 4 `or'`
infixl 6 `and'`
--48)
table_n :: Int -> ([Bool] -> Bool) -> [String]
table_n l f = map (\x -> toStr x ++ " "++ (show $ f x)) $ perm False True l
 where 
 perm :: a -> a -> Int -> [[a]]
 perm a b 1 = [[a],[b]]
 perm a b x = let lst = perm a b $ x-1 in (map (\e -> a : e) lst) ++ (map (\e -> b : e) lst)
 toStr :: (Show a) => [a] -> String
 toStr [h] = show h
 toStr (h:t) = show h ++ " " ++ toStr t

--49)
gray :: Int -> [String]
gray 1 = ["0","1"]
gray x = let g = gray $ x-1 in (map ('0':) g) ++ (map ('1':) $ reverse g)
 

main = sequence $ map putStrLn $ table_n 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ` a `and'` b `or'` a `and'` c)
--50)



huffman :: [(Char, Int)] -> [(Char, String)]
huffman = huffman' 
huffman' :: [(Char, Int)] -> [(Char, String)]
huffman' x 
 |l == 1 = let [(a, _)] = x in [(a, "0")]
 |l == 2 = let [(a, _), (b, _)] = x in [(a, "0"), (b, "1")]
 |odd l = (huffman' $ take 1 x) ++ (map (\(a, c) -> (a, '1':c)) $ huffman' $ drop 1 x)
 |even l = (map (\(a, c) -> (a, '0':c)) $ huffman' $ take 2 x) ++ (map (\(a, c) -> (a, '1':c)) $ huffman' $ drop 2 x)
 where
 l = length x
{-
helper [(s1, fr1), (s2, fr2)] = [(s1 ++ s2, )]
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

data huffmanTree = Zero (huffmanTree)| One (huffmanTree) | Branch (Zero (huffmanTree), One (huffmanTree)) | Empty deriving (Show, Eq)
--data huffmanTree = false | true  -- deriving Show

constructTree :: [a] -> huffmanTree 
constructTree x
 |l == 1 = Zero
 |l == 2 = Branch (Zero) (One)
 |even $ l = Branch (Zero (constructTree' $ take (l `div` 2) x)) (One (constructTree' $ drop (l `div` 2) x))
 |odd $ l = Branch (Zero) (One (constructTree' $ drop 1 x))
 where
 l = length x
 constructTree' :: [a] -> huffmanTree
 constructTree' [x,y] = Branch (Zero) (One)
 constructTree' x = Branch (Zero (constructTree' $ take (l `div` 2) x)) (One (constructTree $ drop (l `div` 2) x))-}
 
 









