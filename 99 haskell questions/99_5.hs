import qualified Data.List as L
import qualified Data.Function as F

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf :: a -> Tree a 
leaf x = Branch x Empty Empty

--55)
cbalTree :: Int -> Tree Char
cbalTree 1 = leaf 'x'
cbalTree 2 = Branch 'x' (leaf 'x') (Empty)
cbalTree 3 = Branch 'x' (leaf 'x') (leaf 'x')
cbalTree n 
 |even n = Branch 'x' (cbalTree ((n-1) `div` 2)) (cbalTree (((n-1) `div` 2) + 1))
 |odd n = Branch 'x' (cbalTree ((n-1) `div` 2)) (cbalTree ((n-1) `div` 2))
 
 
mapFunc :: [a -> b] -> [a] -> [b]
mapFunc [fh] x = map fh x
mapFunc (fh:fl) x = map fh x ++ mapFunc fl x

cbalTree' :: Int -> [Tree Char]
cbalTree' 1 = [leaf 'x']
cbalTree' 2 = [Branch 'x' (leaf 'x') (Empty), Branch 'x' (Empty) (leaf 'x')]
cbalTree' 3 = [Branch 'x' (leaf 'x') (leaf 'x')]
cbalTree' n 
 |even n = mapFunc (map (Branch 'x') l) r ++ mapFunc (map (Branch 'x') r) l
 |odd n = mapFunc (map (Branch 'x') l) l
 where 
 l = cbalTree' ((n-1) `div` 2)
 r = cbalTree' (((n-1) `div` 2) + 1)
--56)
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ aleft aright) (Branch _ bleft bright) = (mirror aleft bright) && (mirror bleft aright)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric x = mirror x x
--57)
construct :: [Int] -> Tree Int
construct [] = Empty
construct [h] = leaf h
construct (h:t) = let (a,b) = L.partition (<h) t in Branch h (construct a) (construct b)
--58)
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree'
--59)
hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = [Branch x l r |(hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],l <- hbalTree x hl, r <- hbalTree x hr]
--60)
minNodes :: Int -> Int
minNodes n
 |n == 1 = 1
 |n == 2 = 2
 |otherwise = 1 + (minNodes $ n-1) + (minNodes $ n-2)


minConstr {-h-}0 = Empty
minConstr 1 = leaf 'x'
minConstr n = Branch 'x' (minConstr $ n-1) (minConstr $ n-2)

maxHeight :: Int -> Int
maxHeight n = maxHeight' n 1
 where
 maxHeight' n i = let x = minNodes i in if n - x == 0 then i else (if n - (minNodes i) < 0 then i-1 else maxHeight' n $ i+1)
 
{-
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes e n = maxHeight n -} 
makeTrees :: a -> Int -> [Tree a]
makeTrees _ 0 = []
makeTrees c 1 = [leaf c]
makeTrees c n = lonly ++ ronly ++ landr
 where 
 lonly  = [Branch c t Empty | t <- smallerTree]
 ronly = [Branch c Empty t | t <- smallerTree]
 landr = concat [[Branch c l r | l <- fst lrtrees, r <- snd lrtrees] | lrtrees <- treeMinusTwo]
 smallerTree = makeTrees c (n-1)
 treeMinusTwo = [(makeTrees c num, makeTrees c (n-1-num)) | num <- [0..n-2]]
--61)
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right
--62)
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch e Empty Empty) = [e]
leaves (Branch _ left right) = leaves left ++ leaves right
--63)
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch e left right) = e : internals left ++ internals right
--64)
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch e left right) 1 = [e]
atLevel (Branch e left right) n = atLevel left (n-1) ++ atLevel right (n-1)
--65)
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = leaf 'x'
completeBinaryTree n = if freenodes - maxl < 0 
 then Branch 'x' (completeBinaryTree $ l + freenodes) (completeBinaryTree r)
 else Branch 'x' (completeBinaryTree $ l + maxl) (completeBinaryTree $ r + (freenodes - maxl))
 where
 minheight = floor $ logBase 2 $ fromIntegral n
 cmpl = 2 ^ minheight
 freenodes = n - (cmpl-1)
 maxl = (2^(minheight+1) - cmpl) `div` 2
 l = r
 r = (cmpl-2) `div` 2
completeBinaryTree' :: Int -> Tree Char                                                                                                                       
completeBinaryTree' n = generate_tree 1                                                                                                                       
 where 
 generate_tree x                                                                                                                                        
  |x > n     = Empty                                                                                                                                  
  |otherwise = Branch 'x' (generate_tree (2*x)) (generate_tree (2*x+1)) 

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree (Branch _ left Empty) = True
isCompleteBinaryTree (Branch _ Empty right) = False
isCompleteBinaryTree (Branch _ left right) = isCompleteBinaryTree left && isCompleteBinaryTree right


--64)
layout :: Tree a -> Tree (a,(Int, Int))
layout Empty = Empty
layout tree = layout' tree 1 [1..(countNodes tree)]
 where
 countNodes :: Tree a -> Int
 countNodes Empty = 0
 countNodes (Branch _ l r) = 1 + countNodes l + countNodes r
 layout' :: Tree a -> Int -> [Int] -> Tree (a,(Int, Int))
 layout' Empty _ _ = Empty
 layout' (Branch x left right) n e = 
  let countl = countNodes left; lst = splitAt countl e
  in Branch (x, (head $ snd lst, n)) (layout' left # n+1 $ fst lst) (layout' right # n+1 $ drop 1 $ snd lst)

infixl 1 #
(#) :: (a -> b -> c) -> a -> (b -> c)
(#) f p = f $ p 

--65)
height :: Tree a -> Int
height Empty = 0
height (Branch x left right) = 1 + (max # height left $ height right)

layout2 :: Tree a -> Tree (a,(Int, Int))
layout2 t = layout2' t  1 $ height t
 where
 layoutR Empty _ _ _ = Empty
 layoutR (Branch e left right) x h maxh = Branch (e, (x, h)) (layoutR left (x-2^(maxh-h-1)) (h+1) maxh) (layoutR right (x+2^(maxh-h-1)) (h+1) maxh)
 layout2' Empty _ _ = Empty
 layout2' (Branch e Empty Empty) h _ = Branch (e, (1, h)) Empty Empty
 layout2' (Branch e left right) h maxh = 
  let l@(Branch (_, (x, y)) _ _) = layout2' left (h+1) maxh
  in Branch (e, (x+2^(maxh-h-1), h)) l $ layoutR right (x+2^(maxh-h-1)+2^(maxh-h-1)) (h+1) maxh
  
  
--67)
treeToString :: Tree Char -> String
treeToString Empty = []
treeToString (Branch e Empty Empty) = [e] 
treeToString (Branch e left right) = e : "(" ++ treeToString left ++ "," ++ treeToString right ++ ")"

stringToTree :: String -> Tree Char
stringToTree s 
 |length s == 0 = Empty 
 |length s == 1 = Branch (head s) Empty Empty
 |otherwise = 
  let (l,r) = splitAt commaIndex s 
  in Branch (head s) (stringToTree $ drop 2 l) (stringToTree $ dropLast $ drop 1 r)
 where
 dropLast = reverse . drop 1 . reverse
 commas = L.elemIndices ',' s
 openP i s= length $ L.elemIndices '(' $ take i s
 closeP i s = length $ L.elemIndices ')' $ take i s
 commaIndex = snd $ L.minimumBy (\(o,_) (c,_) -> compare o c) $ map (\i -> (openP i s - closeP i s, i)) commas
 
 
--68)
treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch e left right) = e : treeToPreorder left ++ treeToPreorder right

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch e left right) = treeToInorder left ++ e : treeToInorder right

preInTree  :: String -> String -> Tree Char
preInTree "" "" = Empty
preInTree (p:o) i = let (l,r) = span (/= p) i; (x,y) = span (flip elem l) o in Branch p (preInTree x l) (preInTree y $ drop 1 r)

--69)
tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch e left right) = e : tree2ds left ++ tree2ds right

ds2tree :: String -> Tree Char
ds2tree "." = Empty
ds2tree (x:xs) = let (l, r) = ds2tree' xs in Branch x (l) (ds2tree r)
 where
 ds2tree' :: String -> (Tree Char, String)
 ds2tree' (x:xs)
  |x == '.' = (Empty, xs)
  |otherwise =  let (l, r) = ds2tree' xs; (r', o) = ds2tree' r in (Branch x (l) (r'), o)