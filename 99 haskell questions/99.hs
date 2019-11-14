import Data.List
--1)
mylast :: [a] -> a
mylast [] = error "List is empty!"
mylast [x] = x
mylast (_:x) = mylast x

mylast' :: [a] -> a
mylast' [] = error "List is empty!"
mylast' [x] = x
mylast' x = (!!) x $ length x - 1
--2)
mybutlast :: [a] -> a
mybutlast [] = error "List is empty!"
mybutlast [x] = x
mybutlast [x,y] = x
mybutlast (_:x) = mybutlast x
--3)
elemAt :: [a] -> Int -> a
elemAt [] _ = error "List is empty!"
elemAt l@(x:xs) index
 | index <= 0 || index > length l = error "Index is out of list"
 | index == 1 = x
 | otherwise = elemAt xs $ index - 1
--4)
mylen :: [a] -> Int 
mylen [] = 0
mylen [x] = 1
mylen (_:l) = 1 + mylen l
--5)
myreverse :: [a] -> [a]
myreverse [] = []
myreverse [x] = [x]
myreverse (x:xs) = myreverse xs ++ [x]
--6)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome [x,y] = x == y
isPalindrome x = if (head x) == (last x) then isPalindrome (init(tail x))  else False
--7)
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (h:l)) = (flatten h) ++ (flatten (List l))
--8)
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y) = x : (compress $ dropWhile (== x) y)

compress' :: (Eq a) => [a] -> [a]
compress' x = map head $ group x

compress'' :: Eq a => [a] -> [a]
compress'' = map head . group
--9)
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack all@(x:_) = let pair = span (== x) all; fst_p = [fst pair]; snd_p = (pack $ snd pair) 
 in if snd_p == [[]] then fst_p else fst_p ++ snd_p
--pack "aaabbb" pair == ("aaa","bbb") ret == ["aaa"]
--10)
--encode :: (Eq a) => [a] -> [(Int, a)]
{-"aaabbb"
pack x == ["aaa","bbb"]
map (zip [1..]) ["aaa","bbb"] == [[()()(3,a)],[()()(3,b)]]
map last [[()()(3,a)],[()()(3,b)]]
-}
encode x =  map last (map (zip [1..]) (pack x))
--11)
data ListType a = Multiple Int a | Single a deriving (Show)
encodemod :: (Eq a) => [a] -> [ListType a]
encodemod  = (map encodehelp) . encode 
 where 
 encodehelp (1,x) = Single x
 encodehelp (n,x) = Multiple n x
{- map :: (a -> b) -> [a] ->[b] ;; ((Int, a) -> ListType a) -> [(Int,a)] ->[ListType a]
encodehelp :: (Int, b) -> ListType b
encode :: [a] -> [(Int,a)]
map encodehelp . encode  == (\x -> map encodehelp  (encode x))
where f == map encodehelp :: [(Int,a)] ->[ListType a]
g == encode [a] -> [(Int,a)]
-}
encodemod' :: (Eq a) => [a] -> [ListType a]
encodemod' x = map encodehelp (encode x)
 where 
 encodehelp (1,x) = Single x
 encodehelp (n,x) = Multiple n x

--encodemod x = map encodehelp (encode x)
-- (.) :: (b -> c) -> (a -> b) -> a -> c  
--f . g = \x -> f (g x)  
{-
map encodehelp . encode 
map \x -> encodehelp (encode x) list
-}
--12)
decodemod :: [ListType a] -> [a]
decodemod  = concat . map decode 
 where
 decode (Single e) = [e]
 decode (Multiple 1 e) = [e]
 decode (Multiple i e) = e : decode (Multiple (i - 1) e)
 
decodemod' :: [ListType a] -> [a]
decodemod' = foldr (\x_ emp -> (decode x_) ++ emp) [] 
 where
 decode (Single e) = [e]
 decode (Multiple 1 e) = [e]
 decode (Multiple i e) = (:) e $ decode (Multiple (i - 1) e)
{-[Multiple 2 'a', Single 'b']
foldr (\x_ emp -> (decode x_) : emp) [] [Multiple 2 'a', Single 'b']
\ -> Single 'b' []
decode Single 'b' = 'b'
(decode Single 'b') : []
'b' : []
\ -> Multiple 2 'a' ['b']
(decode Multiple 2 'a') : ['b']
decode Multiple 2 'a' = (:) 'a' $ decode (Multiple 1 'a')
decode (Multiple 1 'a') = 'a'
decode Multiple 2 'a' = (:) 'a' $ 'a'
(:) 'a' $ 'a' : ['b']
(:) 'a' $ ['a', 'b']
-}
--13)
count :: (Eq a) => a -> [a] -> Int
count e [] = 0
count e (i:l) = if (e == i) then 1 + count e l else count e l

encodedirect :: (Eq a) => [a] -> [ListType a]
encodedirect [] = []
encodedirect (h:t) = (direct h t) :  encodedirect (dropWhile ( == h) t)
 where
 direct e l = let c = count e l + 1 in if (c == 1) then Single e else Multiple c e
--14)
dupli :: [a] -> [a]
dupli = foldr (\x emp -> x:x:emp) [] 
--15)
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i = (replicate' x i) ++ (repli xs i)
 where 
 replicate' :: a -> Int -> [a]
 replicate' x 1 = [x]
 replicate' x i = x : (replicate' x (i - 1))
--16)
dropevery :: [a] -> Int -> [a]
dropevery [] _ = []
dropevery x n = (take (n - 1) x) ++ (dropevery (drop n x) n)
--17)
split :: [a] -> Int -> ([a],[a])
split x i = ((take i x),(drop i x))
--18)
slice :: [a] -> Int -> Int -> [a]
slice x i k = take (k-i+1) (drop (i-1) x)
--19)
rotate :: [a] -> Int -> [a]
rotate list i
 |i < 0 = let rev_lst = reverse list; i_ = abs i in (reverse (take i_ rev_lst)) ++ (reverse (drop i_ rev_lst))
 |i > 0 = (drop i list) ++ (take i list)
 |otherwise = list
--20)
removeAt :: Int -> [a] -> (a,[a])
removeAt i list = (list !! (i-1), (take (i-1) list) ++ (drop i list))
 
 
 
 