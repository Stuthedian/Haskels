import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import System.IO
import Data.Function (on)
import Data.Foldable (maximum)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
--import qualified Data.Numbers.Primes as P
delFirst :: (Eq a) => a -> [a] -> [a]
delFirst e [] = []
delFirst e (h:t) = if e == h  then t else h : delFirst e t
--1)
mult3_5 :: Int -> Int
mult3_5 n = sum $ L.nub $ takeWhile (<n) (map (3*) [1..]) ++ takeWhile (<n) (map (5*) [1..])
--mult3_5 n = sum $ L.nub $ takeWhile (<n) (map (3*) [1..]) ++ takeWhile (<n) (map (5*) [1..])
--2)
fib = sum $ filter (even) $ takeWhile (< 4000000) $ fib' [1,1]
 where
 fib' :: [Int] -> [Int]
 fib' [] = []
 fib' [y, z] = y : fib' (z:[y+z])
 fib' (h:t) = h : fib' t
--3)
primeFactor :: Int -> Int
primeFactor x
 |x == 1 = 1
 |x < 1 = error "Illegal argument"
 |otherwise = primeFactor' x [2..]
 where
 primeFactor'  1 (h:_) = h
 primeFactor' n (h:t) = let (d, m) = n `divMod` h in if m == 0 then primeFactor' d (h:t) else primeFactor' n t
 
primes n = filterPrimes [2..n-1]
filterPrimes [] = []
filterPrimes (h:t) = h : (filterPrimes $ filter (\e -> e `mod` h /= 0) t)

primes2 n = filterPrimes2 [2..n-1]
filterPrimes2 [] = []
filterPrimes2 (h:t) = h : (filterPrimes2 $! filter (\e -> e `mod` h /= 0) t)

primes' n = filterPrimes' [2..n-1]
filterPrimes' [] = []
filterPrimes' (h:t) = h:(filterPrimes' $ filterMe h h (h:t))
filterMe _ _ [] = []
filterMe e s (h:t) = if h == e then filterMe (e+s) s t else h : (filterMe e s t)

{-main = do
 number <- getLine
 putStrLn $ show $ primeFactor $ (read number :: Int)
 getChar-}
{-
primes n = filterPrimes [2..n-1]
filterPrimes [] = []
filterPrimes (h:t) = h : ( filterPrimes $ filter (\e -> e `mod` h /= 0) t)
  primes = filterPrime [2..] 
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
primeFactor :: Int -> Int
primeFactor x
 |x == 1 = 1
 |x < 1 = error "Illegal argument"
 |otherwise = primeFactor' x $ x `div` 2
 where
 primeFactor' a b = if isPrime b  then (if a `mod` b == 0 then b else primeFactor' a $ b-1) else primeFactor' a $ b-1
 isPrime n = isPrime' n [2..n-1]
 isPrime' n [] = True
 isPrime' n (h:t) = if n `mod` h == 0 then False else isPrime' n t
 
 primeFactor' a b = if a `mod` b == 0 then (if isPrime b then b else primeFactor' a $ b-1) else primeFactor' a $ b-1
 isPrime n = isPrime' n [2..n-1]
 isPrime' n [] = True
 isPrime' n (h:t) = if n `mod` h == 0 then False else isPrime' n t-}
 {-
 primeFactor' a 0 = -1
 primeFactor' a b = if a `mod` b == 0 then primeFactor'' a $ reverse $ primes $ b+1 else primeFactor' a $ b-1
 primeFactor'' a [] = -1
 primeFactor'' a (h:t) = if a `mod` h == 0 then h else primeFactor'' a t
 -}
--4)
pal = pal' [999, 998..100] [999, 998..100]
 where
 pal' [a,b] [x,y] = 
  let numbers = map (show) [a*x, a*y, b*y]; state = L.find (\e->e == (reverse e)) numbers 
  in if state == Nothing then "-1" else fromJust state
 pal' (a:b:c) (x:y:z) = 
  let numbers = map (show) [a*x, a*y, b*y]; state = L.find (\e->e == (reverse e)) numbers 
  in if state == Nothing then pal' (b:c) (y:z) else fromJust state
  
pdr = foldl (\acc l-> if show l == (reverse $ show l) && l > acc then l else acc) 0 [a*b|a<-[999, 998..100], b<-[999, 998..100]]
--7)
prim n = (filterPrimes [2..]) !! (n-1)
--filterPrimes [] = []
 where
 filterPrimes (h:t) = h : (filterPrimes $ filter (\e -> e `mod` h /= 0) t)
 
--8)
adjDigit :: Int -> [a] -> [[a]]
adjDigit n (h:t)
 |n > (length (h:t)) = []
 |otherwise = let (a, b) = splitAt n (h:t) in a : adjDigit n t

{-
main = do
 hSetBuffering stdin NoBuffering
 hSetBuffering stdout NoBuffering
 numbers <- getContents
--return $ L.maximumBy (\(_,a) (_,b)-> compare a b) $ map (\e -> (e,product $ map (\t-> read t :: Int) e)) $ filter (not . elem '0') $ adjDigit 13 $ filter (isDigit) numbers
 putStrLn $ show $ maximum $ map (\e -> product $ map (\t-> read [t] :: Int) e) $ filter (not . elem '0') $ adjDigit 13 $ filter (isDigit) numbers
 
main = do
 hSetBuffering stdin NoBuffering
 hSetBuffering stdout NoBuffering
 putStrLn $ show [a*b*c| a<-[0..1000], b<-[0..1000], c<-[0..1000], a < b && b < c, a+b+c==1000, a^2 + b^2 == c^2]
 -}
 
--main = putStrLn $ show $ sum $ primes 2000000


filterup :: ([a] -> Bool) -> [[a]] -> [[a]]
filterup f [h] = if f h then [h] else []
filterup f (h:t) = if f h then h : filterup f t else filterup f t

pr n = [2..n-1]

--11)

diagonales :: [[a]] -> [[a]]
diagonales x = if isRectangular x then diagonales' x else [[]]
 where
 diagonales' :: [[a]] -> [[a]]
 diagonales' x = 
  let (h:t) = genLists $ length x-1
  in map  (\e -> zipWith (!!) e h) [x, mirrorY x] ++ concatMap (\e -> map (zipWith (!!) e) t) [x, mirrorY x, mirrorX x, mirrorY $ mirrorX x]

isRectangular :: [[a]] -> Bool
isRectangular (h:t) = all ((length h ==) . length) t

mirrorY :: [[a]] -> [[a]]
mirrorY x = map reverse x

mirrorX :: [[a]] -> [[a]]
mirrorX  = reverse

genLists :: Int -> [[Int]]
genLists 0 = [[0]]
genLists n = (takeWhile (>=0) $ iterate (flip (-) 1) n) : (genLists $ n-1)

{-
main = do
 hSetBuffering stdin NoBuffering
 hSetBuffering stdout NoBuffering
 numbers <- getContents
 let lists = let text = fromRaw numbers in concatMap (filter (not . elem 0)) [text, L.transpose text, filter ((>=4). length) $ diagonales text]
  in putStrLn $ show $ maximum $ map product $ concatMap (adjDigit 4) lists-}

 
fromRaw :: String -> [[Int]]
fromRaw s = map (map (\e -> read e ::Int)) $ map words $ lines s

--12)
triangular :: Int -> [Int]
triangular 1 = [1]
triangular n = let t = triangular $ pred n in t ++ [(last t) + n]

isTriangular :: Int -> Bool
isTriangular n = isTriangular' n 1
 where
 isTriangular' n s
  |n < 0 = False
  |n == 0 = True
  |otherwise = isTriangular' (n-s) (succ s)
  
  
nthTriangular :: Int -> Int
nthTriangular n = nth n 1
 where
 nth n s
  |n < 0 = -1
  |n == 0 = pred s
  |otherwise = nth (n-s) (succ s)
  


factors :: Int -> [Int]
factors 1 = [1]
factors n = let f = minFactor n in 1 : [x|x<-[f..(n `div` f)], n `mod` x == 0] ++ [n]

factors2 :: Int -> [Int]
factors2 1 = [1]
factors2 n = let f = minFactor n; g = n `div` f in 1:f:(fa n [x|x<-[succ f.. pred g]]) ++ [g, n]
 where
 fa _ [] = []
 fa n (h:t) = let (d,m) = n`divMod`h in if m == 0 then (if h == d then [h] else h:(fa n $ takeWhile (<d) t) ++ [d]) else fa n t
 
minFactor :: Int -> Int
minFactor n = head $ dropWhile ((/=0) . mod n) [2..]

maxFactor :: Int -> Int
maxFactor n = n `div` (minFactor n)

factorsLen :: Int -> Int
factorsLen 1 = 1
factorsLen n = let f = minFactor n in sum [1|x<-[f..(n `div` f)], n `mod` x == 0] + 2

findTriangular :: Int -> Int
findTriangular l = findTriangular' 1 2 l
 where 
 findTriangular' :: Int -> Int -> Int -> Int
 findTriangular' n m l = if length (factors n) > l then n else findTriangular' (n + m) (succ m) l
 
findTriangular2 :: Int -> Int
findTriangular2 l = findTriangular' 1 2 l
 where 
 findTriangular' :: Int -> Int -> Int -> Int
 findTriangular' n m l = if factorsLen n > l then n else findTriangular' (n + m) (succ m) l
 
findTriangular3 :: Int -> Int
findTriangular3 l = findTriangular' 1 2 l
 where 
 findTriangular' :: Int -> Int -> Int -> Int
 findTriangular' n m l = if length (factors2 n) > l then n else findTriangular' (n + m) (succ m) l
--13)
{-
main = do
 hSetBuffering stdin NoBuffering
 hSetBuffering stdout NoBuffering
 numbers <- getContents
 putStrLn $ take 10 $ show $ sum $ map (\e -> read e :: Integer) $ lines $ numbers-}
 
 
--14)
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : (collatz $ if even n then n `div` 2 else 3*n +1)

{-
lngcol :: Int -> Int
lngcol n = map (\x-> IntMap.lookup x ((IntMap.singleton 1 [1]))) [2..n]

let m = (IntMap.singleton 1 [1]), 
func :: Int -> IntMap -> [Int]
func n m = let rslt = IntMap.lookup n m in
            if rslt == Nothing 
            then insert n (n:func (if even n then n `div` 2 else 3*n +1) m) m
            else fromJust rslt
  -}          
--String "inserted " ++ show key ++ ":: " ++ show val
{-
insertS :: StateT   (IntMap.IntMap [Int]) String
insertS = StateT   $ \key val m -> ((), IntMap.insert key val m)              

insMap = do
        insertS 2 [1,2]
        insertS 3 [22,32]-}
        
tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

flick :: State (Int,[Int]) ()
flick = do n <- get
           put (fst n, 234:snd n)

insMap :: State (IntMap.IntMap [Int]) ()
insMap = do m <- get
            put mm

mm = IntMap.singleton 1 [1] :: IntMap.IntMap [Int]

insMap' :: Int -> State (IntMap.IntMap [Int]) ()
insMap' n = do m <- get-- state <- monad (state)
               let lst = IntMap.lookup n m--Maybe [Int]
               if lst == Nothing
               then do let next = if even n then n `div` 2 else 3*n +1; m' = execState (insMap' next) m--Int
                       -- insMap' next >>= (\m'-> put $ IntMap.insert n (n:(fromJust $ IntMap.lookup next m')) m')
                       {-insMap' next >>= (\m'-> put $ IntMap.insert n (n:(fromJust $ IntMap.lookup next m')) m')
                       = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  -}
                       put $ IntMap.insert n (n:(fromJust $ IntMap.lookup next m')) m'
               else return ()
               
insMany :: State (IntMap.IntMap [Int]) ()            
insMany  = do insMap' 2
              insMap' 2
              
              
col :: Int -> Int
--col :: Int -> IntMap.IntMap [Int]
col n = fst $ L.maximumBy (compare `on` (length . snd)) $ IntMap.toList (execState (while 2 n) base)
--col n = execState (while 2 n) base
 where 
 base = IntMap.singleton 1 [1] :: IntMap.IntMap [Int]
 while :: Int -> Int -> State (IntMap.IntMap [Int]) ()  
 while i n = if i <= n then insMap' i >> (while (i+1) n) else return ()
{- while i n = if i <= n 
             then do insMap' i 
                     while (i+1) n
             else return ()


collatzCashed 1 = [1]
collatzCashed n = collatzCashed' (IntMap.singleton 1 [1]) n
 where
 collatzCashed' m x = let lst = IntMap.lookup x m in  
  if lst == Nothing 
  then let t = x:(collatzCashed' m (if even n then n `div` 2 else 3*n +1)) in seq (IntMap.insert x t m) t
  else fromJust lst-}
{-
collatzCashed :: Int -> [(Int, [Int])]
collatzCashed n = collatzCashed' 2 [(1, [1])] n
 where
 collatzCashed' :: Int -> [(Int, [Int])] -> Int
 collatzCashed' s l m
  |s == m = l
  |otherwise = let state = lookup s l in
   if state == Nothing
   --then (s,s:collatzCashed')
   else -}
  


filterList n = 1 : (let f = minFactor n in filterList' n [x|x<-[f..(n `div` f)]]) ++ [n]
 where
 filterList' _ [] = []
 filterList' n (h:t) = 
  if n `mod` h /= 0 
  then filterList' n $ filter (\e -> e `mod` h /= 0) t  
  else h : filterList' n t


{-
main = do
 hSetBuffering stdin NoBuffering
 hSetBuffering stdout NoBuffering
 putStrLn $ show $ maximum $ map (length . collatz) [1..400000]-}

--15)
latticeEven :: Int -> Int
latticeEven n
 |even n = let c = n `div` 2 in c + 1 + (latticeEven' c (c-1))
 |otherwise = error "-1"
 where
 latticeEven' :: Int -> Int -> Int
 latticeEven' 1 m = m
 latticeEven' n m = n*m + (latticeEven' (n-1) m)
 
 
binaryString :: Int -> [String]
binaryString 1 = ["0", "1"]
binaryString n = [x:y | x <- "01", y <- binaryString $ n-1]

latticeMeasure  = length . filter (\e -> (length $ L.elemIndices '0' e) == (length $ L.elemIndices '1' e)) . binaryString 


while :: IO Bool -> IO ()
while action = do
                x <- action
                if x
                then do
                    putStrLn "Evaluating"
                    while action
                else return ()          

-- Counts how many strings from 2^n have only m symbols '1' in it
countStrings :: Int -> Int -> Int
countStrings n m
 | m == 0 = 0
 | m == 1 = n
 | m < 0 || n < 0 || m > n = -1
 | otherwise = sum $ zipWith (*) (map (subGroupCount m) groups) (map (countShifts n) groups)
  where
   groups = [minGroupLen .. maxGroupLen]
   maxGroupLen = n
   minGroupLen = m
   countShifts :: Int -> Int -> Int
   countShifts n l = n - l + 1
   subGroupCount :: Int -> Int -> Int
   subGroupCount m l = let c = countStrings (l-2) (m-2) in if c == 0 then 1 else c