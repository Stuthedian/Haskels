import qualified Data.List as L
import Data.Function
import System.Random
--21)
insertAt :: a -> [a] -> Int -> [a]
insertAt e [] 1 = [e] 
insertAt e x@(h:l) i 
 |i == 1 = e : x
 |i > 1 && i <= (length x + 1) = h : (insertAt e l (i-1))
 |otherwise = error "Out of bounds!"
--22)
range :: Int -> Int -> [Int]
range i k = take (k-i+1) [i..]
--23)
rndSelect :: (Show a) => [a] -> Int -> IO [a]
rndSelect list i = do 
 gen <- getStdGen
--putStrLn $ show $ map (\e -> list !! e) $ take i $ randomRs (0, length list - 1) gen
 return $ map (\e -> list !! e) $ take i $ randomRs (0, length list - 1) gen
--24)
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
 gen <- getStdGen
 return $ take n $ randomRs (1, m) gen
--25)
rndPermu :: [a] -> IO [a]
rndPermu list = do
 gen <- getStdGen
 let lst = L.permutations list 
  in return $ lst !! (head $ randomRs (0, length lst - 1) gen)
--26)
combinations :: Int -> [a] -> [[a]]
combinations i x@(h:l) 
 |i > length x || i < 1 = error "Out of bounds!"
 |length x == 1 = [x]
 |i == 1 = [h] : combinations i l
 |length x == i = [x]
 |otherwise = (map (h:)(combinations (i-1) l)) ++ (combinations i l)
--27)
group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group [] _ = error "!!"
group [h] x = map (\e -> [e]) (combinations h x)
--group (h:l) x = map (\e -> map (e++) (group l (x L.\\ e))) (combinations h x)
group (h:l) x = concatMap (\e -> map (e:)(group l (x L.\\ e))) (combinations h x)
--28)
lsort :: [[a]] -> [[a]]
lsort = L.sortBy (\a b -> (length a) `compare` (length b))

lfsort :: (Eq a) => [[a]] -> [[a]]
lfsort x = L.sortBy (\a b -> (length $ helper a x) `compare` (length $ helper b x)) x
 where
 helper t l = foldr (\e acc -> if (length e) == (length t) then e:acc else acc) [] l





