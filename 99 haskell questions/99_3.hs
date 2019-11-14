import Data.List
--31)
isPrime :: Int -> Bool
isPrime x = (length $ filter ((0 ==) . mod x ) [2..(x-1)]) == 0 

isPrime' :: Int -> Bool
isPrime' x 
 |x < 1 = error "Illegal argument"
 |x == 1 = False
 |x == 2 = True
 |otherwise = helper [2..x-1] 
 where 
 helper :: [Int] -> Bool
 helper [h] = if x `mod` h == 0 then False else True
 helper (h:t) = if x `mod` h == 0 then False else helper t

--32)
mygcd :: Int -> Int -> Int
mygcd a b = mygcd' (abs a) (abs b)
 where
 mygcd' a b = if a /= 0 && b /= 0 then (if a > b then mygcd' (a `mod` b) b else mygcd' a (b `mod` a)) else a + b
--33)
coprime :: Int -> Int -> Bool
coprime a b = (mygcd a b) == 1
--34)
totient :: Int -> Int
totient 1 = 1
totient x = (+) 1 . length . elemIndices True . map  (coprime x) $ [2..x-1]
--35)
primeFactors :: Int -> [Int]
primeFactors x  
 |x == 1 = [1]
 |x < 1 = error "Illegal argument"
 |otherwise = reverse $ helper list_p x 
 where
 list_p = reverse $ primes x
 helper :: [Int] -> Int -> [Int]
 helper _ 1 = []
 helper (h:t) n = if n `mod` h == 0 then h : (helper (h:t) $ n `div` h) else helper t n

markEvery :: Int -> [Int] -> [Int]
markEvery i list
 |i <= 0 = error "Illegal index!"
 |null list = []
 |length list < i = take (i-1) list
 |otherwise = (take (i-1) list) ++ [0] ++ (markEvery i $ drop i list)

composeSieve :: [Int] -> [Int]
composeSieve [e] = [e]
composeSieve (0:t) = 0 : (composeSieve t)
composeSieve (h:t) = h : (composeSieve $ markEvery h t)

primes :: Int -> [Int]
primes x = filter (/=0) $ composeSieve [2..x-1]

filterPrime [] = []
filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
--36)
primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map (\e -> (head e, length e)) . group . primeFactors 
--37)
phi :: Int -> Int
phi 1 = 1
phi x = helper $ primeFactorsMult x
 where
 helper :: [(Int,Int)] -> Int
 helper [(f_,s_)] = (f_ - 1) * f_ ^ (s_ - 1)
 helper ((f_,s_):t) = (f_ - 1) * f_ ^ (s_ - 1) * helper t
--39)
primesR :: Int -> Int -> [Int]
primesR a b = filter (isPrime') [a..b]
--40)
goldbach :: Int -> (Int, Int)
goldbach x
 |x <= 2 || x `mod` 2 /= 0 = error"Illegal argument"
 |otherwise = helper (reverse pr_lst) pr_lst x
 where
 pr_lst = primes x
 helper (a:y) [z] e = if a + z == e then (a, z) else helper y pr_lst e
 helper (a:y) (b:z) e = if a + b == e then (a, b) else helper (a:y) z e
--41)
goldbachList a b = map goldbach [l| l<-[a..b], even l]
goldbachList' a b lim = filter (\(f, s) -> f > lim && s > lim) $ map goldbach [l| l<-[a..b], even l]




