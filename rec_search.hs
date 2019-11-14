rec_search :: [Int] -> Int
rec_search x
 |length x == 1 = head x
 |head x < last x = head x
 |otherwise = let half = length x `div` 2; a = take half x; b = drop half x in min (rec_search a) (rec_search b)

perms :: String -> [String]
perms "" = [""]
perms [x] = [[x]]
perms x = concat $ map (\(a, b) -> map ((:) a) $ perms b) $ rest x
 where
 rest :: String -> [(Char, String)]
 --rest x = zip (map ($ x) $ map (\l -> last . take l) [1..length x]) (map (\l -> take l x ++ drop (l+1) x) [0..length x - 1])
 rest x = map (\l -> (last $ take l x, take (l-1) x ++ drop l x)) [1..length x]