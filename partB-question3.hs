delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs) = if x /= v then [x] ++ delete v xs else xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [i:j | i <- xs, j <- perms (delete i xs)]
