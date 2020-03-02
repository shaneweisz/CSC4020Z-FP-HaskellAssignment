delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs)
    | x /= v = [x] ++ delete v xs
    | x == v = xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [i:j | i <- xs, j <- perms (delete i xs)]
