delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs) = if x == v then xs else x : delete v xs

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [i:js | i <- xs, js <- perms (delete i xs)]
