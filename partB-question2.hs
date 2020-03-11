delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs) = if x == v then xs else x : delete v xs
