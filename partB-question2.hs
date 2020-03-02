delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs) = if x /= v then [x] ++ delete v xs else xs
