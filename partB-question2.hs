delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs)
    | x /= v = [x] ++ delete v xs
    | x == v = xs
