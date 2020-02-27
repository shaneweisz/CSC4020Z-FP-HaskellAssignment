delete :: Int -> [Int] -> [Int]
delete v [] = []
delete v (x:xs) = deleteAux v (x:xs) []
deleteAux v [] p = p
deleteAux v (x:xs) p
        | x /= v = deleteAux v xs (p++[x])
        | x == v = p ++ xs
