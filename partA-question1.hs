prod :: Num p => [p] -> p
prod [] = 1
prod (x:xs) = x * prod xs