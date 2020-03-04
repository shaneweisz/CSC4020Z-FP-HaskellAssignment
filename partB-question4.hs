split :: [Int] -> [([Int],[Int])]
split xs = [(take i xs, drop i xs) | i <- [1 .. length xs - 1]]
