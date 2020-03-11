split :: [Int] -> [([Int],[Int])]
split xs = [(take n xs, drop n xs) | n <- [1..m]] where m = length xs - 1
