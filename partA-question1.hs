prod :: Num p => [p] -> p
prod [] = 1
prod (x:xs) = x * prod xs

-- OR
-- prod2 [] = 1
-- prod2 xs = head xs * prod2 (tail xs)
