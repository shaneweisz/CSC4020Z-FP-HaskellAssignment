-- (1)

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- (2)

safetail' :: [a] -> [a]
safetail' xs | not (null xs) = tail xs
             | otherwise = []

-- (3)

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs
