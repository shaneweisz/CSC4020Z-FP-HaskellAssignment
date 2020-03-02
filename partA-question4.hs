-- (1)

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- (2)

safetail' :: [a] -> [a]
safetail' xs
    | null xs = []
    | otherwise = tail xs

-- (3)

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs
