prod [] = 1
prod (x:xs) = x * prod xs

-- OR ? DOESNT WORK
-- prod2 [] = 1
-- prod2 xs = head xs * prod2 tail xs
