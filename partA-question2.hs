import Prelude hiding (last)

last :: [a] -> a
last xs = head (reverse xs)