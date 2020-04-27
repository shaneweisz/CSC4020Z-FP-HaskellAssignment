import Prelude hiding (product)

product :: Num p => [p] -> p
product [] = 1
product (x:xs) = x * product xs