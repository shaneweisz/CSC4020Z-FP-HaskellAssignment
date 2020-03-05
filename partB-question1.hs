data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

eval :: Expr -> Int
eval (Val n) = n
eval (App Add e1 e2) = eval(e1) + eval(e2) 
eval (App Mul e1 e2) = eval(e1) * eval(e2) 

-- Example usage:

-- > eval (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- > 7

-- > eval (App Add (Val 2) (Val 3))
-- > 5

-- > eval (Val 2)
-- > 2

values :: Expr -> [Int]
values (Val n) = [n]
values (App o e1 e2) = values e1 ++ values e2

-- Example usage:

-- > values (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- > [1,2,3]

