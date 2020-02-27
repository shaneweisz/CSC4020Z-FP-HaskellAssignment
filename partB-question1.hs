data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

-- eval :: Expr -> Int
--
-- values :: Expr -> [ Int ]
