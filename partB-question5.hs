data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

instance Show Op where
    show Add = "+"
    show Mul = "*"

instance Show Expr where
    show (Val n) = show n
    show (App o e1 e2) = parenthesize e1 ++ show o ++ parenthesize e2
                         where
                             parenthesize (Val n) = show n
                             parenthesize e = "(" ++ show e ++ ")"


split :: [Int] -> [([Int],[Int])]
split xs = [(take n xs, drop n xs) | n <- [1..m]] where m = length xs - 1

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [App o l r | (ls, rs) <- split ns,
                        l        <- exprs ls,
                        r        <- exprs rs,
                        o        <- [Add, Mul]]

-- > exprs [1,2,3]
-- > [1+(2+3),1*(2+3),1+(2*3),1*(2*3),(1+2)+3,(1+2)*3,(1*2)+3,(1*2)*3]