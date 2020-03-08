data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

eval :: Expr -> Int
eval (Val n) = n
eval (App Add e1 e2) = eval(e1) + eval(e2) 
eval (App Mul e1 e2) = eval(e1) * eval(e2) 

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
split xs = [(take i xs, drop i xs) | i <- [1 .. length xs - 1]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [App o l r | (ls, rs) <- split ns,
                        l        <- exprs ls,
                        r        <- exprs rs,
                        o        <- [Add, Mul]]

solve :: [Int] -> Int -> [Expr]
solve ns t = [e | e <- exprs ns, eval e == t]

-- > solve [1, 2, 3, 4] 10
-- > [1+(2+(3+4)),1+((2+3)+4),1*((2*3)+4),(1+2)+(3+4),(1+(2+3))+4,(1*(2*3))+4,((1+2)+3)+4,((1*2)*3)+4]
