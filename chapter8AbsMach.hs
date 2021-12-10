data Expr = Val Int | Add Expr Expr | Mult Expr Expr

-- value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y
v1 :: Expr
v1 = Add (Add (Val 2) (Val 3)) (Val 4)
v2:: Expr
v2 = Add (Mult (Val 3) (Val 5)) (Val 1)

data Op = EVAL1 Expr | EVAL2 Expr | ADD Int | MULT Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n)     c = exec c n
eval (Add x y)   c = eval x (EVAL1 y : c)
eval (Mult x y)  c = eval x (EVAL2 y : c)

exec :: Cont -> Int -> Int
exec [] n           = n
exec (EVAL1 y : c) n = eval y (ADD n : c)
exec (EVAL2 y : c) n = eval y (MULT n : c)
exec (ADD n : c)  m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []