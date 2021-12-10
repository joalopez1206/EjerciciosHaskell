type Pos = (Int,Int)

data Move = North | South | East | West deriving Show
-- deriving show

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)  = (x+1,y)
move West (x,y)  = (x-1,y)

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 9))

occursEn :: Ord a => a -> Tree a -> Bool
occursEn x (Leaf y)                 = x == y
occursEn x (Node l y r) | x == y    = True
                        | x<y       = occursEn x l
                        | otherwise = occursEn x r

-- Tautology Checker
--extended for Or aplication

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop deriving Show

p1 :: Prop
p1 = And (Var 'A') (And (Var 'A') (Var 'B'))
-- A && (A && B) no tautologia

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
-- A && B => A tautologia

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
-- A => A && B no tautologia

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply(Var 'A') (Var 'B'))) (Var 'B')
-- A && (A => B) => B tautologia

p5 :: Prop
p5 = Or (Var 'A') (Not(Var 'A'))

--tipo de mapeo de un par k -> v
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q

exSubst :: Subst
exSubst = [('A', True), ('B', False)]

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q)  = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools(n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

