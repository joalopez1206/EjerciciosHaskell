--exercise 1 mult function
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

--exercise 3 Balance tree
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves(l) 
                        + numLeaves(r)

balanced :: Tree a -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = (abs (izq - der) <= 1) && 
                        balanced l && balanced r
                        where 
                            izq = numLeaves l
                            der = numLeaves r

t1 :: Tree Int
t1 = Leaf 1

t2 :: Tree Int
t2 = Node (Leaf 1) (Leaf 2)

t3 :: Tree Int
t3 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

t4 :: Tree Int
t4 = Node (Node (Leaf 1) (Leaf 2)) 
     (Node (Leaf 3) (Leaf 4))

t5 :: Tree Int
t5 = Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
             (Leaf 4)

--exercise 4
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
                where 
                  n = length xs
                  half = n `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance left) (balance right)
                where
                    left  = fst(halve xs)
                    right = snd(halve xs)

l1 = [1,2,3,4,5,6]
l2 = [3,2,9]
l3 = [1,6,2,4,8,9,1,4,3,1,24,8,0]

--exercise 5 and 6
data Expr = Val Int | Add Expr Expr

v1 = Add (Add (Val 2) (Val 3)) (Val 4)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

size :: Expr -> Int
size = folde (\x -> 1) (+)

eval2 :: Expr -> Int
eval2 = folde (id) (+)