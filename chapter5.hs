{-exercise 1-}
sumsqr :: Int -> Int
sumsqr n = sum [x^2 | x <- [1..n]]

{-exercise 2-}
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

{-exercise 3-}
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x/=y ]

{-exercise 4-}
replicate2 :: Int -> a -> [a]
replicate2 n a = [a | _ <-[1..n] ]

{-exercise 5-}
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

{-exercise 6-}
factor :: Int -> Int -> Bool
factor n x = x `mod` n == 0

factors :: Int -> [Int]
factors n = [x | x <-[1..n], factor x n]

perfect :: Int -> Bool
perfect n = sum (init (factors n)) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

{-exercise 7-}
list = concat [[(x,y) | y <- [3..4]] | x <- [1..2]]

{-exercise 8 redefine the function possitions using find -}
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

{-exercise 9-}
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

