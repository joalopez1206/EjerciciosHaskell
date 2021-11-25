{-express the comprehension [f x | x <- xs, p x] with maps and filters-}
comp :: (a -> b) -> (a -> Bool) -> [a] -> [b] 
comp f p xs = map f (filter p xs)

{-defining map with foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
map f [1,2,3] = f 1 : (f 2 : (f 3 : []))

filter :: (a->Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs
-}

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\x xs -> f x : xs ) []

filterxd :: (a -> Bool) -> [a] -> [a]
filterxd p = foldr (\x xs -> if p x then x : xs else xs) []

{-Without looking at the definitions from the standard prelude, define the following higher-order
library functions on lists.-}

--Decide if all elements of a list satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x xs -> p x && xs) True

-- all' p [] = True
-- all' p (b:bs) = p b && all' p bs

--Decide if any element of a list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x xs -> p x || xs) False

-- any' p [] = False
-- any' p (b:bs) = p b || any' p bs

--Select elements from a list while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x : xs else []) [] 

-- takeWhile' p [] = []
-- takeWhile' p (b:bs) | p b = b : takeWhile' p bs
--                     | otherwise = []

--Remove elements from a list while they satisfy a predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (b:bs) | p b = dropWhile' p bs
                    | otherwise = (b:bs)

dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> 10*x + xs) 0

--trying with recursion instead of foldl
dec2intrec :: [Int] -> Int
dec2intrec  = go1 0

go1 :: Int -> [Int] -> Int
go1 v [] = v
go1 v (x:xs) = go1 (10*v + x) xs

curryxd :: ((a,b) -> c) -> (a -> b -> c)
curryxd f x y = f (x,y)

uncurry2 :: (a -> b -> c) -> (a,b) -> c
uncurry2 f (x,y) = f x y 


-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f v [] = v
-- foldl f v (x:xs) = foldl f (f v x) xs