{-example of the chapter insertion sort-}
--given a sorted list keeps it sorted
insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a  => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs) 

{-Modify the factorial function-}
-- Modification : if n < 0 then we define it to be 0
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | n < 0 = 0

{-defining sumdown-}
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

{-defining ^ function (we called it pow)-}
pow :: Int -> Int -> Int
m `pow` 0 = 1
m `pow` n = m * (m `pow` (n-1))

{-defining euclid-}
euclid :: Int -> Int -> Int
euclid x y | x==y      = x
           | x<y       = euclid x (y-x)
           | otherwise = euclid (x-y) y 

{-exercise 5
length [1,2,3] = 1 + length [2,3]
               = 1 + (1 + length [3])
               = 1 + (1 + (1 + length []))
               = 3

drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5]
                   = drop 1 [3,4,5]
                   = drop 0 [4,5]
                   = [4,5]

init [1,2,3] = 1 : init [2,3]
             = 1 : (2 : init [3])
             = 1 : (2 : [])
             = [1,2]
-}

{-defining and exercise-}
and1 :: [Bool] -> Bool
and1 []     = True
and1 (x:xs) = x && and1 xs

{-defining concat exercise-}
concat2 :: [[a]] -> [a]
concat2 []     = []
concat2 (x:xs) = x ++ concat2 xs

{-defining replicate exercise-}
replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : replicate2 (n-1) x

{-defining selection exercise-}
nth :: [a] -> Int -> a
nth (x:_) 0  = x
nth (_:xs) n = nth xs (n-1)

{-Is in list exercise-}
elem1 :: Eq a => a -> [a] -> Bool
elem1 _ []     = False
elem1 a (x:xs) = a == x || elem1 a xs

{-merge function exercise-}
--given 2 sorted list returns a sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)  | x > y     = y : merge (x:xs) ys
                     | otherwise = x : merge xs (y:ys)

{-merge sort exercise-}
halve :: [a] -> ([a],[a])
halve xs = (take (n `div` 2) xs , drop (n `div` 2) xs )
                where n = length xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
            where 
               (ys,zs) = halve xs

{------------------------------------------
defining calculate sum of list with the steps
step 1: defining the type

we expect that if we take a sum of list then takes a list
and then gives a number, per instance lets take integers
sumlist :: [Int] -> Int

step 2: enumerate the cases

we have 2 cases, when the list is empty and where is not and
we should look the first element
sumlist [] = 
sumlist (n:ns) []

step 3: defining the simple cases

if the list is empty then we are not adding anything so
it should be 0
sumlist [] = 0 

step 4: defining the other cases

if the list is not empty then we should look the first element
and add that recursively
sumlist (n:ns) = n + sumlist ns

step 5: generalise and simplify
note that we can add any type of number so we should have
sumlist :: Num a => [a] -> a 
and with the defintion given with the foldr function we
can define sumlist easily
sumlist = foldr (+) 0
we should have note that foldr function 
is a short for the pattern that was described earlier 
takes a operator, a base case and a list
-----------------------------------------------}

sumlist :: Num a => [a] -> a
sumlist = foldr (+) 0

-- trying define foldr ? --
foldrxd :: (a -> a -> a) -> a -> [a] -> a
foldrxd op a []     = a
foldrxd op a (x:xs) = x `op` (foldrxd op a xs)

{-the other ones are left as a exercise-}

{-take a given number of elements from the start of a list-}
take_2 :: Int -> [a] -> [a]
take_2 0 _      = []
take_2 _ []     = []
take_2 n (x:xs) = x : take_2 (n-1) xs 

{-select the last element of a non-empty list-}
last1 :: [a] -> a
last1 (x:[]) = x
last1 (_:xs) = last1 xs