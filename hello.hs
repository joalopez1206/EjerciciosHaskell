import Data.Char

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                            then x
                            else doubleMe x

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c<-st, c `elem` ['A'..'Z']]

-- 3
n = a `div` (length xs)
    where
        a=10
        xs=[1,2,3,4,5]

{-una manera posible de definir
la funcion last es con !! y length lista -1 
a modo de ejemplo-}
ejemplo = xs !! (length xs - 1)
    where xs = ['p','a','l','a','b','r','a']

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1,2,3,4],[3,2,1],[1,2,3]]

add :: Int -> Int -> Int -> Int
add x y z = x*y*z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

odds :: Int -> [Int]
odds n = map (\x -> 2*x + 1) [0..n-1]

factors :: Int -> [Int]
factors n = [x | x <-[1..n], n `mod` x == 0]
-- capitulo 6

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers str = length [x | x <- str, x>='a' && x<='z']

count :: Char -> String -> Int
count c s = length [x | x <- s, x==c]

{- caesar encoding-}
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr(ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n)`mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n c | c <- s]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
          0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
          6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs str = [percent (count x str) n| x <- ['a'..'z'] ]
        where n = lowers str

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String 
crack xs = encode (- factor) xs
 where
     table' = freqs xs
     chitab = [chisqr (rotate n table') table | n <- [0..25]]
     factor = head(positions(minimum chitab) chitab)
     
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = snoc (reverse xs) x

snoc :: [a] -> a -> [a]
snoc [] a = a:[]
snoc (x:xs) a = x : (snoc xs a) 