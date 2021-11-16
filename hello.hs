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

{- una manera posible de definir
la funcion last es con !! y length lista -1 
a modo de ejemplo
    -}
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
