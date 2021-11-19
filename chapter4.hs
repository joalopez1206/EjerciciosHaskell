{- Halves ercersise -}
halve :: [a] -> ([a],[a])
halve xs = (take (length(xs) `div` 2) xs ,
                drop (length(xs) `div` 2) xs )

{- third excersise -}

third1 :: [a] -> a
third1 xs = head (tail(tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

{- mult to lambdas exercise-}

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

{- safetail Exercises from the video "FP 6 - Defining Functions" -}

safetail :: [a] -> [a]
safetail xs = if null xs 
                then []
                else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 (_:xs) = xs
safetail3 _ = []

{- or Excersises from the video "FP 6 - Defining Functions" -}

or1 :: Bool -> Bool -> Bool
False `or1` False = False
_     `or1` _     = True

or2 :: Bool -> Bool -> Bool
False `or2` b  = b
True  `or2` _  = True

or3 :: Bool -> Bool -> Bool
True  `or3` True   = True
True  `or3` False  = True
False `or3` True   = True
False `or3` False  = False

{- both and Excersises from the video "FP 6 - Defining Functions" -}

and1 :: Bool -> Bool -> Bool
and1 p q = if p == True
              then if q == True
                        then True
                        else False 
              else False

and2 :: Bool -> Bool -> Bool
and2 p q = if p == True
              then q
              else False 

{-Luhn excersise-}
luhnDouble :: Int -> Int
luhnDouble x = if 2*x <= 9 
                    then
                        2*x
                    else
                        2*x-9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if ((a + luhnDouble b + c + luhnDouble d) `mod` 10) == 0
                then 
                    True
                else
                    False