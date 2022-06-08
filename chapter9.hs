-- choices :: [a] ->[[a]]
-- choices [] = [[]]
-- choices (x:xs) = ?
--expected that 
--choices [1,2] = [[], [1], [2], [1,2], [2,1]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map(x:) yss
            where yss = subs xs