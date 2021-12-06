import Data.Char

type Bit = Int

bit2intbad :: [Bit] -> Int
bit2intbad bits = sum [w*b | (w,b) <- zip weights bits]
                where weights = iterate (*2) 1

bit2int :: [Bit] -> Int
bit2int = foldr(\x y -> x + y*2) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

--transmision part
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8(drop 8 bits)

decode :: [Bit] -> String
decode = map(chr . bit2int) . chop8
