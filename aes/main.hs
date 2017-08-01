import Data.Bits
import Data.List
import Data.Maybe

index x xs = fromJust $ elemIndex x xs

rj_xtime :: Int -> Int
rj_xtime x | (x .&. 128) > 0 = xor y 27
           | otherwise       = y where y = (shiftL x 1) .&. 255

gf_alog x = foldl foldingFunction 1 [1..x] where foldingFunction y _ = xor y $ rj_xtime y

gf_log 0 = 0
gf_log x = index x $ map gf_alog [0..]

gf_mulinv 0 = 0
gf_mulinv x = gf_alog(255 - gf_log(x))

test x = a x
 where a y = let b x = 3
             in case y of 0 -> b y
                          _ -> 4

rj_sbox x = xor sb 99
 where sb = snd $ foldl foldingFunction (gf_mulinv x, gf_mulinv x) [0..3]
       foldingFunction (y, sb') _ = 
        let y' = ((shiftL y 1) .&. 255) .|. (shiftR y 7)
        in (y', xor sb' y')

subBytes :: [Int] -> [Int] 
subBytes b = map rj_sbox b

kOffset o k = take 4 (drop ((length k) - o - 4) k)

-- NEED TO STORE KEYS BACKWARDS

expandedKeySize k = (length k) * 4 + 112


-- rounds
-- 4,6,8
--
-- Sub Word(Rot Word(EK((4-1)*4))) XOR Rcon((4/4)-1) XOR EK((4-4)*4)
-- Sub Word(Rot Word(EK((6-1)*4))) XOR Rcon((6/4)-1) XOR EK((6-4)*4)
-- Sub Word(Rot Word(EK((8-1)*4))) XOR Rcon((8/4)-1) XOR EK((8-4)*4)
--

xor' xs ys = zipWith xor xs ys

subWord = subBytes

rotWord xs = drop 3 $ take 7 $ cycle xs

rcon' = [1,2,4,8,16,32,64,128,27,54,108,216,171,77,154]
rcon x = [0,0,0,rcon' !! x]

addWord [a,b,c,d] xs = a:b:c:d:xs

expand''' :: Int -> Int -> [Int] -> [Int]
expand''' rnds a ek = foldl foldingFunction ek [1..(rnds-1)]
 where foldingFunction acc i = let ek1 = kOffset ((a+i-1)*4) acc
                                   ek2 = kOffset ((a+i-4)*4) acc
                               in if (rnds == 8) && (i == 4) then ((xor' (subWord ek1) ek2) `addWord` acc) else ((xor' ek1 ek2) ++ acc)

-- r num of rounds
-- i round index
-- k key
expand'' rnds r ek = ek' ++ ek
 where ek' = xor' (xor' (subWord $ rotWord $ kOffset ((a - 1) * 4) ek) (rcon $ (quot a 4) - 1)) (kOffset ((a - 4) * 4) ek)
       a = rnds * (r + 1)

expand' rnds r ek = expand''' rnds a $ expand'' rnds r ek
 where a = rnds * (r + 1)

quotCeil x y = quot (x + (rem x y)) y

expand :: [Int] -> [Int]
expand k = foldl foldingFunction k [0..n]
 where foldingFunction acc r = expand' rnds r acc
       n = quotCeil ((expandedKeySize k) - (length k)) (4 * rnds) - 1
       rnds = quot (length k) 4

--addRoundKey

hex :: Int -> [Int]
hex 0 = []
hex x = (x .&. 15):(hex (shiftR x 4))

hexS x = map mf (reverse $ hex x)
 where mf x = (['0'..'9'] ++ ['A'..]) !! x

key = [32,31..1]

main = do
 putStrLn $ show $ map rj_xtime [0..255]
 putStrLn "alog"
 putStrLn $ show $ map gf_alog [0..255]
 putStrLn "log"
 putStrLn $ show $ map gf_log [0..255]
 putStrLn "sbox"
 putStrLn $ show $ map hexS $ map rj_sbox [0..15]
 putStrLn $ show $ map hexS $ map rj_sbox [240..255]
 putStrLn $ show $ map hexS rcon'
 putStrLn "key"
 putStrLn $ show $ map hexS $ reverse key
 putStrLn $ show $ length key
 putStrLn "expanded key"
 putStrLn $ show $ map hexS $ reverse $ expand key
 putStrLn $ show $ length $ expand key
 putStrLn $ show $ length $ expand [1..16]
 putStrLn $ show $ length $ expand [1..24]





