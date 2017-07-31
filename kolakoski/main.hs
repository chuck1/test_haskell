
count :: Eq a => [a] -> [Int]
count [] = []
count (x:xs) = (n+1):(count $ drop n xs)
 where n = length $ takeWhile (==x) xs

kol :: Int -> [Int] -> [Int]
kol i xs | i <= 0    = xs
         | otherwise = kol (i-j) (xs ++ (replicate j $ alternate $ tail' xs))
 where j = next' xs

alternate 1 = 2
alternate 2 = 1

unmatched' xs = drop (length $ count xs) xs 
tail' = (head . reverse)
next' xs = (head . unmatched') xs

main = do putStrLn $ show $ kol 30 [2,2,1,1]


