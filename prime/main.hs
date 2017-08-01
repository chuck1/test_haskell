
filterPrime (p:xs) = (:) p $ filterPrime $ flip filter xs $ (/= 0) . (`mod` p)

primes = filterPrime [2..]

isPrime x = elem x $ takeWhile (<= x) primes

mersenne = filter isPrime $ map mapFunction primes where mapFunction = (subtract 1) . (2^)

mersenneExponents = filter filterFunction primes where filterFunction = isPrime . (subtract 1) . (2^)

--firstWith p []                 = Nothing
firstWith p (x:xs) | p x       = x
                   | otherwise = firstWith p xs

pFactor 1 = []
pFactor x = factor:(pFactor (x `quot` factor))
 where factor = firstWith (\p -> (x `mod` p) == 0) primes

x = (primes !! 10000) * (primes !! 10001)

main = do putStrLn $ show $ take 10 primes
          putStrLn $ show $ take 5 mersenneExponents
          putStrLn $ show $ take 5 mersenne
          putStrLn $ show $ pFactor 12
          putStrLn $ show $ pFactor 13
          putStrLn $ show $ x
          putStrLn $ show $ pFactor x




