
filterPrime (p:xs) = (:) p $ filterPrime $ flip filter xs $ (/= 0) . (`mod` p)

primes = filterPrime [2..]

isPrime x = elem x $ takeWhile (<= x) primes

mersenne = filter isPrime $ map mapFunction primes where mapFunction = (subtract 1) . (2^)

mersenneExponents = filter filterFunction primes where filterFunction = isPrime . (subtract 1) . (2^)

main = do putStrLn $ show $ take 10 primes
          putStrLn $ show $ take 6 mersenneExponents
          putStrLn $ show $ take 6 mersenne




