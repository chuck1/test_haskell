
applyN n = foldr (.) id . replicate n

fibFunction (x:y:xs) = (x+y):x:y:xs

fib' :: Int -> [Int]
fib' 0 = []
fib' 1 = [1]
fib' 2 = [1,1]
fib' n = fibFunction $ fib' $ n - 1

fibN n = reverse $ fib' n

fibI i = (!!) (fibN $ i + 1) i

fib = map fibI [0..]

main = do putStrLn $ show $ fibN 0
          putStrLn $ show $ fibN 1
          putStrLn $ show $ fibN 2
          putStrLn $ show $ fibN 3
          putStrLn $ show $ fibN 20
          putStrLn $ show $ take 20 fib




