



fib' n (x:y:xs) | length xs + 2 >= n = x:y:xs
                | otherwise          = fib' n ((x+y):x:y:xs)

fib n = reverse $ fib' n [1,1]

main = do putStrLn $ show $ fib 10
          putStrLn $ show $ zipWith zipFunction (drop 1 $ fib 20) (fib 20) where zipFunction x y = x/y




