
foldl' :: (a -> b -> b) -> b -> [a] -> b
foldl' f b []     = b
foldl' f b (a:as) = f a (foldl' f b as)


main = do
    putStrLn $ show $ foldl' (+) 0 [1,2,3,4]

