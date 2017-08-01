
-- g :: a -> b
-- f :: b -> c
--
-- (f . g) a = f (g a)

-- g :: a -> (b -> c)
-- f :: c -> d
--
-- (conj f g) a b = f (g a b)

-- (conj f g) a = f . (g a)

comp :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(comp f g) a' b' = f (g a' b')

--conj f g = \a' b' -> f (g a' b')

-- new syntax:
--   func = (-) (/3) comp

subAndDivThree = comp (/3) (-)

main = do putStrLn ""
          putStrLn $ show $ subAndDivThree 11 2
          putStrLn $ show $ comp (/) (-) 11 2 3




