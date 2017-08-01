import Data.List

-- xs length xs nub length (==)
--   xs
--   (length xs)
--   (length xs), xs
--   (length xs), (nub xs)
--   (length xs), (length (nub xs))
--   (length xs) == (length (nub xs))

-- xs dup length swap nub length (==)
--   xs
--   xs, xs
--   xs, (length xs)
--   (length xs), xs
--   (length xs), (nub xs)
--   (length xs), (length (nub xs))
--   (length xs) == (length (nub xs))

-- dup length swap nub length (==)
--   (a -> !!!!! cannot return mutliple things
--               the idea to have a command that can duplicate the top item
--               on the stack will not work. it implies a function with two
--               results.

-- xs $ length $ nub length (==) flip # sameInput
--   xs $ length $ nub length (==) flip # sameInput
--   xs $ length $ (length . nub) (==) flip # sameInput
--   xs $ length $ ((==) . length . nub) flip # sameInput
--   xs $ length $ flip((==) . length . nub) # sameInput             the '#' tells us to eliminate the preceding '$', this also eliminates the #. annihilation!
--   xs $ length flip((==) . length . nub) sameInput               
--   xs $ ((flip ((==) . length . nub)) . length) sameInput      
--   xs $ (sameInput ((flip ((==) . length . nub)) . length))
--           the monster in parentheses here (starting with sameInput) should have type ([a] -> Bool)
--           the working out below should prove this to be so

-- think of the $'s and #'s playing table tennis, we switch modes when we run into either one
--    does this system allow for all possible cominations?
--    PROOF?!

-- [a] -> Int -> Bool
-- Int -> [a] -> Bool
-- [a] -> Bool

-- length $ nub length (==) flip # sameInput
--   ([a] -> Int), $, ([a] -> [a]), ([a] -> Int), (Int -> (Int -> Bool)), flip, #, sameInput
--   ([a] -> Int), $, ([a] -> Int), (Int -> (Int -> Bool)), flip
--   ([a] -> Int), $, ([a] -> (Int -> Bool)), flip
--   ([a] -> Int), $, ([a] -> (Int -> Bool)), flip
--   ([a] -> Int), $, (Int -> ([a] -> Bool)), #, sameInput
--   ([a] -> Int), (Int -> ([a] -> Bool)), #, sameInput
--   ([a] -> ([a] -> Bool)), #, sameInput
--   ([a] -> Bool)

-- I was trying to write the above expression (that takes no parameters and return a function of one parameter)
-- such that if I left-appended it with the parameter

-- length (==)
--   ([a] -> Int), (b -> (b -> Bool))
--   ([a] -> (Int -> Bool))

-- xs length (==)
--   [a], ([a] -> Int), (b -> (b -> Bool))
--   [a], ([a] -> Int), (Int -> (Int -> Bool))
--   Int, (Int -> (Int -> Bool))
--   (Int -> Bool)

-- (a -> b -> c) -> [a] -> ([b] -> [c])
-- [a] -> (a -> b -> c) -> ([b] -> [c])


queens n = filter ff $ permutations [1..n]
 where ff xs = ff' (-) xs && ff' (+) xs
       ff' o = noRepeat . (flip zipWith [1..n] o)
       noRepeat xs = (length xs) == (length $ nub xs)

main = do putStrLn ""
          putStrLn $ show $ length $ permutations [0..7]
          putStrLn $ show $ length $ queens 8





