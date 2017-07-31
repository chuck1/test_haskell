import Data.Maybe
import Data.List

p = [[1,2,0,0],[3,4,0,0],[0,0,4,1],[0,0,2,0]]

r = zipWith (\x y -> replicate (length y) x) [0..] p
c = transpose r

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = (take n xs):(groupN n (drop n xs))

everyN n [] = []
everyN n xs = (take n xs):(everyN n (drop (2*n) xs))

defer :: (a -> b -> c) -> (d -> a) -> (e -> b) -> d -> e -> c
defer f g h x y = f (g x) (h y)

puzToSquares puz = zipWith (defer (:) id (replicate 1)) a b
 where a = groupN 4 $ concat $ everyN 2 (concat puz)
       b = groupN 4 $ concat $ everyN 2 (drop 2 (concat puz))

squareIndex = map (map (\x -> flip quot 2 $ x - x `mod` 2))

index2' xs x y = xs !! x !! y

squares puz = zipWith (index2' $ puzToSquares puz) (concat $ squareIndex r) (concat $ squareIndex c)

sub' :: Eq a => [a] -> [a] -> [a]
sub' xs [] = xs
sub' xs (y:ys) = sub' (delete y xs) ys

func (puz,[r,c,s]) | puz /= 0 = puz
                   | (length options') == 1 = head options'
                   | otherwise = 0
                   where options' = sub' [1..4] $ union r $ union c s

inputs' x = zip (concat x) $ transpose [map (x !!) $ concat r, map ((transpose x) !!) $ concat c, squares x]

dumb = (groupN 4) . (map func) . inputs'

main = do putStrLn $ ""
          putStrLn $ show $ dumb p
          putStrLn $ ""




