import Data.List

sub' :: Eq a => [a] -> [a] -> [a]
sub' xs [] = xs
sub' xs (y:ys) = sub' (delete y xs) ys



data Tree a = EmptyTree | Node a [Tree a] deriving (Show, Read, Eq)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a xs)
    | x == a = True  
    | otherwise = any id $ map (treeElem x) xs

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = (c `elem` [1..8] && r `elem` [1..8])  

pass root EmptyTree = EmptyTree
pass root (Node a []) = Node a [Node x [] | x <- (moveKnight a)]
pass root (Node a xs) = Node a $ map (pass root) xs

m = Node (0,0) []

main = do
  putStrLn $ show $ moveKnight (0,0)
  putStrLn $ show $ Node (0,0) []
  putStrLn $ show $ pass m m
  putStrLn $ show $ foldl (\acc _ -> pass acc acc) m [0..5]






