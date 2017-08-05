import qualified Data.List as L
import Data.Tree

import Trees

newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  

instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  

data State = State [[Int]] deriving (Show)

listInsert i a as = (take i as) ++ [a] ++ (drop (i+1) as)

insert :: Int -> Int -> Int -> State -> State
insert r c v (State as) = State $ listInsert r (listInsert c v (as !! r)) as

maybeInsert r c v (State as) | r >= length as        = Nothing
                             | c >= length (as !! r) = Nothing
                             | (as !! r) !! c > 0    = Nothing
                             | otherwise             = Just $ insert r c v (State as)

filter' f as = filter f' as where f' a = f a as

listJust :: [Maybe a] -> [a]
listJust = foldl foldingFunction []
  where foldingFunction as (Just a) = a:as
        foldingFunction as Nothing  = as

count a as = foldl f 0 as 
  where f acc b | a == b    = acc + 1 
                | otherwise = acc

next :: State -> Int
next (State as) | a <= b = 1 | otherwise = 2
  where a = sum $ map (count 1) as
        b = sum $ map (count 2) as

testRow as | count 1 as == 3 = First $ Just 1
           | count 2 as == 3 = First $ Just 2
           | otherwise       = First $ Nothing

diagonals [[a00,a01,a02],[a10,a11,a12],[a20,a21,a22]] = [[a00,a11,a22],[a02,a11,a20]]

test :: State -> Maybe Int
test (State as) = getFirst $ mconcat [rows, cols, diag] 
  where rows = mconcat $ map testRow as
        cols = mconcat $ map testRow $ L.transpose as
        diag = mconcat $ map testRow $ diagonals as


identical (State a) (State b) | a == b             = True
                              | L.transpose a == b = True
                              | reverse a == b     = True
                              | f a == b           = True
                              | g a == b           = True
                              | h a == b           = True
                              | i a == b           = True
                              | otherwise          = False
  where f =                         reverse . L.transpose
        g =           L.transpose . reverse . L.transpose
        h = reverse . L.transpose . reverse . L.transpose
        i = L.transpose . reverse . L.transpose . reverse . L.transpose

filterIdentical as = foldl f [] as
  where f acc a | any id $ map (identical a) acc = acc
                | otherwise                      = a:acc

moves :: State -> [State]
moves s@(State [[0,0,0],[0,0,0],[0,0,0]]) = [(insert 0 0 1 s), (insert 0 1 1 s), (insert 1 1 1 s)]
moves s                                   = filterIdentical $ moves' s $ test s

moves' s (Just a) = []
moves' s Nothing  = listJust $ f <$> [0..2] <*> [0..2]
  where f r c = maybeInsert r c (next s) s


blank = State [[0,0,0],[0,0,0],[0,0,0]]

score :: State -> Int
score s = score' $ test s

score' (Just 1) = 1
score' (Just 2) = -1
score' Nothing  = 0

main = do
  tree@(Node _ ts) <- return $ buildTree moves blank
  stree <- return $ treeScore score $ prune 5 tree 
  print $ treeLength $ tree
  --print $ map (treeScore score) ts
  --putStrLn $ drawTree $ treeMap show tree
  putStrLn $ drawTree $ treeMap show $ prune 2 $ stree






