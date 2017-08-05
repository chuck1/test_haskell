module Trees (
  buildTree,
  prune,
  treeFoldl,
  treeMap,
  treeScore,
  treeLength
  ) where

import Data.Tree


buildTree :: (a -> [a]) -> a -> Tree a
buildTree f a = Node a $ map (buildTree f) (f a)

prune 0 (Node a ts) = Node a []
prune n (Node a ts) = Node a (map (prune (n-1)) ts)

treeFoldl :: (b -> a -> b) -> b -> (Tree a) -> b
treeFoldl f acc (Node a []) = f acc a
treeFoldl f acc (Node a ts) = foldl (treeFoldl f) acc ts

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node a []) = Node (f a) []
treeMap f (Node a ts) = Node (f a) $ map (treeMap f) ts


treeScore :: (a -> Int) -> Tree a -> Tree Int
treeScore f (Node a []) = Node (f a) []
treeScore f (Node _ ts) = Node (maximum $ map rootLabel xs) $ xs
  where xs = map (treeScore' f) ts

treeScore' :: (a -> Int) -> Tree a -> Tree Int
treeScore' f (Node a []) = Node (f a) []
treeScore' f (Node _ ts) = Node (minimum $ map rootLabel xs) $ xs
  where xs = map (treeScore f) ts

treeLength t = treeFoldl f 0 t
  where f acc a = acc + 1



