import qualified Data.List as L
import Data.Tree

import Trees

newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  

instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  


data Piece = Rook | Bishop deriving (Show)
data Color = White | Black deriving (Eq, Show)
data Coor = Coor Int Int deriving (Eq, Show)

data Position = Position Piece Color Coor deriving (Show)

replaceCoor p@(Position pc c _) coor = Position pc c coor

data State = State Color [Position] deriving (Show)

coorFromTo (Coor x1 y1) (Coor x2 y2) | y1 == y2 = map fx [x1..x2]
                                     | x1 == x2 = map fy [y1..y2]
  where fx x = Coor x y1
        fy y = Coor x1 y

otherColor White = Black
otherColor Black = White

doMove s@(State who ps) (Position _ _ ca) pb@(Position _ _ cb) = State (otherColor who) (pb:(filter f ps))
  where f (Position _ _ c) | c == ca   = False
                           | c == cb   = False
                           | otherwise = True

movesP :: State -> Position -> [State]
movesP s p@(Position Rook c coor@(Coor x y)) = map (doMove s p) positions
  where positions = filter f (east ++ west ++ north ++ south)
        f (Position _ _ coor2) = coor /= coor2
        west   = map (replaceCoor p) $ coorFromTo (limitW s p coor) coor
        east   = map (replaceCoor p) $ coorFromTo coor (limitE s p coor)
        south  = map (replaceCoor p) $ coorFromTo (limitS s p coor) coor
        north  = map (replaceCoor p) $ coorFromTo coor (limitN s p coor)

moves :: State -> [State]
moves s@(State who ps) = concat $ map (movesP s) $ filter f ps
  where f (Position _ c _) = c == who

--limitW s p@(Position _ _ _ y) 0 = 0
--limitW s p@(Position _ _ _ y) x = limitW' s p (x-1) $ checkSquare s (x-1) y

isEdge (Coor 0 _) = True
isEdge (Coor 7 _) = True
isEdge (Coor _ 0) = True
isEdge (Coor _ 7) = True
isEdge _ = False

limitE s p coor@(Coor x0 y0) = limit s p coor fwd bac edge (coor == edge) Nothing
  where fwd (Coor x y) = Coor (x + 1) y
        bac (Coor x y) = Coor (x - 1) y
        edge = Coor 7 y0

limitW s p coor@(Coor x0 y0) = limit s p coor fwd bac edge (coor == edge) Nothing
  where fwd (Coor x y) = Coor (x - 1) y
        bac (Coor x y) = Coor (x + 1) y
        edge = Coor 0 y0

limitN s p coor@(Coor x0 y0) = limit s p coor fwd bac edge (coor == edge) Nothing
  where fwd (Coor x y) = Coor x (y + 1)
        bac (Coor x y) = Coor x (y - 1)
        edge = Coor x0 7

limitS s p coor@(Coor x0 y0) = limit s p coor fwd bac edge (coor == edge) Nothing
  where fwd (Coor x y) = Coor x (y - 1)
        bac (Coor x y) = Coor x (y + 1)
        edge = Coor x0 0

limit :: State -> Position -> Coor -> (Coor -> Coor) -> (Coor -> Coor) -> Coor -> Bool -> Maybe Position -> Coor
limit s p@(Position _ c _) coor fwd bac edge True Nothing                              = coor
limit s p@(Position _ c _) coor fwd bac edge _    Nothing                              = limit s p (fwd coor) fwd bac edge ((fwd coor) == edge) $ checkSquare s (fwd coor)
limit s p@(Position _ c _) coor fwd bac edge _    (Just (Position _ c2 _)) | c == c2   = bac coor
                                                                      | otherwise = coor

checkSquare :: State -> Coor -> Maybe Position
checkSquare (State _ ps) c1 = getFirst $ mconcat $ map f ps
  where f p@(Position _ _ c2) | c1 == c2  = First (Just p)
                              | otherwise = First Nothing

rookLineup = State White $ (map (f White 0) [3..4]) ++ (map (f Black 7) [3..4])
  where f c y x = Position Rook c (Coor x y)

score :: Color -> State -> Int
score c (State _ ps) = foldl f 0 ps
  where f acc (Position _ c2 _) | c == c2   = acc + 1
                                | otherwise = acc

scoreW = score White

doTurn :: State -> State
doTurn s = s

main = do
  s@(State _ (p:ps)) <- return rookLineup
  tree@(Node _ ts) <- return $ buildTree moves s
  stree@(Node _ scores) <- return $ treeScore scoreW $ prune 4 tree 

  print $ zip (map rootLabel $ subForest tree) (map rootLabel $ subForest stree)

  --print $ map (treeScore (score White). prune 3) ts
  --putStrLn $ drawTree $ treeMap show tree
  --putStrLn $ drawTree $ treeMap show $ prune 2 $ stree
  --print $ movesP s p





