import Data.List
import System.IO
import System.Random
import System.Random.Shuffle as S

newtype OnlyOne a = OnlyOne { getMaybe :: Maybe a }

instance Monoid (OnlyOne a) where
    mempty = OnlyOne Nothing  
    OnlyOne Nothing `mappend` m = m  
    m `mappend` OnlyOne Nothing = m  
    _ `mappend` _ = OnlyOne Nothing

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Bounded, Enum, Show, Read)

data Card = Card Suit Int deriving (Eq, Show, Ord, Read)

cardSuit (Card s _) = s

cardPoints (Card Spades 10) = 13
cardPoints (Card Hearts _) = 1
cardPoints (Card _ _) = 0

cardIsPoint c = (cardPoints) c > 0

cardIsSuit s (Card suit _) = s == suit

type Cards = [Card]

data PlayerType = AI | Human deriving (Show)

data Player = Player Cards Cards PlayerType deriving (Show)

playerPile (Player _ p _) = p

data State = State [Player] deriving (Show)

intToCard x = Card suit v
    where suit = [Clubs ..] !! (quot (x - v) 13)
          v = mod x 13

deal [] = []
deal d = (take 13 d):(deal $ drop 13 d)

playerHas c (Player h _ _) = elem c h

whosGot c ps = getMaybe $ mconcat $ zipWith zipFunction ps [0..3]
    where zipFunction p i | playerHas c p = OnlyOne (Just i)
                          | otherwise     = OnlyOne Nothing

dealToPlayer cs (Player [] [] t) = Player cs [] t

beginRound :: Cards -> State -> State
beginRound d (State ps) = State $ zipWith dealToPlayer (deal d) ps

filterSuit s cards = filter filterFunction cards
    where filterFunction (Card x _) = x == s

playerHasSuitInPile suit (Player _ pile _) = any (cardIsSuit Hearts) pile

playerHasCardInPile f (Player _ pile _) = any f pile

playerHasCardInHand f (Player hand _ _) = any f hand

playerPileLength (Player _ pile _) = length pile

pointsBroken (State ps) = any (playerHasCardInPile cardIsPoint) ps

isFirstTrick s@(State ps) = all f ps where f (Player _ pile _) = null pile

leadOptions s@(State (p@(Player hand pile _):ps)) | hasTheTwo = [Card Clubs 0]
                                                | ((not $ isFirstTrick s) && (pointsBroken s)) || (null nonHeartsInHand) = hand
                                                | otherwise = nonHeartsInHand
    where nonHeartsInHand = filter (\(Card s _) -> s /= Hearts) hand
          hasTheTwo = playerHasCardInHand ((==) (Card Clubs 0)) p

yourMove s@(State ((Player hand pile _):ps)) [] = choose s [] (leadOptions s)

yourMove s@(State ((Player hand pile _):ps)) played = do
    leadSuit <- return $ cardSuit $ (head . reverse) played
    options  <- return $ playOptions leadSuit (filterSuit leadSuit hand) s
    choose s played options

readCard :: [(Card,String)] -> Maybe Card
readCard [(card,_)] = do
    Just card
readCard _ = do
    --putStrLn "error parsing card"
    Nothing

getCard :: Cards -> Maybe Card -> IO Card 
getCard options (Just card) = do
    if (elem card options) then 
      return card 
    else
      getCard options Nothing

getCard options Nothing = do
    putStrLn "enter a card:"
    str <- getLine
    putStrLn ("you entered: " ++ str)
    getCard options $ readCard $ reads str

choose :: State -> Cards -> Cards -> IO Card
choose s@(State ((Player hand pile _):ps)) _ [card] = do
    return card

choose s@(State ((Player hand pile AI):ps)) [] options = do
    return $ head options 

choose s@(State ((Player hand pile AI):ps)) played options = do
    return $ head options 

choose s@(State ((Player hand pile Human):ps)) played options = do
    putStrLn $ "played:  " ++ (show played)
    putStrLn $ "options: " ++ (show options)
    card <- getCard options Nothing
    putStrLn ("the card you picked is: " ++ (show card))
    --if (elem card options) then return card
    return card


countPoints :: Cards -> Int
countPoints = foldl f 0 where f p c = p + cardPoints c

playOptions suit [] s@(State ((Player hand _ _):ps)) | isFirstTrick s = playOptionsFirstTrick s
                                                     | otherwise = hand
    where nonHeartsInHand = filter (\(Card s _) -> s /= Hearts) hand
          nonPointsInHand = filter (not . cardIsPoint) hand
playOptions suit cs (State (p:ps)) = cs

playOptionsFirstTrick (State ((Player hand _ _):ps)) | null nonPointsInHand = filter (cardIsSuit Hearts) hand
                                                     | otherwise = nonPointsInHand
    where nonPointsInHand = filter (not . cardIsPoint) hand

removeFromHand cs (Player hand pile t) = Player (filter (not . flip elem cs) hand) pile t

removeFromHands cs (State ps) = State $ map (removeFromHand cs) ps

shiftLeft x xs = drop x $ take (x + length xs) (cycle xs)

stateShiftLeft n (State ps) = State $ shiftLeft n ps

data Trick = Trick State [Card]

doMove (Trick s@(State [p0,p1,p2,p3]) played) = do
    card <- yourMove s played
    return $ Trick (State [p1,p2,p3,p0]) (card:played)

doMoves :: (IO Trick) -> (IO Trick)
doMoves t = t >>= doMove >>= doMove >>= doMove >>= doMove

putInPile cs s@(State ((Player h pile t):px)) = State ((Player h (pile ++ cs) t):px)

doTrick s@(State ps) = do
    (Trick s' played) <- doMoves $ return (Trick s [])
    rplayed <- return $ reverse played
    leadSuit <- return $ cardSuit $ head rplayed
    Just winner <- return $ elemIndex (maximum (filterSuit leadSuit played)) rplayed
    return $ ((putInPile played) . (stateShiftLeft winner) . (removeFromHands played)) s'

doTrick' s = do
    s@(State [p0,p1,p2,p3]) <- doTrick s
    return s

doRound s@(State ((Player [] _ _):ps)) = do
    return s

doRound s@(State ps) = do
    s@(State ps) <- (pure s) >>= doTrick' >>= doRound
    return s

printPile (Player _ p _) = do
    putStrLn $ "player pile. " ++ (show $ countPoints p) ++ " points"
    mapM_ (putStrLn . ((++) "  ") . show) $ p

main = do
    g <- getStdGen
    d <- return $ S.shuffle' [0..51] 52 g
    s            <- return (State [Player [] [] AI, Player [] [] AI, Player [] [] AI, Player [] [] AI])
    (State ps)   <- return $ beginRound (map intToCard d) s
    (Just i)     <- return $ whosGot (Card Clubs 0) ps
    s@(State ps) <- return $ (State (shiftLeft i ps))
    s@(State ps@([p0,p1,p2,p3])) <- doRound s
    mapM_ printPile ps



