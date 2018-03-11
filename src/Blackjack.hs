{-# LANGUAGE TemplateHaskell #-}
module Blackjack where
  -- ( Score
  -- , Action (..)
  -- , Hand
  -- , PlayerRole
  -- , blackjack
  -- , score
  -- , GameState
  -- , Strategy
  -- ) where

import Card
import Deck
import Lens.Micro.Platform
import Data.Map (Map)
import Data.List.Zipper (Zipper, fromList, toList, duplicatez, cursor, replace)
import Control.Monad (when, forM_)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State.Lazy (StateT, liftIO, get, put, modify)
import Data.Text (Text)
import Control.Monad.Except (MonadError, throwError)

type Score = Int

blackjack :: Score
blackjack = 21

isBlackjack :: [Card] -> Bool
isBlackjack [x,y] = (isAce x && isFace y) || (isAce y && isFace x)

value :: Card -> [Score]
value c
  | isAce  c  = [1, 11]
  | isFace c  = [10]
  | otherwise = [fromEnum (rank c) + 2]

score :: [Card] -> Score
score cs = let combinations []     = [[]]
               combinations (x:xs) = [p : q | p <- x, q <- combinations xs]
               scores    = fmap sum $ combinations $ fmap value cs
               available = filter (<= blackjack) scores
            in if null available
                  then head scores
                  else maximum available

newtype Hand = Hand { unHand :: [Card] }
  deriving Show

type ParticipantID = Int

data Status = Busted | Active
  deriving (Show, Eq, Ord)

data Action = Stand | Hit
  deriving (Show, Read, Eq)

type Strategy = ReaderT GameState IO Action

data ParticipantInfo = PI { _status :: Status
                          , _hand   :: Hand
                          } deriving Show

dummyPI :: ParticipantInfo
dummyPI = PI { _status = Active
             , _hand   = Hand []
             }

data Player = Player { _piP      :: ParticipantInfo
                     , _strategy :: Strategy
                     }

data Dealer = Dealer { _piD        :: ParticipantInfo
                     , _hiddenCard :: Card
                     }

data GameState = GS { _players' :: Zipper (ParticipantID, ParticipantInfo)
                    , _dealer'  :: ParticipantInfo
                    , _deckSz   :: Int
                    , _wasteSz  :: Int
                    } deriving Show

data GameStateReal = GSR { _players :: Zipper (ParticipantID, Player)
                         , _dealer  :: Dealer
                         , _deck    :: Deck
                         , _waste   :: [Card]
                         }

makeLenses ''ParticipantInfo
makeLenses ''Player
makeLenses ''Dealer
makeLenses ''GameState
makeLenses ''GameStateReal

shatter :: GameStateReal -> Zipper GameState
shatter gsr = let ps   = gsr ^. players
                  zps  = duplicatez $ ps & mapped . _2 %~ _piP
                  tmpl x = GS { _players' = x
                              , _dealer'  = gsr ^. dealer . piD
                              , _deckSz   = gsr ^. deck . to deckSize
                              , _wasteSz  = gsr ^. waste . to length
                              }
               in fmap tmpl zps

dealerStrategy :: Strategy
dealerStrategy = do
  dCards <- unHand . _hand . _dealer' <$> ask
  return $ if score dCards <= 17
        then Hit
        else Stand

humanStrategy :: Strategy
humanStrategy = do
  x <- ask
  liftIO $ print x
  liftIO readLn

type BJ = StateT GameStateReal IO

twoCardsForTheDealer :: BJ ()
twoCardsForTheDealer = do
  (Just h, Just e) <- zoom deck $ do
    c1 <- takeTopCard
    c2 <- takeTopCard
    return (c1, c2)
  zoom (dealer . hiddenCard) $ put h
  zoom (dealer . piD . hand) $ put $ Hand [e]

newGame :: BJ ()
newGame = do
  d <- mkDeckRandom
  let dummyDealer = Dealer { _piD        = dummyPI
                           , _hiddenCard = undefined
                           }
  put $ GSR (fromList [(0, Player { _piP = dummyPI, _strategy = humanStrategy })]) dummyDealer d []
  return ()

itsYourTurn :: BJ ()
itsYourTurn = do
  ps <- zoom players get
  let you = snd $ cursor ps
      curScore = score $ you ^. piP . hand . to unHand
  if curScore <= blackjack
     then do
       gs <- cursor . shatter <$> get
       ans <- liftIO $ runReaderT (you ^. strategy) gs
       when (ans == Hit) $ do
            (Just c) <- zoom deck takeTopCard
            let newPI = you & piP . hand %~ (Hand . (c:) . unHand)
            zoom players $ modify $ replace (fst $ cursor ps, newPI)
            itsYourTurn
     else do
       let newPI = you & piP . status .~ Busted
       zoom players $ modify $ replace (fst $ cursor ps, newPI)
       liftIO $ print "Busted!"
  return ()

dealersTurn :: BJ ()
dealersTurn = do
  d <- zoom dealer get
  let h = d ^. hiddenCard
      dh = (h:) $ d ^. piD . hand . to unHand
      curScore = score dh
  if curScore <= blackjack
     then do
       gs <- cursor . shatter <$> get
       let gs' = gs & dealer' . hand .~ Hand dh
       ans <- liftIO $ runReaderT dealerStrategy gs'
       when (ans == Hit) $ do
            (Just c) <- zoom deck takeTopCard
            let newPI = d & piD . hand %~ (Hand . (c:) . unHand)
            zoom dealer $ put newPI
            dealersTurn
     else do
       let newPI = d & piD . status .~ Busted
       zoom dealer $ put newPI
       liftIO $ print "Dealer busted!"
  return ()

twoCardsForYou :: BJ ()
twoCardsForYou = do
  Just c1 <- zoom deck takeTopCard
  Just c2 <- zoom deck takeTopCard
  ps <- zoom players get
  let you = snd $ cursor ps
      newPI = you & piP . hand %~ (Hand . ([c1,c2]++) . unHand)
  zoom players $ modify $ replace (fst $ cursor ps, newPI)
  return ()

round1 :: BJ ()
round1 = do
  ps <- zoom players get
  mapM_ (const twoCardsForYou) (toList ps)
  twoCardsForTheDealer
  ps <- zoom players get
  let instawinners = filter isBlackjack $ fmap (unHand . _hand . _piP . snd) (toList ps)
  forM_ instawinners $ \_ ->
    liftIO $ print "You win!"
  return ()

advance :: BJ ()
advance = do
  ps <- zoom players get
  mapM_ (const itsYourTurn) (toList ps)
  d <- _dealer <$> get
  gs <- cursor . shatter <$> get
  let h = d ^. hiddenCard
      dh = (Hand . (h:) . unHand) $ gs ^. dealer' . hand
  dealersTurn
  ps <- zoom players get
  d <- _dealer <$> get
  let dh = (Hand . (h:) . unHand) $ gs ^. dealer' . hand
  let active = filter ((== Active) . _status) $ fmap (_piP . snd) (toList ps)
  if null active
     then liftIO $ print "Dealer wins!"
     else liftIO $ do
       print "Players:"
       print $ fmap (score . unHand . _hand) active
       print "Dealer:"
       print $ (score . unHand) dh
  return ()
