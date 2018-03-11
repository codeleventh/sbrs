{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Deck
  ( Deck
  , mkDeckRandom
  , deckSize
  , isAlmostEmpty
  , takeTopCard
  ) where

import Card
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Except (MonadError, throwError)
import System.Random.Shuffle (shuffleM)
import Data.Text (Text)

newtype Deck = Deck { unDeck :: [Card] }
  deriving Show

mkDeck :: Deck
mkDeck = Deck $ fmap (uncurry Card) [ (s, r) | s <- [Spades ..], r <- [Two ..] ]

deckSize :: Deck -> Int
deckSize = length . unDeck

isAlmostEmpty:: Deck -> Bool
isAlmostEmpty = (<= (deckSize mkDeck `div` 3)) . deckSize

takeTopCard :: MonadState Deck m => m (Maybe Card)
takeTopCard = do
  cs <- unDeck <$> get
  if null cs
     then return Nothing
     else do
       put $ Deck $ tail cs
       return $ Just $ head cs

mkDeckRandom :: MonadRandom m => m Deck
mkDeckRandom = fmap Deck $ shuffleM $ unDeck mkDeck
