module Card
  ( Card (..)
  , Suit (..)
  , Rank (..)
  , isPip
  , isFace
  , isAce
  ) where

data Suit = Spades | Clubs | Diamonds | Hearts
  deriving (Show, Eq, Ord, Enum)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Show, Eq, Ord, Enum)

data Card = Card { suit :: Suit
                 , rank :: Rank
                 }
                 deriving (Show, Eq, Ord)

isPip :: Card -> Bool
isPip = not . isFace

isFace :: Card -> Bool
isFace = (`elem` [Jack .. King]) . rank

isAce :: Card -> Bool
isAce = (== Ace) . rank
