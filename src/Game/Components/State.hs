-- | Module for defining data types for Cards
module Game.Components.State (
    CardState (..),
    Card (..),
    Suit (..),
    Rank (..),
    DeckState,
    Deck,
    Color,
    color,
    createDeck,
    createDeckState,
    shuffleDeck,
    shuffleDeckState,
) where

import Data.List (unfoldr)
import System.Random

data Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades
    deriving (Enum, Bounded, Eq)

instance Show Suit where
    show s = case s of
        Hearts -> "H"
        Diamonds -> "D"
        Clubs -> "C"
        Spades -> "S"

data Rank
    = EmptyRank
    | Ace
    | Two
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
    deriving (Enum, Bounded, Eq, Ord)

instance Show Rank where
    show r = case r of
        EmptyRank -> "-"
        Ace -> "A"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Five -> "5"
        Six -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine -> "9"
        Ten -> "X"
        Jack -> "J"
        Queen -> "Q"
        King -> "K"

-- Define a card as a combination of a rank and a suit
data Card = Card
    { rank :: Rank
    , suit :: Suit
    }
    deriving (Eq)

instance Show Card where
    show (Card r s) = show r ++ show s

data Face = Up | Down deriving (Show, Eq)

data CardState = CardState
    { card :: Card
    , face :: Face
    }
    deriving (Eq)

instance Show CardState where
    show (CardState _ Down) = "**"
    show (CardState c Up) = show c

data Color = Red | Black deriving (Show, Eq)

color :: Card -> Color
color (Card _ Hearts) = Red
color (Card _ Diamonds) = Red
color (Card _ Clubs) = Black
color (Card _ Spades) = Black

type Deck = [Card]
type DeckState = [CardState]

-- Generate a standard deck of 52 cards in the cannonical order
createDeck :: Deck
createDeck =
    [ Card r s
    | r <- [Ace .. maxBound]
    , s <- [minBound .. maxBound]
    ]

createDeckState :: DeckState
createDeckState =
    [CardState c Down | c <- createDeck]

-- Shuffle the deck
shuffleDeck :: Deck -> IO Deck
shuffleDeck d = do
    shuffle' d (length d) <$> newStdGen

-- Shuffle the deck with CardState
shuffleDeckState :: DeckState -> IO DeckState
shuffleDeckState d = do
    shuffle' d (length d) <$> newStdGen

-- Fisher-Yates shuffle algorithm
shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' xs n gen = unfoldr select (xs, n, gen)
  where
    select ([], _, _) = Nothing
    select (ys, k, g) =
        let (i, g') = randomR (0, k - 1) g
            (front, (a : back)) = splitAt i ys
         in Just (a, (front ++ back, k - 1, g'))
