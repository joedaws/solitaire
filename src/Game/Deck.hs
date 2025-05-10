module Game.Deck (
    Deck,
    createDeck,
    shuffleDeck,
) where

import Data.List (unfoldr)
import System.Random

import Game.Card

type Deck c = [c]

-- Generate a standard deck of 52 cards in the cannonical order
createDeck :: (Rank -> Suit -> c) -> Deck c
createDeck makeCard =
    [ makeCard r s
    | r <- [Ace .. maxBound]
    , s <- [minBound .. maxBound]
    ]

-- Shuffle the deck
shuffleDeck :: Deck c -> IO (Deck c)
shuffleDeck d = do
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
