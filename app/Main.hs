module Main (main) where

import Game.Card
import Game.Deck
import Game.Solitaire.State
import Game.Solitaire.Create

main :: IO ()
main = do
    shuffledDeck <- shuffleDeck $ createDeck mkGameCardDown
    let solitaire  = setupSolitaire shuffledDeck
    render $ toStrList solitaire
