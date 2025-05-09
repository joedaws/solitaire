module Main (main) where

import Game.Components.State
import Game.Solitaire.State

main :: IO ()
main = do
    shuffledDeck <- shuffleDeck createDeck
    let solitaire  = setupSolitaire shuffledDeck
    render $ toStrList solitaire
