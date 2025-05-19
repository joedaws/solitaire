module Main (main) where

import Game.Card
import Game.Deck
import Game.Solitaire.Create
import Game.Solitaire.Hint
import Game.Solitaire.State

main :: IO ()
main = do
    shuffledDeck <- shuffleDeck $ createDeck mkKlondikeCardDown
    let solitaire  = setupSolitaire shuffledDeck
    putStrLn "Here is a solitaire state\n"
    render $ toStrList solitaire
    putStrLn "Next possible states are\n"
    let ss = hint solitaire
    mapM_ print ss
