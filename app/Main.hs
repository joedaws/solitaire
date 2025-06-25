module Main (main) where

import Game.Card
import Game.Deck
import Game.Solitaire.Create
import Game.Solitaire.Hint
import Game.Solitaire.State

main :: IO ()
main = do
    -- to make a thoughtful solitarie game, just pass in all the 
    -- Up Cards
    shuffledDeck <- shuffleDeck $ createDeck mkKlondikeCardDown
    let solitaire = setupSolitaire shuffledDeck
    putStrLn "Here is a solitaire state\n"
    render $ toStrList solitaire
    putStrLn "Next possible states are\n"
    let hints = hintTrace solitaire
    mapM_ renderHint (zip [1..] hints)

    where
    renderHint (n, (_f, s)) = do
        putStrLn $ "\nHint #" ++ show n ++ ":"
        render $ toStrList s
