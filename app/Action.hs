{-# LANGUAGE OverloadedStrings #-}

-- | A module with the core commands
module Action (
    help,
    helpNoMatchCommand,
    newGame,
    showInfo
)
where

import System.Console.ANSI (
    Color (..),
    ColorIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    setSGR,
 )

import Game.Card (mkKlondikeCardDown)
import Game.Deck
import Game.Solitaire.Persist
import Game.Solitaire.Create
import Game.Solitaire.State

coloredPutStr :: Color -> String -> IO ()
coloredPutStr color msg = do
    setSGR [SetColor Foreground Vivid color] -- Turn color on
    putStr msg
    setSGR [Reset] -- Reset back to default

help :: IO ()
help = do
    putStrLn "Usage: solitaire <command> <command-arguments>"
    putStrLn "The following commands are available:"

    coloredPutStr Green "  help\n"
    putStrLn "      prints this help message."

    coloredPutStr Green "  info\n"
    putStrLn "      prints info about the application such file where data is saved."

    coloredPutStr Green "  new\n"
    putStrLn "      Create a new game. Overwrites existing game data."

helpNoMatchCommand :: String -> IO ()
helpNoMatchCommand cmd = do
    putStr "ERROR: "
    coloredPutStr Red cmd
    putStrLn " is not a known command."
    help

newGame :: IO ()
newGame = do
    shuffledDeck <- shuffleDeck $ createDeck mkKlondikeCardDown
    let solitaire = setupSolitaire shuffledDeck
    saveState solitaire
    putStrLn "A new solitaire game is ready: \n"
    render $ toStrList solitaire

-- TODO Add how many moves have been made
-- TODO Add score
showInfo :: IO ()
showInfo = do
    fileName <- getSavePath
    putStr "Data is persisted at: "
    putStrLn fileName

