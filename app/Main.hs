module Main (main) where

import System.Environment

import Action (help, helpNoMatchCommand, newGame, showHint, showInfo, showGame)

data Command = Help | HelpNoMatch String | New | Info | Hint | Show

{- | The first word is a command and the rest is the argument
  of the command.
-}
parseArgs :: [String] -> (Command, [String])
parseArgs [] = (Help, [])
parseArgs (x : xs) = case x of
    "help" -> (Help, xs)
    "info" -> (Info, xs)
    "new" -> (New, xs)
    "hint" -> (Hint, xs)
    "show" -> (Show, xs)
    _ -> (HelpNoMatch x, xs)

main :: IO ()
main = do
    args <- getArgs
    let (cmd, rest) = parseArgs args
    case cmd of
        Help -> help
        New -> newGame
        HelpNoMatch unknownCmd -> helpNoMatchCommand unknownCmd
        Info -> showInfo
        Hint -> showHint
        Show -> showGame
