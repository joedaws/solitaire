{-# LANGUAGE OverloadedStrings #-}

module Game.Solitaire.State (
    Solitaire (..),
    Foundations (..),
    Tableau (..),
    BuildPile,
    SuitPile,
    Waste,
    Stock,
    setupSolitaire,
    setupFoundations,
    toStrList,
    render,
) where

import Data.List (intercalate)

import Game.Components.State

-- Synonyms for different list of Cards
-- head [Card] is the top of the stack of cards
type Waste = [Card]

type SuitPile = [Card]

-- Regular piles can only grow with alternating color
type BuildPile = [Card]

type Stock = [Card]

-- There are four foundations, one for each suit
data Foundations = Foundations
    { heartsPile :: SuitPile
    , diamondsPile :: SuitPile
    , clubsPile :: SuitPile
    , spadesPile :: SuitPile
    }
    deriving (Show)

data Tableau = Tableau
    { one :: BuildPile
    , two :: BuildPile
    , three :: BuildPile
    , four :: BuildPile
    , five :: BuildPile
    , six :: BuildPile
    , seven :: BuildPile
    }
    deriving (Show)

-- There are seven Build piles in the Tableau
-- Each pile is labeled 1 through 7
-- type Tableau = [(Int, BuildPile)]

-- The full game state
data Solitaire = Solitaire
    { stock :: Stock
    , waste :: Waste
    , foundations :: Foundations
    , tableau :: Tableau
    }

-- each string is a line in the visualiziation
toStrList :: Solitaire -> [String]
toStrList (Solitaire s [] f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: " ++ show (length s)
    line1 = show (head $ heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = show (head $ clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = show (head $ diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = show (head $ spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = "--||" ++ toStrBuildPile (two t)
    line7 = show (head s) ++ "||" ++ toStrBuildPile (one t)
toStrList (Solitaire [] w f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: 0"
    line1 = show (head $ heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = show (head $ clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = show (head $ diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = show (head $ spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = show (head w) ++ "||" ++ toStrBuildPile (two t)
    line7 = "--||" ++ toStrBuildPile (one t)
toStrList (Solitaire s w f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: " ++ show (length s)
    line1 = show (head $ heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = show (head $ clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = show (head $ diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = show (head $ spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = show (head w) ++ "||" ++ toStrBuildPile (two t)
    line7 = show (head s) ++ "||" ++ toStrBuildPile (one t)

render :: [String] -> IO ()
render = mapM_ putStrLn

toStrBuildPile :: BuildPile -> String
toStrBuildPile [] = "--"
toStrBuildPile bp = intercalate "|" (map show (reverse bp))

setupFoundations :: Foundations
setupFoundations = Foundations hp dp cp sp
  where
    hp = [Card EmptyRank Hearts]
    dp = [Card EmptyRank Diamonds]
    cp = [Card EmptyRank Clubs]
    sp = [Card EmptyRank Spades]

-- Deal cards into tableaus
-- output is the list of tableaus and stock
dealTableaus :: Deck -> Int -> Int -> [BuildPile] -> ([BuildPile], Stock)
dealTableaus deck _ 0 acc = (reverse acc, deck)
dealTableaus deck n count acc =
    dealTableaus rest (n + 1) (count - 1) (newPile : acc)
  where
    (dealt, rest) = splitAt n deck
    newPile = dealt

setupTableau :: [BuildPile] -> Tableau
setupTableau bps = Tableau p1 p2 p3 p4 p5 p6 p7
  where
    p1 : rest1 = bps
    p2 : rest2 = rest1
    p3 : rest3 = rest2
    p4 : rest4 = rest3
    p5 : rest5 = rest4
    p6 : rest6 = rest5
    p7 = head rest6

-- Setup Solitaire game given a deck in a certain state
-- The game starts with
-- - A stock with the left over cards
-- - an empty waste
-- - seven piles in the tableau
-- - four empty foundation piles
setupSolitaire :: Deck -> Solitaire
setupSolitaire shuffledDeck = Solitaire rest [] initFoundations initTableau
  where
    (tableaus, rest) = dealTableaus shuffledDeck 1 7 []
    initFoundations = setupFoundations
    initTableau = setupTableau tableaus

-- still a work in progress
instance Show Solitaire where
    show (Solitaire s [] f t) =
        intercalate "\n" [line0, line1, line2, line3, line4, line5, line6, line7]
      where
        line0 = "stock: " ++ show (length s)
        line1 = show (head $ heartsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ seven t)))
        line2 = show (head $ clubsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ six t)))
        line3 = show (head $ diamondsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ five t)))
        line4 = show (head $ spadesPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ four t)))
        line5 = "  ||" ++ show (intercalate "|" (map show (reverse $ three t)))
        line6 = "--||" ++ show (intercalate "|" (map show (reverse $ two t)))
        line7 = show (head s) ++ "||" ++ show (one t)
    show (Solitaire [] w f t) =
        intercalate "\n" [line0, line1, line2, line3, line4, line5, line6, line7]
      where
        line0 = "stock: 0"
        line1 = show (head $ heartsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ seven t)))
        line2 = show (head $ clubsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ six t)))
        line3 = show (head $ diamondsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ five t)))
        line4 = show (head $ spadesPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ four t)))
        line5 = "  ||" ++ show (intercalate "|" (map show (reverse $ three t)))
        line6 = show (head w) ++ "||" ++ show (intercalate "|" (map show (reverse $ two t)))
        line7 = "--||" ++ show (one t)
    show (Solitaire s w f t) =
        intercalate "\n" [line0, line1, line2, line3, line4, line5, line6, line7]
      where
        line0 = "stock: 0"
        line1 = show (head $ heartsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ seven t)))
        line2 = show (head $ clubsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ six t)))
        line3 = show (head $ diamondsPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ five t)))
        line4 = show (head $ spadesPile f) ++ "||" ++ show (intercalate "|" (map show (reverse $ four t)))
        line5 = "  ||" ++ show (intercalate "|" (map show (reverse $ three t)))
        line6 = show (head w) ++ "||" ++ show (intercalate "|" (map show (reverse $ two t)))
        line7 = show (head s) ++ "||" ++ show (one t)
