{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Solitaire.State (
    Solitaire (..),
    Foundations (..),
    Tableau (..),
    BuildPile,
    SuitPile,
    Waste,
    Stock,
    toStrList,
    render,
    flipTop,
) where

import Data.Binary (Binary (..))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Game.Card

-- Synonyms for different list of Cards
-- head of the list is the top of the stack of cards
type Waste c = [c]

type SuitPile c = [c]

-- Regular piles can only grow with alternating color
type BuildPile c = [c]

type Stock c = [c]

-- There are four foundations, one for each suit
data Foundations c = Foundations
    { heartsPile :: SuitPile c
    , diamondsPile :: SuitPile c
    , clubsPile :: SuitPile c
    , spadesPile :: SuitPile c
    }
    deriving (Eq, Show, Generic)

instance (Binary c) => Binary (Foundations c)

data Tableau c = Tableau
    { one :: BuildPile c
    , two :: BuildPile c
    , three :: BuildPile c
    , four :: BuildPile c
    , five :: BuildPile c
    , six :: BuildPile c
    , seven :: BuildPile c
    }
    deriving (Eq, Show, Generic)

instance (Binary c) => Binary (Tableau c)

-- There are seven Build piles in the Tableau
-- Each pile is labeled 1 through 7
-- type Tableau = [(Int, BuildPile)]

-- The full game state
data Solitaire c = Solitaire
    { stock :: Stock c
    , waste :: Waste c
    , foundations :: Foundations c
    , tableau :: Tableau c
    }
    deriving (Eq, Generic)

instance (Binary c) => Binary (Solitaire c)

instance (Eq c, Show c) => Show (Solitaire c) where
    show s = intercalate "\n" (toStrList s)

-- each string is a line in the visualiziation
toStrList :: (Eq c, Show c) => Solitaire c -> [String]
toStrList (Solitaire s [] f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: " ++ show (length s)
    line1 = toStrFoundationPile Hearts (heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = toStrFoundationPile Clubs (clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = toStrFoundationPile Diamonds (diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = toStrFoundationPile Spades (spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = "--||" ++ toStrBuildPile (two t)
    line7 = show (head s) ++ "||" ++ toStrBuildPile (one t)
toStrList (Solitaire [] w f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: 0"
    line1 = toStrFoundationPile Hearts (heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = toStrFoundationPile Clubs (clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = toStrFoundationPile Diamonds (diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = toStrFoundationPile Spades (spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = show (head w) ++ "||" ++ toStrBuildPile (two t)
    line7 = "--||" ++ toStrBuildPile (one t)
toStrList (Solitaire s w f t) = [line0, line1, line2, line3, line4, line5, line6, line7]
  where
    line0 = "stock: " ++ show (length s)
    line1 = toStrFoundationPile Hearts (heartsPile f) ++ "||" ++ toStrBuildPile (seven t)
    line2 = toStrFoundationPile Clubs (clubsPile f) ++ "||" ++ toStrBuildPile (six t)
    line3 = toStrFoundationPile Diamonds (diamondsPile f) ++ "||" ++ toStrBuildPile (five t)
    line4 = toStrFoundationPile Spades (spadesPile f) ++ "||" ++ toStrBuildPile (four t)
    line5 = "  ||" ++ toStrBuildPile (three t)
    line6 = show (head w) ++ "||" ++ toStrBuildPile (two t)
    line7 = show (head s) ++ "||" ++ toStrBuildPile (one t)

render :: [String] -> IO ()
render = mapM_ putStrLn

toStrBuildPile :: (Show c) => BuildPile c -> String
toStrBuildPile [] = "--"
toStrBuildPile bp = intercalate "|" (map show (reverse bp))

toStrFoundationPile :: (Show c) => Suit -> SuitPile c -> String
toStrFoundationPile s [] = case s of
    Hearts -> "-H"
    Diamonds -> "-D"
    Clubs -> "-C"
    Spades -> "-S"
toStrFoundationPile _ cs = show $ head cs

flipTop :: (HasFace c) => BuildPile c -> BuildPile c
flipTop [] = []
flipTop (x : xs) = flipCard x : xs
