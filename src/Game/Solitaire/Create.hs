module Game.Solitaire.Create (
    setupSolitaire,
) where

import Game.Card
import Game.Deck
import Game.Solitaire.State

setupFoundations :: Foundations c
setupFoundations = Foundations hp dp cp sp
  where
    hp = []
    dp = []
    cp = []
    sp = []

-- Deal cards into tableaus
-- output is the list of tableaus and stock
dealTableaus :: (HasFace c) => Deck c -> Int -> Int -> [BuildPile c] -> ([BuildPile c], Stock c)
dealTableaus deck _ 0 acc = (reverse acc, deck)
dealTableaus deck n count acc =
    dealTableaus rest (n + 1) (count - 1) (newPile : acc)
  where
    (dealt, rest) = splitAt n deck
    newPile = flipTop dealt

setupTableau :: [BuildPile c] -> Tableau c
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
setupSolitaire :: (Eq c, HasFace c, Show c) => Deck c -> Solitaire c
setupSolitaire shuffledDeck = Solitaire s [] initFoundations initTableau
  where
    (tableaus, rest) = dealTableaus shuffledDeck 1 7 []
    s = flipTop rest
    initFoundations = setupFoundations
    initTableau = setupTableau tableaus
