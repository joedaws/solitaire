-- | functions to represent transitions in state
module Game.Solitaire.Transitions (
    canBuild,
    stockToWaste,
    refreshStock,
    wasteToTableauOne,
    wasteToTableauTwo,
    wasteToTableauThree,
    wasteToTableauFour,
    wasteToTableauFive,
    wasteToTableauSix,
    wasteToTableauSeven,
    wasteMinusOne,
    tableauToTableau,
) where

import Game.Card
import Game.Deck
import Game.Solitaire.State

canBuildCard :: Card -> Card -> Bool
canBuildCard (Card Ace Hearts) (Card Two Clubs) = True
canBuildCard (Card Ace Hearts) (Card Two Spades) = True
canBuildCard (Card Ace Diamonds) (Card Two Clubs) = True
canBuildCard (Card Ace Diamonds) (Card Two Spades) = True
canBuildCard (Card Ace Clubs) (Card Two Diamonds) = True
canBuildCard (Card Ace Clubs) (Card Two Hearts) = True
canBuildCard (Card Ace Spades) (Card Two Diamonds) = True
canBuildCard (Card Ace Spades) (Card Two Hearts) = True
canBuildCard (Card Two Hearts) (Card Three Clubs) = True
canBuildCard (Card Two Hearts) (Card Three Spades) = True
canBuildCard (Card Two Diamonds) (Card Three Clubs) = True
canBuildCard (Card Two Diamonds) (Card Three Spades) = True
canBuildCard (Card Two Clubs) (Card Three Diamonds) = True
canBuildCard (Card Two Clubs) (Card Three Hearts) = True
canBuildCard (Card Two Spades) (Card Three Diamonds) = True
canBuildCard (Card Two Spades) (Card Three Hearts) = True
canBuildCard (Card Three Hearts) (Card Four Clubs) = True
canBuildCard (Card Three Hearts) (Card Four Spades) = True
canBuildCard (Card Three Diamonds) (Card Four Clubs) = True
canBuildCard (Card Three Diamonds) (Card Four Spades) = True
canBuildCard (Card Three Clubs) (Card Four Diamonds) = True
canBuildCard (Card Three Clubs) (Card Four Hearts) = True
canBuildCard (Card Three Spades) (Card Four Diamonds) = True
canBuildCard (Card Three Spades) (Card Four Hearts) = True
canBuildCard (Card Four Hearts) (Card Five Clubs) = True
canBuildCard (Card Four Hearts) (Card Five Spades) = True
canBuildCard (Card Four Diamonds) (Card Five Clubs) = True
canBuildCard (Card Four Diamonds) (Card Five Spades) = True
canBuildCard (Card Four Clubs) (Card Five Diamonds) = True
canBuildCard (Card Four Clubs) (Card Five Hearts) = True
canBuildCard (Card Four Spades) (Card Five Diamonds) = True
canBuildCard (Card Four Spades) (Card Five Hearts) = True
canBuildCard (Card Five Hearts) (Card Six Clubs) = True
canBuildCard (Card Five Hearts) (Card Six Spades) = True
canBuildCard (Card Five Diamonds) (Card Six Clubs) = True
canBuildCard (Card Five Diamonds) (Card Six Spades) = True
canBuildCard (Card Five Clubs) (Card Six Diamonds) = True
canBuildCard (Card Five Clubs) (Card Six Hearts) = True
canBuildCard (Card Five Spades) (Card Six Diamonds) = True
canBuildCard (Card Five Spades) (Card Six Hearts) = True
canBuildCard (Card Six Hearts) (Card Seven Clubs) = True
canBuildCard (Card Six Hearts) (Card Seven Spades) = True
canBuildCard (Card Six Diamonds) (Card Seven Clubs) = True
canBuildCard (Card Six Diamonds) (Card Seven Spades) = True
canBuildCard (Card Six Clubs) (Card Seven Diamonds) = True
canBuildCard (Card Six Clubs) (Card Seven Hearts) = True
canBuildCard (Card Six Spades) (Card Seven Diamonds) = True
canBuildCard (Card Six Spades) (Card Seven Hearts) = True
canBuildCard (Card Seven Hearts) (Card Eight Clubs) = True
canBuildCard (Card Seven Hearts) (Card Eight Spades) = True
canBuildCard (Card Seven Diamonds) (Card Eight Clubs) = True
canBuildCard (Card Seven Diamonds) (Card Eight Spades) = True
canBuildCard (Card Seven Clubs) (Card Eight Diamonds) = True
canBuildCard (Card Seven Clubs) (Card Eight Hearts) = True
canBuildCard (Card Seven Spades) (Card Eight Diamonds) = True
canBuildCard (Card Seven Spades) (Card Eight Hearts) = True
canBuildCard (Card Eight Hearts) (Card Nine Clubs) = True
canBuildCard (Card Eight Hearts) (Card Nine Spades) = True
canBuildCard (Card Eight Diamonds) (Card Nine Clubs) = True
canBuildCard (Card Eight Diamonds) (Card Nine Spades) = True
canBuildCard (Card Eight Clubs) (Card Nine Diamonds) = True
canBuildCard (Card Eight Clubs) (Card Nine Hearts) = True
canBuildCard (Card Eight Spades) (Card Nine Diamonds) = True
canBuildCard (Card Eight Spades) (Card Nine Hearts) = True
canBuildCard (Card Nine Hearts) (Card Ten Clubs) = True
canBuildCard (Card Nine Hearts) (Card Ten Spades) = True
canBuildCard (Card Nine Diamonds) (Card Ten Clubs) = True
canBuildCard (Card Nine Diamonds) (Card Ten Spades) = True
canBuildCard (Card Nine Clubs) (Card Ten Diamonds) = True
canBuildCard (Card Nine Clubs) (Card Ten Hearts) = True
canBuildCard (Card Nine Spades) (Card Ten Diamonds) = True
canBuildCard (Card Nine Spades) (Card Ten Hearts) = True
canBuildCard (Card Ten Hearts) (Card Jack Clubs) = True
canBuildCard (Card Ten Hearts) (Card Jack Spades) = True
canBuildCard (Card Ten Diamonds) (Card Jack Clubs) = True
canBuildCard (Card Ten Diamonds) (Card Jack Spades) = True
canBuildCard (Card Ten Clubs) (Card Jack Diamonds) = True
canBuildCard (Card Ten Clubs) (Card Jack Hearts) = True
canBuildCard (Card Ten Spades) (Card Jack Diamonds) = True
canBuildCard (Card Ten Spades) (Card Jack Hearts) = True
canBuildCard (Card Jack Hearts) (Card Queen Clubs) = True
canBuildCard (Card Jack Hearts) (Card Queen Spades) = True
canBuildCard (Card Jack Diamonds) (Card Queen Clubs) = True
canBuildCard (Card Jack Diamonds) (Card Queen Spades) = True
canBuildCard (Card Jack Clubs) (Card Queen Diamonds) = True
canBuildCard (Card Jack Clubs) (Card Queen Hearts) = True
canBuildCard (Card Jack Spades) (Card Queen Diamonds) = True
canBuildCard (Card Jack Spades) (Card Queen Hearts) = True
canBuildCard (Card Queen Hearts) (Card King Clubs) = True
canBuildCard (Card Queen Hearts) (Card King Spades) = True
canBuildCard (Card Queen Diamonds) (Card King Clubs) = True
canBuildCard (Card Queen Diamonds) (Card King Spades) = True
canBuildCard (Card Queen Clubs) (Card King Diamonds) = True
canBuildCard (Card Queen Clubs) (Card King Hearts) = True
canBuildCard (Card Queen Spades) (Card King Diamonds) = True
canBuildCard (Card Queen Spades) (Card King Hearts) = True
canBuildCard _ _ = False

canBuild :: (HasCard a, HasCard b) => a -> b -> Bool
canBuild a b = canBuildCard (toCard a) (toCard b)

-- Transition state functions

{- | Move the top card in the stock to the waste

When the stock is empty this moves the waste cards
back into the stock.
-}
stockToWaste :: (Eq c, Show c) => Solitaire c -> Solitaire c
stockToWaste s
    | stock s /= [] = stockToWaste' s
    | otherwise = s

{- | Helper transition function.
  Moves the top stock card to the top of the waste
  Warning cannot be used with stock is empty
-}
stockToWaste' :: (Eq c, Show c) => Solitaire c -> Solitaire c
stockToWaste' s = newSolitaire
  where
    c : cs = stock s
    newStock = cs
    newWaste = c : waste s
    newSolitaire = s{stock = newStock, waste = newWaste}

{- | Helper transition function.
  Moves the cards in the waste to the stock and emtpies waste
-}
refreshStock :: (Eq c, Show c) => Solitaire c -> Solitaire c
refreshStock s
    | null $ stock s = s{stock = reverse $ waste s, waste = []}
    | otherwise = s

wasteToTableauOne :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauOne s
    | null $ waste s = s
    | null firstTableau = s
    | canBuild (head $ waste s) (head firstTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{one = (head $ waste s) : firstTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    firstTableau = one initTableau

wasteToTableauTwo :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauTwo s
    | null $ waste s = s
    | null secondTableau = s
    | canBuild (head $ waste s) (head secondTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{two = (head $ waste s) : secondTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    secondTableau = two initTableau

wasteToTableauThree :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauThree s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{three = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = three initTableau

wasteToTableauFour :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauFour s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{four = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = four initTableau

wasteToTableauFive :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauFive s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{five = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = five initTableau

wasteToTableauSix :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauSix s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{six = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = six initTableau

wasteToTableauSeven :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToTableauSeven s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{seven = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = seven initTableau

wasteMinusOne :: Waste c -> Waste c
wasteMinusOne (_ : ws) = ws

tableauToTableau :: (Eq c, HasCard c, Show c) => Int -> Int -> Int -> Solitaire c -> Solitaire c
tableauToTableau fromIdx toIdx numCards s
    | canBuild fromCard toCard = s{tableau = updatedTableau}
    | otherwise = s
  where
    t = tableau s
    fromBuildPile = getBuildPile fromIdx t
    toBuildPile = getBuildPile toIdx t
    (cardsToMove, newFromBuildPile) = splitAt numCards fromBuildPile
    fromCard = last cardsToMove
    toCard = head toBuildPile
    updatedToPile = cardsToMove ++ toBuildPile
    updatedTableau = updateTableau fromIdx newFromBuildPile toIdx updatedToPile t

getBuildPile :: Int -> Tableau c -> BuildPile c
getBuildPile idx tableau' = case idx of
    1 -> one tableau'
    2 -> two tableau'
    3 -> three tableau'
    4 -> four tableau'
    5 -> five tableau'
    6 -> six tableau'
    7 -> seven tableau'
    _ -> error "Invalid tableau index"

updateTableau :: Int -> BuildPile c -> Int -> BuildPile c -> Tableau c -> Tableau c
updateTableau fromIdx newFromBuildPile toIdx newToBuildPile t =
    t
        { one = if fromIdx == 1 then newFromBuildPile else if toIdx == 1 then newToBuildPile else one t
        , two = if fromIdx == 2 then newFromBuildPile else if toIdx == 2 then newToBuildPile else two t
        , three = if fromIdx == 3 then newFromBuildPile else if toIdx == 3 then newToBuildPile else three t
        , four = if fromIdx == 4 then newFromBuildPile else if toIdx == 4 then newToBuildPile else four t
        , five = if fromIdx == 5 then newFromBuildPile else if toIdx == 5 then newToBuildPile else five t
        , six = if fromIdx == 6 then newFromBuildPile else if toIdx == 6 then newToBuildPile else six t
        , seven = if fromIdx == 7 then newFromBuildPile else if toIdx == 7 then newToBuildPile else seven t
        }
