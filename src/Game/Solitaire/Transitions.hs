-- | functions to represent transitions in state
module Game.Solitaire.Transitions (
    NamedTransition(..),
    allTransitions,
    lookupTransition,
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
    wasteToHeartsFoundation,
    wasteToSpadesFoundation,
    wasteToDiamondsFoundation,
    wasteToClubsFoundation,
    tableauOneToHeartFoundation,
    tableauTwoToHeartFoundation,
    tableauThreeToHeartFoundation,
    tableauFourToHeartFoundation,
    tableauFiveToHeartFoundation,
    tableauSixToHeartFoundation,
    tableauSevenToHeartFoundation,
    tableauOneToSpadeFoundation,
    tableauTwoToSpadeFoundation,
    tableauThreeToSpadeFoundation,
    tableauFourToSpadeFoundation,
    tableauFiveToSpadeFoundation,
    tableauSixToSpadeFoundation,
    tableauSevenToSpadeFoundation,
    tableauOneToDiamondFoundation,
    tableauTwoToDiamondFoundation,
    tableauThreeToDiamondFoundation,
    tableauFourToDiamondFoundation,
    tableauFiveToDiamondFoundation,
    tableauSixToDiamondFoundation,
    tableauSevenToDiamondFoundation,
    tableauOneToClubFoundation,
    tableauTwoToClubFoundation,
    tableauThreeToClubFoundation,
    tableauFourToClubFoundation,
    tableauFiveToClubFoundation,
    tableauSixToClubFoundation,
    tableauSevenToClubFoundation,
    tableauToTableau,
) where

import qualified Data.Map.Strict as Map

import Game.Card
import Game.Solitaire.State

data NamedTransition c = NamedTransition {
    name :: String
    , apply :: Solitaire c -> Solitaire c
}

allTransitions :: (IsPlayable c, HasCard c, HasFace c, Eq c, Show c) => [NamedTransition c]
allTransitions =
    [ NamedTransition "refreshStock" refreshStock
    , NamedTransition "stockToWaste" stockToWaste
    , NamedTransition "tableauOneToHeartFoundation" tableauOneToHeartFoundation
    , NamedTransition "tableauTwoToHeartFoundation" tableauTwoToHeartFoundation
    , NamedTransition "tableauThreeToHeartFoundation" tableauThreeToHeartFoundation
    , NamedTransition "tableauFourToHeartFoundation" tableauFourToHeartFoundation
    , NamedTransition "tableauFiveToHeartFoundation" tableauFiveToHeartFoundation
    , NamedTransition "tableauSixToHeartFoundation" tableauSixToHeartFoundation
    , NamedTransition "tableauSevenToHeartFoundation" tableauSevenToHeartFoundation
    , NamedTransition "tableauOneToSpadeFoundation" tableauOneToSpadeFoundation
    , NamedTransition "tableauTwoToSpadeFoundation" tableauTwoToSpadeFoundation
    , NamedTransition "tableauThreeToSpadeFoundation" tableauThreeToSpadeFoundation
    , NamedTransition "tableauFourToSpadeFoundation" tableauFourToSpadeFoundation
    , NamedTransition "tableauFiveToSpadeFoundation" tableauFiveToSpadeFoundation
    , NamedTransition "tableauSixToSpadeFoundation" tableauSixToSpadeFoundation
    , NamedTransition "tableauSevenToSpadeFoundation" tableauSevenToSpadeFoundation
    , NamedTransition "tableauOneToDiamondFoundation" tableauOneToDiamondFoundation
    , NamedTransition "tableauTwoToDiamondFoundation" tableauTwoToDiamondFoundation
    , NamedTransition "tableauThreeToDiamondFoundation" tableauThreeToDiamondFoundation
    , NamedTransition "tableauFourToDiamondFoundation" tableauFourToDiamondFoundation
    , NamedTransition "tableauFiveToDiamondFoundation" tableauFiveToDiamondFoundation
    , NamedTransition "tableauSixToDiamondFoundation" tableauSixToDiamondFoundation
    , NamedTransition "tableauSevenToDiamondFoundation" tableauSevenToDiamondFoundation
    , NamedTransition "tableauOneToClubFoundation" tableauOneToClubFoundation
    , NamedTransition "tableauTwoToClubFoundation" tableauTwoToClubFoundation
    , NamedTransition "tableauThreeToClubFoundation" tableauThreeToClubFoundation
    , NamedTransition "tableauFourToClubFoundation" tableauFourToClubFoundation
    , NamedTransition "tableauFiveToClubFoundation" tableauFiveToClubFoundation
    , NamedTransition "tableauSixToClubFoundation" tableauSixToClubFoundation
    , NamedTransition "tableauSevenToClubFoundation" tableauSevenToClubFoundation
    , NamedTransition "wasteToTableauOne" wasteToTableauOne
    , NamedTransition "wasteToTableauTwo" wasteToTableauTwo
    , NamedTransition "wasteToTableauThree" wasteToTableauThree
    , NamedTransition "wasteToTableauFour" wasteToTableauFour
    , NamedTransition "wasteToTableauFive" wasteToTableauFive
    , NamedTransition "wasteToTableauSix" wasteToTableauSix
    , NamedTransition "wasteToTableauSeven" wasteToTableauSeven
    , NamedTransition "wasteToHeartsFoundation" wasteToHeartsFoundation
    , NamedTransition "wasteToSpadesFoundation" wasteToSpadesFoundation
    , NamedTransition "wasteToDiamondsFoundation" wasteToDiamondsFoundation
    , NamedTransition "wasteToClubsFoundation" wasteToClubsFoundation
    ]
    ++ [ NamedTransition ("tableau" ++ show i ++ "To" ++ show j ++ "_" ++ show n)
         (tableauToTableau i j n)
       | i <- [1 .. 7], j <- [1 .. 7], i /= j, n <- [1 .. 13]
       ]

transitionMap :: (Show c, Eq c, HasCard c, IsPlayable c, HasFace c) => Map.Map String (Solitaire c -> Solitaire c)
transitionMap = Map.fromList [(name nt, apply nt) | nt <- allTransitions]

lookupTransition :: (Show c, Eq c, HasCard c, IsPlayable c, HasFace c) => String -> Maybe (Solitaire c -> Solitaire c)
lookupTransition = (`Map.lookup` transitionMap)

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

canBuild :: (HasCard a, HasCard b, IsPlayable a, IsPlayable b) => a -> b -> Bool
canBuild a b
    | isPlayable a = canBuildCard (toCard a) (toCard b)
    | otherwise = False

-- Transition state functions

{- | Move the top card in the stock to the waste

When the stock is empty this moves the waste cards
back into the stock.
-}
stockToWaste :: (HasFace c, Eq c, Show c) => Solitaire c -> Solitaire c
stockToWaste s
    | stock s /= [] = stockToWaste' s
    | otherwise = s

{- | Helper transition function.
  Moves the top stock card to the top of the waste
  cannot be used when stock is empty
-}
stockToWaste' :: (HasFace c, Eq c, Show c) => Solitaire c -> Solitaire c
stockToWaste' (Solitaire [a] w' f' t') = Solitaire [] (flipCard a : w') f' t'
stockToWaste' s = newSolitaire
  where
    c : cs = stock s
    newStock = cs
    newWaste = flipCard c : waste s
    newSolitaire = s{stock = newStock, waste = newWaste}

{- | Helper transition function.
  Moves the cards in the waste to the stock and emtpies waste
-}
refreshStock :: (Eq c, Show c) => Solitaire c -> Solitaire c
refreshStock s
    | null $ stock s = s{stock = reverse $ waste s, waste = []}
    | otherwise = s

wasteToTableauOne :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauOne s
    | null $ waste s = s
    | null firstTableau = s
    | canBuild (head $ waste s) (head firstTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{one = (head $ waste s) : firstTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    firstTableau = one initTableau

wasteToTableauTwo :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauTwo s
    | null $ waste s = s
    | null secondTableau = s
    | canBuild (head $ waste s) (head secondTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{two = (head $ waste s) : secondTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    secondTableau = two initTableau

wasteToTableauThree :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauThree s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{three = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = three initTableau

wasteToTableauFour :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauFour s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{four = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = four initTableau

wasteToTableauFive :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauFive s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{five = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = five initTableau

wasteToTableauSix :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
wasteToTableauSix s
    | null $ waste s = s
    | null newTableau = s
    | canBuild (head $ waste s) (head newTableau) = s{waste = wasteMinusOne (waste s), tableau = initTableau{six = (head $ waste s) : newTableau}}
    | otherwise = s
  where
    initTableau = tableau s
    newTableau = six initTableau

wasteToTableauSeven :: (Eq c, HasCard c, Show c, IsPlayable c) => Solitaire c -> Solitaire c
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

wasteToHeartsFoundation :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToHeartsFoundation s
    | null $ waste s = s
    | not $ null heartFoundation = s -- when there is someting on the pile we can't build with an Ace
    | canBuildToEmptyHeartFoundation wasteCard = s{foundations = newFoundations}
    | otherwise = s
  where
    heartFoundation = heartsPile $ foundations s
    wasteCard = head $ waste s
    oldFoundations = foundations s
    newFoundations = oldFoundations{heartsPile = [wasteCard]}

wasteToSpadesFoundation :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToSpadesFoundation s
    | null $ waste s = s
    | not $ null spadeFoundation = s -- when there is someting on the pile we can't build with an Ace
    | canBuildToEmptySpadeFoundation wasteCard = s{foundations = newFoundations}
    | otherwise = s
  where
    spadeFoundation = spadesPile $ foundations s
    wasteCard = head $ waste s
    oldFoundations = foundations s
    newFoundations = oldFoundations{spadesPile = [wasteCard]}

wasteToDiamondsFoundation :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToDiamondsFoundation s
    | null $ waste s = s
    | not $ null diamondFoundation = s -- when there is someting on the pile we can't build with an Ace
    | canBuildToEmptyDiamondFoundation wasteCard = s{foundations = newFoundations}
    | otherwise = s
  where
    diamondFoundation = diamondsPile $ foundations s
    wasteCard = head $ waste s
    oldFoundations = foundations s
    newFoundations = oldFoundations{diamondsPile = [wasteCard]}

wasteToClubsFoundation :: (Eq c, HasCard c, Show c) => Solitaire c -> Solitaire c
wasteToClubsFoundation s
    | null $ waste s = s
    | not $ null clubFoundation = s -- when there is someting on the pile we can't build with an Ace
    | canBuildToEmptyClubFoundation wasteCard = s{foundations = newFoundations}
    | otherwise = s
  where
    clubFoundation = clubsPile $ foundations s
    wasteCard = head $ waste s
    oldFoundations = foundations s
    newFoundations = oldFoundations{clubsPile = [wasteCard]}

-- Helper to allow placing an Ace on empty foundation
canBuildToEmptyHeartFoundation :: (HasCard c) => c -> Bool
canBuildToEmptyHeartFoundation c = toCard c == Card Ace Hearts

canBuildToEmptySpadeFoundation :: (HasCard c) => c -> Bool
canBuildToEmptySpadeFoundation c = toCard c == Card Ace Spades

canBuildToEmptyDiamondFoundation :: (HasCard c) => c -> Bool
canBuildToEmptyDiamondFoundation c = toCard c == Card Ace Diamonds

canBuildToEmptyClubFoundation :: (HasCard c) => c -> Bool
canBuildToEmptyClubFoundation c = toCard c == Card Ace Clubs

tableauOneToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauOneToHeartFoundation s
    | null (one $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ one $ tableau s
    newTableau = flipTop $ tail $ one $ tableau s

tableauTwoToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauTwoToHeartFoundation s
    | null (two $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ two $ tableau s
    newTableau = flipTop $ tail $ two $ tableau s

tableauThreeToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauThreeToHeartFoundation s
    | null (three $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ three $ tableau s
    newTableau = flipTop $ tail $ three $ tableau s

tableauFourToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFourToHeartFoundation s
    | null (four $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ four $ tableau s
    newTableau = flipTop $ tail $ four $ tableau s

tableauFiveToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFiveToHeartFoundation s
    | null (five $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ five $ tableau s
    newTableau = flipTop $ tail $ five $ tableau s

tableauSixToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSixToHeartFoundation s
    | null (six $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ six $ tableau s
    newTableau = flipTop $ tail $ six $ tableau s

tableauSevenToHeartFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSevenToHeartFoundation s
    | null (seven $ tableau s) = s
    | otherwise =
        case heartsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyHeartFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){heartsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){heartsPile = tableauCard : heartsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ seven $ tableau s
    newTableau = flipTop $ tail $ seven $ tableau s

tableauOneToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauOneToSpadeFoundation s
    | null (one $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ one $ tableau s
    newTableau = flipTop $ tail $ one $ tableau s

tableauTwoToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauTwoToSpadeFoundation s
    | null (two $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ two $ tableau s
    newTableau = flipTop $ tail $ two $ tableau s

tableauThreeToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauThreeToSpadeFoundation s
    | null (three $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ three $ tableau s
    newTableau = flipTop $ tail $ three $ tableau s

tableauFourToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFourToSpadeFoundation s
    | null (four $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ four $ tableau s
    newTableau = flipTop $ tail $ four $ tableau s

tableauFiveToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFiveToSpadeFoundation s
    | null (five $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ five $ tableau s
    newTableau = flipTop $ tail $ five $ tableau s

tableauSixToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSixToSpadeFoundation s
    | null (six $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ six $ tableau s
    newTableau = flipTop $ tail $ six $ tableau s

tableauSevenToSpadeFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSevenToSpadeFoundation s
    | null (seven $ tableau s) = s
    | otherwise =
        case spadesPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptySpadeFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){spadesPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){spadesPile = tableauCard : spadesPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ seven $ tableau s
    newTableau = flipTop $ tail $ seven $ tableau s

tableauOneToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauOneToDiamondFoundation s
    | null (one $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ one $ tableau s
    newTableau = flipTop $ tail $ one $ tableau s

tableauTwoToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauTwoToDiamondFoundation s
    | null (two $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ two $ tableau s
    newTableau = flipTop $ tail $ two $ tableau s

tableauThreeToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauThreeToDiamondFoundation s
    | null (three $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ three $ tableau s
    newTableau = flipTop $ tail $ three $ tableau s

tableauFourToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFourToDiamondFoundation s
    | null (four $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ four $ tableau s
    newTableau = flipTop $ tail $ four $ tableau s

tableauFiveToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFiveToDiamondFoundation s
    | null (five $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ five $ tableau s
    newTableau = flipTop $ tail $ five $ tableau s

tableauSixToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSixToDiamondFoundation s
    | null (six $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ six $ tableau s
    newTableau = flipTop $ tail $ six $ tableau s

tableauSevenToDiamondFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSevenToDiamondFoundation s
    | null (seven $ tableau s) = s
    | otherwise =
        case diamondsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyDiamondFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){diamondsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){diamondsPile = tableauCard : diamondsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ seven $ tableau s
    newTableau = flipTop $ tail $ seven $ tableau s

tableauOneToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauOneToClubFoundation s
    | null (one $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{one = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ one $ tableau s
    newTableau = flipTop $ tail $ one $ tableau s

tableauTwoToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauTwoToClubFoundation s
    | null (two $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{two = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ two $ tableau s
    newTableau = flipTop $ tail $ two $ tableau s

tableauThreeToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauThreeToClubFoundation s
    | null (three $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{three = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ three $ tableau s
    newTableau = flipTop $ tail $ three $ tableau s

tableauFourToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFourToClubFoundation s
    | null (four $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{four = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ four $ tableau s
    newTableau = flipTop $ tail $ four $ tableau s

tableauFiveToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauFiveToClubFoundation s
    | null (five $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{five = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ five $ tableau s
    newTableau = flipTop $ tail $ five $ tableau s

tableauSixToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSixToClubFoundation s
    | null (six $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{six = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ six $ tableau s
    newTableau = flipTop $ tail $ six $ tableau s

tableauSevenToClubFoundation :: (Eq c, Show c, HasCard c, HasFace c, IsPlayable c) => Solitaire c -> Solitaire c
tableauSevenToClubFoundation s
    | null (seven $ tableau s) = s
    | otherwise =
        case clubsPile (foundations s) of
            [] ->
                -- if the foundation is empty, allow Ace
                if canBuildToEmptyClubFoundation tableauCard
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){clubsPile = [tableauCard]}
                            }
                    else s
            (f : _) ->
                if canBuild tableauCard f
                    then
                        s
                            { tableau = initTableau{seven = newTableau}
                            , foundations = (foundations s){clubsPile = tableauCard : clubsPile (foundations s)}
                            }
                    else s
  where
    initTableau = tableau s
    tableauCard = head $ seven $ tableau s
    newTableau = flipTop $ tail $ seven $ tableau s

-- ---------------------------
-- Tableau to Tableau
-- Here each tableau can contain at most 12 cards
-- The first Int is the tableau to move a card from, i.e. 1--7
-- The second Int is the tableau to move to, i.e. 1--7 that is different than this one
-- The third Int is the number of cards to move, i.e. 1--12
-- There are 504 such functions
-- ---------------------------

tableauToTableau :: (Eq c, HasCard c, HasFace c, Show c, IsPlayable c) => Int -> Int -> Int -> Solitaire c -> Solitaire c
tableauToTableau fromIdx toIdx numCards (Solitaire s' w f t)
    | tableauIsEmpty fromIdx t = Solitaire s' w f t
    | length (faceUpCards fromBuildPile) < numCards = Solitaire s' w f t
    | tableauIsEmpty toIdx t && not (isKing fromCard) = Solitaire s' w f t
    | tableauIsEmpty toIdx t && isKing fromCard = Solitaire s' w f updatedTableau
    | canBuild fromCard toCard' = Solitaire s' w f updatedTableau
    | otherwise = Solitaire s' w f t
  where
    fromBuildPile = getBuildPile fromIdx t
    toBuildPile = getBuildPile toIdx t
    (cardsToMove, newFromBuildPile) = splitAt numCards fromBuildPile
    newFromBuildPile' = flipTop newFromBuildPile
    fromCard = last cardsToMove
    toCard' = head toBuildPile -- use ' to avoid clash with toCard from HasCard
    updatedToPile = cardsToMove ++ toBuildPile
    updatedTableau = updateTableau fromIdx newFromBuildPile' toIdx updatedToPile t

tableauIsEmpty :: Int -> Tableau c -> Bool
tableauIsEmpty fromIdx t
    | null (getBuildPile fromIdx t) = True
    | otherwise = False

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

allTableauToTableauFunctions :: (HasFace c, Show c, Eq c, HasCard c, IsPlayable c) => [Solitaire c -> Solitaire c]
allTableauToTableauFunctions = [tableauToTableau i j n | i <- [1 .. 7], j <- [1 .. 7], i /= j, n <- [1 .. 13]]

isKing :: (HasCard c) => c -> Bool
isKing c = rank (toCard c) == King

faceUpCards :: (HasFace c) => [c] -> [c]
faceUpCards = takeWhile isFaceUp

isFaceUp :: (HasFace c) => c -> Bool
isFaceUp = (== Up) . toFace
