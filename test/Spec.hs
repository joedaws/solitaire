import Game.Card
import Game.Deck
import Game.Solitaire.Create
import Game.Solitaire.State
import Game.Solitaire.Transitions
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Game setup" $ do
        it "setupSolitaire" $ do
            let unshuffledDeck = createDeck mkKlondikeCardUp
            let s = setupSolitaire unshuffledDeck
            let initFoundations = foundations s
            let initStock = stock s
            length initStock `shouldBe` 24
            (toCard $ head initStock) `shouldBe` Card Eight Hearts
            heartsPile initFoundations `shouldBe` []
            waste s `shouldBe` []
            let oneTableau = map toCard (one $ tableau s)
            oneTableau `shouldBe` [Card Ace Hearts]

    describe "Solitaire State Transitions -- stockToWaste" $ do
        it "when stock has no cards" $ do
            let c1 = KlondikeCard (Card Five Spades) Down
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] []) -- all foundations empty
                        (Tableau [] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = stockToWaste s
            s' `shouldBe` s -- no state change expected
        it "when stock has one card" $ do
            let s =
                    Solitaire
                        [KlondikeCard (Card Five Spades) Down] -- stock
                        [] -- waste
                        (Foundations [] [] [] []) -- all foundations empty
                        (Tableau [] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = stockToWaste s
            stock s' `shouldBe` []
            waste s' `shouldBe` [KlondikeCard (Card Five Spades) Up] -- the state should change
        it "when stock has multiple cards" $ do
            let c1 = KlondikeCard (Card Five Spades) Down
            let c2 = KlondikeCard (Card Ace Hearts) Down
            let c1' = flipCard c1
            let s =
                    Solitaire
                        [c1, c2] -- stock
                        [] -- waste
                        (Foundations [] [] [] []) -- all foundations empty
                        (Tableau [] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = stockToWaste s
            stock s' `shouldBe` [c2] -- now that c2 is top of stack it must be up
            waste s' `shouldBe` [c1'] -- the card was already up while on top of stock
    describe "Solitaire State Transitions -- tablueOneToHeartFoundation" $ do
        it "when tableau one has a card that can move to heart foundation" $ do
            let tableauCard = KlondikeCard (Card Ace Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [] [] [] []) -- all foundations empty
                        (Tableau [tableauCard] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = tableauOneToHeartFoundation s
            heartsPile (foundations s') `shouldBe` [tableauCard]
            one (tableau s') `shouldBe` []

        it "does not move card if it can't build on foundation" $ do
            let tableauCard = KlondikeCard (Card Three Hearts) Up
            let foundationCard = [KlondikeCard (Card Ace Hearts) Up] -- already an ace there
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations foundationCard [] [] []) -- heart foundation has an Ace
                        (Tableau [tableauCard] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = tableauOneToHeartFoundation s
            heartsPile (foundations s') `shouldBe` foundationCard
            one (tableau s') `shouldBe` [tableauCard]

        it "does nothing when tableau one is empty" $ do
            let s =
                    Solitaire
                        [KlondikeCard (Card Three Hearts) Down]
                        []
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let s' = tableauOneToHeartFoundation s
            s' `shouldBe` s
    describe "Solitaire State Transitions -- tableauToTableau" $ do
        it "does nothing when fromTabluea is empty" $ do
            let c1 = KlondikeCard (Card Five Spades) Down
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c1] [] [] [c1] [] [] [])
            let s' = tableauToTableau 2 3 1 s -- move one card from t2 to t3 
            s' `shouldBe` s -- no state change expected
        it "does nothing when more cards are moved than in tableau" $ do
            let c1 = KlondikeCard (Card Ten Diamonds) Down
            let c2 = KlondikeCard (Card King Spades) Down
            let c3 = KlondikeCard (Card Queen Hearts) Down
            let c4 = KlondikeCard (Card Jack Clubs) Down
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c4, c3, c2] [] [] [c1] [] [] [])
            let s' = tableauToTableau 4 1 2 s
            s' `shouldBe` s -- no state change expected
        it "moves one card when valid" $ do
            let c1 = KlondikeCard (Card Five Spades) Up
            let c2 = KlondikeCard (Card Four Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c2] [] [] [c1] [] [] [])
            let s' = tableauToTableau 1 4 1 s -- move one card from t1 to t4
            four (tableau s') `shouldBe` [c2, c1] -- c2 should be on top of c1
        it "moves two cards when valid" $ do
            let c1 = KlondikeCard (Card Five Spades) Up
            let c2 = KlondikeCard (Card Four Hearts) Up
            let c3 = KlondikeCard (Card Six Diamonds) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [c2, c1] [] [] [c3])
            let s' = tableauToTableau 4 7 2 s -- move two cards from t4 to t7
            seven (tableau s') `shouldBe` [c2, c1, c3]
        it "fails to move when more than number of Up cards" $ do
            let c1 = KlondikeCard (Card Five Spades) Up
            let c2 = KlondikeCard (Card Four Hearts) Up
            let c3 = KlondikeCard (Card Six Diamonds) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [c2, c1] [] [] [c3])
            let s' = tableauToTableau 4 7 8 s 
            s' `shouldBe` s
        it "does not move non-King card to empty tableau" $ do
            let c1 = KlondikeCard (Card Queen Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c1] [] [] [] [] [] []) -- t1 has Queen, t2 empty
            let s' = tableauToTableau 1 2 1 s
            s' `shouldBe` s -- move is not allowed
        it "allows King to move to empty tableau" $ do
            let c1 = KlondikeCard (Card King Spades) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c1] [] [] [] [] [] []) -- t1 has King, t2 empty
            let s' = tableauToTableau 1 2 1 s
            one (tableau s') `shouldBe` []         -- from pile is now empty
            two (tableau s') `shouldBe` [c1]       -- King moved to empty t2
    describe "Solitaire State Transitions -- wasteToFoundation" $ do
        it "does nothing when waste is empty" $ do
            let c1 = KlondikeCard (Card Five Spades) Down
            let s =
                    Solitaire
                        [c1] -- stock
                        [] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [c1] [] [] [c1] [] [] [])
            let s' = wasteToHeartsFoundation s
            s' `shouldBe` s -- no state change expected
        it "does nothing when foundation has card" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Down
            let c2 = KlondikeCard (Card Ace Spades) Down
            let s =
                    Solitaire
                        [] -- stock
                        [c2] -- waste
                        (Foundations [c1] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let s' = wasteToHeartsFoundation s
            s' `shouldBe` s -- no state change expected
        it "moves Ace when Foundation is empty" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Down
            let s =
                    Solitaire
                        [] -- stock
                        [c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let s' = wasteToHeartsFoundation s
            head (heartsPile $ foundations s') `shouldBe` c1
    describe "Solitaire State Transitions -- TableauNToHeartFoundation" $ do
        it "does nothing when tableau is empty" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Up
            let c2 = KlondikeCard (Card Ace Spades) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [c1] [] [] [c2])
                        (Tableau [] [] [] [] [] [] [])
            let sOne = tableauOneToHeartFoundation s
            sOne `shouldBe` s -- no state change expected
            let sTwo = tableauTwoToHeartFoundation s
            sTwo `shouldBe` s -- no state change expected
            let sThree = tableauThreeToHeartFoundation s
            sThree `shouldBe` s -- no state change expected
            let sFour = tableauFourToHeartFoundation s
            sFour `shouldBe` s -- no state change expected
            let sFive = tableauFiveToHeartFoundation s
            sFive `shouldBe` s -- no state change expected
            let sSix = tableauSixToHeartFoundation s
            sSix `shouldBe` s -- no state change expected
            let sSeven = tableauSevenToHeartFoundation s
            sSeven `shouldBe` s -- no state change expected
        it "moves card from tableau to foundation" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Up
            let c2 = KlondikeCard (Card Ace Spades) Up
            let c3 = KlondikeCard (Card Two Hearts) Up
            let c4 = KlondikeCard (Card Two Spades) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [c1] [] [] [c2])
                        (Tableau [c3] [] [] [] [c4] [] [])
            let sHeart = tableauOneToHeartFoundation s
            head (heartsPile $ foundations sHeart)`shouldBe` c3
            let sSpade = tableauFiveToSpadeFoundation s
            head (spadesPile $ foundations sSpade)`shouldBe` c4
        it "does nothing when card can't be played" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Up
            let c2 = KlondikeCard (Card Three Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [] -- waste
                        (Foundations [c1] [] [] [])
                        (Tableau [c2] [] [] [] [] [] [])
            let s' = tableauOneToHeartFoundation s
            s' `shouldBe` s
    describe "Solitaire State Transitions -- wasteToSuitFoundation" $ do
        it "does nothing when foundaiton is empty and no Ace is playable" $ do
            let c1 = KlondikeCard (Card Three Hearts) Up
            let c2 = KlondikeCard (Card Two Spades) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c2, c1] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let sHearts = wasteToHeartsFoundation s
            sHearts `shouldBe` s -- no state change expected
            let sDiamonds = wasteToDiamondsFoundation s
            sDiamonds `shouldBe` s -- no state change expected
            let sClubs = wasteToClubsFoundation s
            sClubs `shouldBe` s -- no state change expected
            let sSpades = wasteToSpadesFoundation s
            sSpades `shouldBe` s -- no state change expected
        it "moves card from waste to foundation" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Up
            let c2 = KlondikeCard (Card Two Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c1, c2] -- waste
                        (Foundations [] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let s' = wasteToHeartsFoundation s
            head (heartsPile $ foundations s') `shouldBe` c1
            head (waste s') `shouldBe` c2
            let s'' = wasteToHeartsFoundation s'
            head (heartsPile $ foundations s'') `shouldBe` c2
            waste s'' `shouldBe` []
        it "does nothing when card can't be played" $ do
            let c1 = KlondikeCard (Card Ace Hearts) Up
            let c2 = KlondikeCard (Card Three Hearts) Up
            let s =
                    Solitaire
                        [] -- stock
                        [c2] -- waste
                        (Foundations [c1] [] [] [])
                        (Tableau [] [] [] [] [] [] [])
            let s' = wasteToHeartsFoundation s
            s' `shouldBe` s
