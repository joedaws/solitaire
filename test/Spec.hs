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
            let c1 = KlondikeCard (Card Five Spades) Up
            let c2 = KlondikeCard (Card Ace Hearts) Down
            let c2' = KlondikeCard (Card Ace Hearts) Up
            let s =
                    Solitaire
                        [c1, c2] -- stock
                        [] -- waste
                        (Foundations [] [] [] []) -- all foundations empty
                        (Tableau [] [] [] [] [] [] []) -- only first tableau pile populated
            let s' = stockToWaste s
            stock s' `shouldBe` [c2'] -- now that c2 is top of stack it must be up
            waste s' `shouldBe` [c1] -- the card was already up while on top of stock
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

{-
    xit "stockToWaste when no cards in stock" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts] initFoundations initTableau
      let s' = stockToWaste s
      stock s' `shouldBe` []
      waste s' `shouldBe` [Card Two Hearts]

    xit "refreshStock when stock is empty" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts, Card Three Hearts] initFoundations initTableau
      let s' = refreshStock s
      stock s' `shouldBe` [Card Three Hearts, Card Two Hearts]
      waste s' `shouldBe` []

    xit "refreshStock when stock is non-empty" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] []
      let s = Solitaire [Card Ace Hearts] [Card Two Hearts] initFoundations initTableau
      let s' = refreshStock s
      stock s' `shouldBe` [Card Ace Hearts]
      waste s' `shouldBe` [Card Two Hearts]

    xit "wasteToTableauOne when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Two Hearts] initFoundations initTableau
      let s' = wasteToTableauOne s
      let t' = one $ tableau s'
      t' `shouldBe` [Card Two Hearts, Card Three Clubs]

    xit "wasteToTableauOne when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [] [] [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauOne s
      let t' = one $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauTwo when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [Card King Spades] [] [] [] [] []
      let s = Solitaire [] [Card Queen Hearts] initFoundations initTableau
      let s' = wasteToTableauTwo s
      let t' = two $ tableau s'
      t' `shouldBe` [Card Queen Hearts, Card King Spades]

    xit "wasteToTableauTwo when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [Card Three Clubs] [] [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauTwo s
      let t' = two $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauThree when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [Card Three Clubs] [Card King Spades] [] [] [] []
      let s = Solitaire [] [Card Queen Hearts] initFoundations initTableau
      let s' = wasteToTableauThree s
      let t' = three $ tableau s'
      t' `shouldBe` [Card Queen Hearts, Card King Spades]

    xit "wasteToTableauThree when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [Card Three Clubs]  [] [] [] []
      let s = Solitaire [] [Card Five Hearts] initFoundations initTableau
      let s' = wasteToTableauThree s
      let t' = three $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauFour when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [Card Two Spades]  [] [] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauFour s
      let t' = four $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    xit "wasteToTableauFour when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [Card Three Clubs] [] [] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauFour s
      let t' = four $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauFive when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [Card Two Spades] [] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauFive s
      let t' = five $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    xit "wasteToTableauFive when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [Card Three Clubs] [] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauFive s
      let t' = five $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauSix when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [Card Two Spades] []
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauSix s
      let t' = six $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    xit "wasteToTableauSix when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [Card Three Clubs] []
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauSix s
      let t' = six $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "wasteToTableauSeven when top waste card can build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] [Card Two Spades]
      let s = Solitaire [] [Card Ace Hearts] initFoundations initTableau
      let s' = wasteToTableauSeven s
      let t' = seven $ tableau s'
      t' `shouldBe` [Card Ace Hearts, Card Two Spades]

    xit "wasteToTableauSeven when top waste card can not build" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [] [] [] [] [] [] [Card Three Clubs]
      let s = Solitaire [] [Card Five Spades] initFoundations initTableau
      let s' = wasteToTableauSeven s
      let t' = seven $ tableau s'
      t' `shouldBe` [Card Three Clubs]

    xit "tableauToTableau when toBuildPile can be built" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Three Clubs] [Card Two Hearts] [] [] [] [] []
      let s = Solitaire [] [] initFoundations initTableau
      let s' = tableauToTableau 2 1 1 s
      let t1' = one $ tableau s'
      t1' `shouldBe` [Card Two Hearts, Card Three Clubs]

    xit "tableauToTableau when toBuildPile can be built move 2" $ do
      let initFoundations = setupFoundations
      let initTableau = Tableau [Card Five Clubs] [Card Three Spades, Card Four Hearts] [] [] [] [] []
      let s = Solitaire [] [] initFoundations initTableau
      let s' = tableauToTableau 2 1 2 s
      let t1' = one $ tableau s'
      t1' `shouldBe` [Card Three Spades, Card Four Hearts, Card Five Clubs]
-}
