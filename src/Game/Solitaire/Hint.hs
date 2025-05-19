module Game.Solitaire.Hint (
    hint,
    hint2,
) where

import Game.Card
import Game.Solitaire.State
import Game.Solitaire.Transitions (allTransitions)

hint :: (HasFace c, HasCard c, Eq c, Show c, IsPlayable c) => Solitaire c -> [Solitaire c]
hint s = filter differentState allNextStates
  where
    differentState s' = s /= s'
    allNextStates = map ($ s) allTransitions

hint2 :: (HasFace c, HasCard c, Eq c, Show c, IsPlayable c) => Solitaire c -> [Solitaire c]
hint2 start = do
    first <- hint start
    hint first
