module Game.Solitaire.Hint (
    hint,
) where

import Game.Card
import Game.Solitaire.State
import Game.Solitaire.Transitions (allTransitions)

hint :: (HasCard c, Eq c, Show c, IsPlayable c) => Solitaire c -> [Solitaire c]
hint s = filter differentState allNextStates
  where
    differentState s' = s /= s'
    allNextStates = map ($ s) allTransitions
