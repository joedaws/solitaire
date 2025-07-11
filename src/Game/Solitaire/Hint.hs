module Game.Solitaire.Hint (
    hint,
) where

import Game.Card
import Game.Solitaire.State
import Game.Solitaire.Transitions (NamedTransition (..), allTransitions)

hint :: (HasFace c, HasCard c, Eq c, Show c, IsPlayable c) => Solitaire c -> [(String, Solitaire c)]
hint s = [(name nt, s') | nt@(NamedTransition _ f) <- allTransitions, let s' = f s, s /= s']
