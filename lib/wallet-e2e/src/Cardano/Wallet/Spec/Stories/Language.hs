module Cardano.Wallet.Spec.Stories.Language (FxStory)
where

import Effectful (Eff, type (:>))

type FxStory es fxs a = Fxs es fxs => Eff es a

type family Fxs es as :: Constraint where
    Fxs es '[] = ()
    Fxs es (a ': as) = (a :> es, Fxs es as)
