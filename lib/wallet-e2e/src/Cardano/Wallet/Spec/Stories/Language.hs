module Cardano.Wallet.Spec.Stories.Language (FxStory) where

import Effectful
    ( Eff
    , (:>)
    )

type FxStory otherEffects knownEffects a =
    Fxs otherEffects knownEffects => Eff otherEffects a

type family Fxs otherEffects knownEffects :: Constraint where
    Fxs otherEffects '[] = ()
    Fxs otherEffects (knownEffect ': otherKnownEffects) =
        (knownEffect :> otherEffects, Fxs otherEffects otherKnownEffects)
