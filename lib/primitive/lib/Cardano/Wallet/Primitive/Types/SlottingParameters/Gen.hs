module Cardano.Wallet.Primitive.Types.SlottingParameters.Gen
    ( genActiveSlotCoefficient
    , shrinkActiveSlotCoefficient
    ) where

import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( ActiveSlotCoefficient (..)
    )
import Test.QuickCheck
    ( Gen
    , choose
    )
import Prelude

genActiveSlotCoefficient :: Gen ActiveSlotCoefficient
genActiveSlotCoefficient = ActiveSlotCoefficient <$> choose (0.001, 1.0)

shrinkActiveSlotCoefficient
    :: ActiveSlotCoefficient -> [ActiveSlotCoefficient]
shrinkActiveSlotCoefficient (ActiveSlotCoefficient f)
    | f < 1 = [1]
    | otherwise = []
