module Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySmall
    , genTokenQuantityLarge
    , genTokenQuantityMixed
    , shrinkTokenQuantitySmall
    , shrinkTokenQuantityLarge
    , shrinkTokenQuantityMixed
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Test.QuickCheck
    ( Gen, choose, oneof, shrink )

--------------------------------------------------------------------------------
-- Small token quantities
--------------------------------------------------------------------------------

genTokenQuantitySmall :: Gen TokenQuantity
genTokenQuantitySmall = TokenQuantity <$> oneof
    [ pure 0
    , choose ( 1, smallPositiveValue)
    , choose (-1, smallNegativeValue)
    ]

shrinkTokenQuantitySmall :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantitySmall = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Large token quantities
--------------------------------------------------------------------------------

genTokenQuantityLarge :: Gen TokenQuantity
genTokenQuantityLarge = TokenQuantity <$> oneof
    [ choose (smallPositiveValue + 1, largePositiveValue)
    , choose (smallNegativeValue - 1, largeNegativeValue)
    ]

shrinkTokenQuantityLarge :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityLarge = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Mixed token quantities (both small and large)
--------------------------------------------------------------------------------

genTokenQuantityMixed :: Gen TokenQuantity
genTokenQuantityMixed = oneof
    [ genTokenQuantitySmall
    , genTokenQuantityLarge
    ]

shrinkTokenQuantityMixed :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityMixed = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

shrinkTokenQuantity :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantity =
    -- Since token quantities can be very large, we limit the number of results
    -- that the shrinker can return:
    take 8 . fmap TokenQuantity . shrink . unTokenQuantity

smallNegativeValue :: Integer
smallNegativeValue = Prelude.negate smallPositiveValue

smallPositiveValue :: Integer
smallPositiveValue = 10

largeNegativeValue :: Integer
largeNegativeValue = Prelude.negate largePositiveValue

largePositiveValue :: Integer
largePositiveValue = (10 :: Integer) ^ (1000 :: Integer)
