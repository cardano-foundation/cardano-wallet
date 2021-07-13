module Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantitySized
    , genTokenQuantitySmall
    , genTokenQuantitySmallPositive
    , genTokenQuantityLarge
    , genTokenQuantityMassive
    , genTokenQuantityMixed
    , shrinkTokenQuantitySized
    , shrinkTokenQuantitySmall
    , shrinkTokenQuantitySmallPositive
    , shrinkTokenQuantityLarge
    , shrinkTokenQuantityMassive
    , shrinkTokenQuantityMixed
    , tokenQuantitySmall
    , tokenQuantityLarge
    , tokenQuantityMassive
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Data.Word
    ( Word64 )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Gen, choose, oneof, shrink, sized )

--------------------------------------------------------------------------------
-- Token quantities chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genTokenQuantitySized :: Gen TokenQuantity
genTokenQuantitySized = sized $ \n ->
    quantityFromInt <$> choose (0, n)

shrinkTokenQuantitySized :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantitySized = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Small token quantities
--------------------------------------------------------------------------------

genTokenQuantitySmall :: Gen TokenQuantity
genTokenQuantitySmall = quantityFromInteger <$> oneof
    [ pure 0
    , choose (1, quantityToInteger tokenQuantitySmall)
    ]

shrinkTokenQuantitySmall :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantitySmall = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Small strictly-positive token quantities
--------------------------------------------------------------------------------

genTokenQuantitySmallPositive :: Gen TokenQuantity
genTokenQuantitySmallPositive = quantityFromInteger <$>
    choose (1, quantityToInteger tokenQuantitySmall)

shrinkTokenQuantitySmallPositive :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantitySmallPositive q = quantityFromInteger <$>
    filter (> 0) (shrink $ quantityToInteger q)

--------------------------------------------------------------------------------
-- Large token quantities
--------------------------------------------------------------------------------

genTokenQuantityLarge :: Gen TokenQuantity
genTokenQuantityLarge = quantityFromInteger <$> choose
    ( quantityToInteger tokenQuantitySmall + 1
    , quantityToInteger tokenQuantityLarge
    )

shrinkTokenQuantityLarge :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityLarge = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Massive token quantities
--------------------------------------------------------------------------------

genTokenQuantityMassive :: Gen TokenQuantity
genTokenQuantityMassive = quantityFromInteger <$> choose
    ( quantityToInteger tokenQuantityLarge + 1
    , quantityToInteger tokenQuantityMassive
    )

shrinkTokenQuantityMassive :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityMassive = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Mixed token quantities (both small and large)
--------------------------------------------------------------------------------

genTokenQuantityMixed :: Gen TokenQuantity
genTokenQuantityMixed = oneof
    [ genTokenQuantitySmall
    , genTokenQuantityLarge
    , genTokenQuantityMassive
    ]

shrinkTokenQuantityMixed :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityMixed = shrinkTokenQuantity

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

shrinkTokenQuantity :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantity
    -- Since token quantities can be very large, we limit the number of results
    -- that the shrinker can return:
    = take 8
    . fmap quantityFromInteger
    . shrink
    . quantityToInteger

tokenQuantitySmall :: TokenQuantity
tokenQuantitySmall = TokenQuantity 10

tokenQuantityLarge :: TokenQuantity
tokenQuantityLarge = TokenQuantity $ fromIntegral (maxBound :: Word64)

tokenQuantityMassive :: TokenQuantity
tokenQuantityMassive = TokenQuantity $ (10 :: Natural) ^ (1000 :: Natural)

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

quantityToInteger :: TokenQuantity -> Integer
quantityToInteger (TokenQuantity q) = fromIntegral q

quantityFromInt :: Int -> TokenQuantity
quantityFromInt i
    | i < 0 = error $ "Unable to convert integer to token quantity: " <> show i
    | otherwise = TokenQuantity $ fromIntegral i

quantityFromInteger :: Integer -> TokenQuantity
quantityFromInteger i
    | i < 0 = error $ "Unable to convert integer to token quantity: " <> show i
    | otherwise = TokenQuantity $ fromIntegral i
