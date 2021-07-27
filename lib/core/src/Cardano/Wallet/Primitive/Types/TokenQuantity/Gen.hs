{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantity
    , genTokenQuantityPositive
    , genTokenQuantityFullRange
    , shrinkTokenQuantity
    , shrinkTokenQuantityPositive
    , shrinkTokenQuantityFullRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Data.Word
    ( Word64 )
import Test.QuickCheck
    ( Gen, choose, frequency, shrink, sized )

--------------------------------------------------------------------------------
-- Token quantities chosen according to the size parameter.
--------------------------------------------------------------------------------

genTokenQuantity :: Gen TokenQuantity
genTokenQuantity = sized $ \n -> quantityFromInt <$> choose (0, n)

shrinkTokenQuantity :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantity = fmap quantityFromInteger . shrink . quantityToInteger

--------------------------------------------------------------------------------
-- Token quantities chosen according to the size parameter, but strictly
-- positive.
--------------------------------------------------------------------------------

genTokenQuantityPositive :: Gen TokenQuantity
genTokenQuantityPositive = sized $ \n -> quantityFromInt <$> choose (1, max 1 n)

shrinkTokenQuantityPositive :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityPositive
    = fmap quantityFromInteger
    . filter (> 0)
    . shrink
    . quantityToInteger

--------------------------------------------------------------------------------
-- Token quantities chosen from the full range available.
--------------------------------------------------------------------------------

-- | Generates token quantities across the full range of what may be encoded
--   within a single on-chain token bundle.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
--
genTokenQuantityFullRange :: Gen TokenQuantity
genTokenQuantityFullRange = frequency
    [ ( 1, pure minTokenQuantity )
    , ( 1, pure maxTokenQuantity )
    , ( 8
      , quantityFromInteger <$>
        choose (1, quantityToInteger maxTokenQuantity - 1)
      )
    ]
  where
    minTokenQuantity :: TokenQuantity
    minTokenQuantity = TokenQuantity 0
    maxTokenQuantity :: TokenQuantity
    maxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Word64

shrinkTokenQuantityFullRange :: TokenQuantity -> [TokenQuantity]
shrinkTokenQuantityFullRange =
    -- Given that we may have a large value, we limit the number of results
    -- returned in order to avoid processing long lists of shrunken values.
    take 8 . shrinkTokenQuantity

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
