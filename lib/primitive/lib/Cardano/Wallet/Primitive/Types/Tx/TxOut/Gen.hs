{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
  ( genTxOut
  , genTxOutCoin
  , genTxOutTokenBundle
  , shrinkTxOut
  , shrinkTxOutCoin
  )
where

import Cardano.Wallet.Primitive.Types.Address.Gen
  ( genAddress
  , shrinkAddress
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..)
  )
import Cardano.Wallet.Primitive.Types.Coin qualified as Coin
import Cardano.Wallet.Primitive.Types.TokenBundle
  ( TokenBundle
  )
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
  ( genTokenBundleSmallRange
  , shrinkTokenBundleSmallRange
  )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
  ( genAssetIdLargeRange
  )
import Cardano.Wallet.Primitive.Types.TokenQuantity
  ( TokenQuantity (..)
  )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
  ( coinIsValidForTxOut
  , txOutMaxCoin
  , txOutMaxTokenQuantity
  , txOutMinCoin
  , txOutMinTokenQuantity
  )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
  ( TxOut (..)
  )
import Control.Monad
  ( replicateM
  )
import Data.List qualified as L
import Test.QuickCheck
  ( Gen
  , choose
  , frequency
  , oneof
  , shrinkMapBy
  , suchThat
  )
import Test.QuickCheck.Extra
  ( chooseNatural
  , shrinkInterleaved
  , shrinkNatural
  )
import Prelude

--------------------------------------------------------------------------------
-- Transaction outputs generated according to the size parameter
--------------------------------------------------------------------------------

genTxOut :: Gen TxOut
genTxOut =
  TxOut
    <$> genAddress
    <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

shrinkTxOut :: TxOut -> [TxOut]
shrinkTxOut (TxOut a b) =
  uncurry TxOut
    <$> shrinkInterleaved
      (a, shrinkAddress)
      (b, filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Coins chosen from the full range allowed in a transaction output
--------------------------------------------------------------------------------

-- | Generates coins across the full range allowed in a transaction output.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
genTxOutCoin :: Gen Coin
genTxOutCoin =
  frequency
    [ (1, pure txOutMinCoin)
    , (1, pure txOutMaxCoin)
    ,
      ( 8
      , Coin.fromNatural
          <$> chooseNatural
            ( Coin.toNatural txOutMinCoin + 1
            , Coin.toNatural txOutMaxCoin - 1
            )
      )
    ]

shrinkTxOutCoin :: Coin -> [Coin]
shrinkTxOutCoin =
  L.filter coinIsValidForTxOut
    . shrinkMapBy Coin.fromNatural Coin.toNatural shrinkNatural

--------------------------------------------------------------------------------
-- Token bundles with fixed numbers of assets.
--
-- Values are chosen from across the full range of values permitted within
-- transaction outputs.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTxOutTokenBundle :: Int -> Gen TokenBundle
genTxOutTokenBundle fixedAssetCount =
  TokenBundle.fromFlatList
    <$> genTxOutCoin
    <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity =
      (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genTokenQuantity =
      integerToTokenQuantity
        <$> oneof
          [ pure $ tokenQuantityToInteger txOutMinTokenQuantity
          , pure $ tokenQuantityToInteger txOutMaxTokenQuantity
          , choose
              ( tokenQuantityToInteger txOutMinTokenQuantity + 1
              , tokenQuantityToInteger txOutMaxTokenQuantity - 1
              )
          ]
      where
        tokenQuantityToInteger :: TokenQuantity -> Integer
        tokenQuantityToInteger = fromIntegral . unTokenQuantity

        integerToTokenQuantity :: Integer -> TokenQuantity
        integerToTokenQuantity = TokenQuantity . fromIntegral
