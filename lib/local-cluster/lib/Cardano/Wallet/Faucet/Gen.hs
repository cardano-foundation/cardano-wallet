{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Faucet.Gen
    ( genFaucetFunds
    ) where

import Prelude

import Cardano.Wallet.Faucet.Gen.Address
    ( genAddress
    )
import Cardano.Wallet.Launch.Cluster.Cluster
    ( FaucetFunds (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( fromFlatList
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText
    )
import Control.Monad
    ( replicateM
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , choose
    , vectorOf
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

genFaucetFunds :: Gen FaucetFunds
genFaucetFunds =
    FaucetFunds
        <$> generateAddrCoins
        <*> generateMaryAllegraFunds
        <*> generateAddrCoins
  where
    genCoin = Coin . fromIntegral <$> choose (1 :: Int, 1000000)
    generateAddrCoins = do
        n <- choose (1, 10)
        replicateM n $ (,) <$> genAddress <*> genCoin
    generateMaryAllegraFunds = do
        n <- choose (1, 10)
        replicateM n $ do
            addr <- genAddress
            coin <- genCoin
            assets <- generateAssets
            keys <- generateKeys
            pure (addr, (fromFlatList coin assets, keys))
    generateAssets = do
        n <- choose (1, 10)
        replicateM n $ do
            policyId <-
                UnsafeTokenPolicyId . Hash . B8.pack
                    <$> vectorOf 28 arbitrary
            assetNameLength <- choose (5, 16)
            assetName <-
                unsafeFromText
                    . T.decodeUtf8
                    . convertToBase Base16
                    . B8.pack
                    <$> vectorOf assetNameLength arbitrary
            quantity <-
                TokenQuantity . fromIntegral
                    <$> choose (1 :: Int, 1000000)
            pure (AssetId {..}, quantity)
    generateKeys = do
        n <- choose (1, 10)
        replicateM n $ (,) <$> arbitrary <*> arbitrary
