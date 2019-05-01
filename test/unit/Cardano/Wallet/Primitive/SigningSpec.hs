{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.SigningSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment
    ( Network (..), network )
import Cardano.Wallet.Binary
    ( encodeSignedTx, toByteString )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressScheme (..) )
import Cardano.Wallet.Primitive.Signing
    ( mkStdTx )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..) )
import Control.Arrow
    ( first )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Map
    ( Map )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe, xit )

import qualified Data.ByteArray as BA
import qualified Data.Map as Map


spec :: Spec
spec = do
    describe "Golden Tests - Cardano-SL" $ case network of
        Mainnet -> do
            goldenTestSignedTx 1
                [(xprv "addr-0", Coin 42)]
                "820182839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf8346\
                \1ace32d79336579a1939b3aad1c0b700ff9f8282d818582183581c8db0f00f\
                \abc0ff30e85ee171cefb4970c91d6b21a11038d7bbb581b3a0001ae7a1791b\
                \182affa0818200d81858858258403d4a81396e88155da40ec9e9807b77e4e5\
                \272cfd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1f994c8a7af5e08\
                \05b9e945c091e2e7f7d987bf4f4561df5840a21fd4bfe3f3ae4f33bec35ff8\
                \6ac28a5de741739c408dad9c8287a19cde4f1361045ef86dc1f68b717e1318\
                \506ae3c69235b0f6e09a2225fd2d2856fb57c309"
        Testnet -> do
            goldenTestSignedTx 1
                [(xprv "addr-0", Coin 42)]
                "820182839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf8346\
                \1ace32d79336579a1939b3aad1c0b700ff9f8282d818582883581c72dc02f5\
                \3b5c84217630f8d25ba629c4a4e4575f1c21402e8028667ba102451a4170cb\
                \17001aaf875866182affa0818200d81858858258403d4a81396e88155da40e\
                \c9e9807b77e4e5272cfd76a11f2dba6b6bd0a35195e720a2fd86f6692378c1\
                \f994c8a7af5e0805b9e945c091e2e7f7d987bf4f4561df5840334b1d54f35c\
                \e51f23e16d8541d8ebbbdcf3fa56f1fdcdaa427600510266ec90d0b1fba417\
                \6d61ef70bb54fde7c6f82ed089fe152d6ad4bdaab2e798156ab709"
        Local ->
            xit "No golden tests for 'Local' network" False
        Staging ->
            xit "No golden tests for 'Staging' network" False

{-------------------------------------------------------------------------------
                                Golden Tests
-------------------------------------------------------------------------------}

xprv :: ByteString -> Key 'AddressK XPrv
xprv seed =
    unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) mempty

goldenTestSignedTx
    :: Int
        -- ^ Number of outputs
    -> [(Key 'AddressK XPrv, Coin)]
        -- ^ (Address Private Keys, Output value)
    -> ByteString
        -- ^ Expected result, in Base16
    -> SpecWith ()
goldenTestSignedTx nOuts xprvs expected = it title $ do
    let addrs = first (keyToAddress . publicKey) <$> xprvs
    let s = Map.fromList (zip (fst <$> addrs) (fst <$> xprvs))
    let inps = mkInput <$> zip addrs [0..]
    let outs = take nOuts $ mkOutput <$> cycle addrs
    let res = mkStdTx s (rootXPrv, mempty) inps outs
    case res of
        Left e -> fail (show e)
        Right tx -> do
            let bytes = convertToBase Base16 $ toByteString $ encodeSignedTx tx
            bytes `shouldBe` expected
  where
    title :: String
    title = mempty
        <> "golden test | signed tx | "
        <> show (length xprvs) <> " inputs | "
        <> show nOuts <> " outputs"

    mkInput :: ((Address, Coin), Word32) -> (TxIn, TxOut)
    mkInput (out, ix) =
        ( TxIn faucetTx ix
        , mkOutput out
        )

    mkOutput :: (Address, Coin) -> TxOut
    mkOutput =
        uncurry TxOut

    faucetTx :: Hash "Tx"
    faucetTx = either (\e -> error $ "faucetTx: " <> e) Hash $ convertFromBase
        @ByteString Base16 "8258203B40265111D8BB3C3C608D95B3A0BF83461ACE32D79336579A1939B3AAD1C0B700"

    rootXPrv :: Key 'RootK XPrv
    rootXPrv =
        error "rootXPrv was evaluated but it shouldn't"

instance AddressScheme (Map Address (Key 'AddressK XPrv)) where
    keyFrom addr _ = Map.lookup addr
    nextChangeAddress = error "AddressScheme.nextChangeAddress: not implemented"
