{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment.HttpBridge
    ( Network (..), network )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , DerivationType (..)
    , Index
    , Passphrase (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , keyToAddress
    , publicKey
    )
import Fmt
    ( build, fmt )
import Test.Hspec
    ( Spec, describe, it, xit )
import Test.QuickCheck
    ( Property, (===) )

spec :: Spec
spec = do
    describe "Golden Tests - Yoroi's style addresses" $ case network of
        Mainnet -> do
            let seed0 = Passphrase "4\175\242L\184\243\191 \169]\171 \207\r\v\233\NUL~&\ETB"
            let recPwd0 = mempty
            it "m/0'/0/0 --> Ae2tdPwUPEZGB...EfoeiuW4MtaXZ" $ do
                let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x00000000)
                goldenYoroiAddr (seed0, recPwd0) ExternalChain accIx addrIx
                    "Ae2tdPwUPEZGQVrA6qKreDzdtYxcWMMrpTFYCpFcuJfhJBEfoeiuW4MtaXZ"
            it "m/0'/0/14 --> Ae2tdPwUPEZD...bxbkCyQYyxckP" $ do
                let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x0000000E)
                goldenYoroiAddr (seed0, recPwd0) ExternalChain accIx addrIx
                    "Ae2tdPwUPEZDLWQQEBR1UW7HeXJVaqUnuw8DUFu52TDWCJbxbkCyQYyxckP"
            it "m/14'/1/42 --> Ae2tdPwUPEZ...EkxDbkPodpMAi" $ do
                let (accIx, addrIx) = (toEnum 0x8000000E, toEnum 0x0000002A)
                goldenYoroiAddr (seed0, recPwd0) InternalChain accIx addrIx
                    "Ae2tdPwUPEZFRbyhz3cpfC2CumGzNkFBN2L42rcUc2yjQpEkxDbkPodpMAi"

            let seed1 = Passphrase "\171\151\240\DC4\147Q\ACK\NULfJxq\176h\172\DEL/\DC4\DC2\227\&6\155\129\134\f\221/\NUL\175a\252\249"
            let recPwd1 = Passphrase "Cardano the cardano that cardano!"
            it "m/0'/0/0 --> Ae2tdPwUPEZ1D...64dqTSRpWqzLH" $ do
                let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x00000000)
                goldenYoroiAddr (seed1, recPwd1) ExternalChain accIx addrIx
                    "Ae2tdPwUPEZ1DYmhvpJWtVkMUbypPVkCVjQLNJeKRRG4LJ64dqTSRpWqzLH"
            it "m/0'/0/14 --> Ae2tdPwUPEZ7...pVwEPhKwseVvf" $ do
                let (accIx, addrIx) = (toEnum 0x80000000, toEnum 0x0000000E)
                goldenYoroiAddr (seed1, recPwd1) ExternalChain accIx addrIx
                    "Ae2tdPwUPEZ7ZyqyuDKkCnjrRjTY1vMJ8353gD7XWrUYufpVwEPhKwseVvf"
            it "m/14'/1/42 --> Ae2tdPwUPEZ...nRtbfw6EHRv1D" $ do
                let (accIx, addrIx) = (toEnum 0x8000000E, toEnum 0x0000002A)
                goldenYoroiAddr (seed1, recPwd1) InternalChain accIx addrIx
                    "Ae2tdPwUPEZLSqQN7XNJRMJ6yHWdfFLaQgPPYgyJKrJnCVnRtbfw6EHRv1D"

        Testnet -> do
            xit "No golden tests for 'Testnet' network" False

        Staging ->
            xit "No golden tests for 'Staging' network" False

{-------------------------------------------------------------------------------
                                Golden Tests
-------------------------------------------------------------------------------}

goldenYoroiAddr
    :: (Passphrase "seed", Passphrase "generation")
    -> ChangeChain
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> String
    -> Property
goldenYoroiAddr (seed, recPwd) cc accIx addrIx addr =
    let
        encPwd = mempty
        rootXPrv = generateKeyFromSeed (seed, recPwd) encPwd
        accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
        addrXPrv = deriveAddressPrivateKey encPwd accXPrv cc addrIx
    in
        fmt (build $ keyToAddress @HttpBridge $ publicKey addrXPrv) === addr
