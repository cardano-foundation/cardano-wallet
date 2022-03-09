{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.Primitive.Passphrase.LegacySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( checkPassphraseTestingOnly, getSalt, preparePassphrase )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..), PassphraseHash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.ByteArray as BA

spec :: Spec
spec = describe "Scrypt tests-only cryptonite version" $ do
    it "Verify passphrase" $ do
        checkPassphraseTestingOnly (preparePassphrase fixturePassphrase)
            fixturePassphraseEncrypted `shouldBe` True
    it "Verify wrong passphrase" $ do
        checkPassphraseTestingOnly (preparePassphrase fixturePassphraseWrong)
            fixturePassphraseEncrypted `shouldBe` False
    it "Verify wrong salt" $ do
        checkPassphraseTestingOnly (preparePassphrase fixturePassphrase)
            fixturePassphraseEncryptedWrongSalt `shouldBe` False
    it "Verify wrong hash" $ do
        checkPassphraseTestingOnly (preparePassphrase fixturePassphrase)
            fixturePassphraseEncryptedWrongHash `shouldBe` False
    it "getSalt" $ do
        getSalt fixturePassphraseEncrypted `shouldBe` Just (Passphrase "abc")

-- | Default passphrase used for fixture wallets
fixturePassphrase :: Passphrase "user"
fixturePassphrase = Passphrase "cardano-wallet"

fixturePassphraseWrong :: Passphrase "user"
fixturePassphraseWrong = Passphrase "wrong"

-- | fixturePassphrase encrypted by Scrypt function
fixturePassphraseEncrypted :: PassphraseHash
fixturePassphraseEncrypted = PassphraseHash $ BA.convert $ unsafeFromHex
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"

fixturePassphraseEncryptedWrongSalt :: PassphraseHash
fixturePassphraseEncryptedWrongSalt = PassphraseHash $ BA.convert $ unsafeFromHex
    "31347c387c317c206a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"

fixturePassphraseEncryptedWrongHash :: PassphraseHash
fixturePassphraseEncryptedWrongHash = PassphraseHash $ BA.convert $ unsafeFromHex
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d30"
