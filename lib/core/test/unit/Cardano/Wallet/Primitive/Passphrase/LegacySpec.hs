{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Passphrase.LegacySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (ErrWrongPassphrase)
    , PassphraseScheme (EncryptWithScrypt)
    , checkPassphrase
    , encryptPassphrase'
    )
import Cardano.Wallet.Primitive.Passphrase.Gen
    ( genEncryptionPassphrase
    , genPassphraseScheme
    , genUserPassphrase
    , shrinkUserPassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( checkPassphraseTestingOnly, getSalt, haveScrypt, preparePassphrase )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..), PassphraseHash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteString
    ( ByteString )
import GHC.Stack
    ( HasCallStack )
import Test.Hspec
    ( Spec, before_, describe, it, pendingWith, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, property, (==>) )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteArray as BA
import qualified Data.Text.Encoding as T


spec :: Spec
spec = onlyWithScrypt $ describe "Scrypt tests-only cryptonite version" $ do
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
        let
            unwrap :: Passphrase "salt" -> ByteString
            unwrap (Passphrase x) = BA.convert x
        unwrap <$> (getSalt fixturePassphraseEncrypted)  `shouldBe` (Just "abc")

    it "checkPassphrase p h(p) == Right () for Scrypt passwords" $
        property prop_passphraseFromScryptRoundtrip
    it "p /= p' => checkPassphrase p' h(p) == Left ErrWrongPassphrase for Scrypt passwords" $
        property prop_passphraseFromScryptRoundtripFail

    parallel $ describe "golden test legacy passphrase encryption" $ do
        it "compare new implementation with cardano-sl - short password" $ do
            let pwd  = Passphrase $ BA.convert $ T.encodeUtf8 "patate"
            let hash = PassphraseHash $ BA.convert $ unsafeFromHex $ mconcat
                    [ "31347c387c317c574342652b796362417576356c2b4258676a344a314c"
                    , "6343675375414c2f5653393661364e576a2b7550766655513d3d7c2f37"
                    , "6738486c59723174734e394f6e4e753253302b6a65515a6b5437316b45"
                    , "414941366a515867386539493d"
                    ]
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()
        it "compare new implementation with cardano-sl - normal password" $ do
            let pwd  = Passphrase @"user" $ BA.convert $ T.encodeUtf8 "Secure Passphrase"
            let hash = PassphraseHash $ BA.convert $ unsafeFromHex $ mconcat
                    [ "31347c387c317c714968506842665966555a336f5156434c384449744b"
                    , "677642417a6c584d62314d6d4267695433776a556f3d7c53672b436e30"
                    , "4232766b4475682f704265335569694577633364385845756f55737661"
                    , "42514e62464443353569474f4135736e453144326743346f47564c472b"
                    , "524331385958326c6863552f36687a38432f496172773d3d"
                    ]
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()
        it "compare new implementation with cardano-sl - empty password" $ do
            let pwd  = Passphrase @"user" $ BA.convert $ T.encodeUtf8 ""
            let hash = PassphraseHash $ BA.convert $ unsafeFromHex $ mconcat
                    [ "31347c387c317c5743424875746242496c6a66734d764934314a30727a7"
                    , "9663076657375724954796376766a793150554e377452673d3d7c54753"
                    , "434596d6e547957546c5759674a3164494f7974474a7842632b432f786"
                    , "2507657382b5135356a38303d"
                    ]
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()
        it "compare new implementation with cardano-sl - cardano-wallet password" $ do
            let pwd  = Passphrase @"user" $ BA.convert $ T.encodeUtf8 "cardano-wallet"
            let hash = PassphraseHash $ BA.convert $ unsafeFromHex $ mconcat
                    [ "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c"
                    , "597a425834515177666475467578436b4d485569733d7c78324d646738"
                    , "49554a3232507235676531393575445a76583646552b7757395a6a6a2f"
                    , "51303054356c654751794279732f7662753367526d726c316c657a7150"
                    , "43676d364e6758476d4d2f4b6438343265304b4945773d3d"
                    ]
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()

onlyWithScrypt :: HasCallStack => Spec -> Spec
onlyWithScrypt =
    if haveScrypt
    then id
    else before_ (pendingWith "needs to be compiled with scrypt to run")

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

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_passphraseFromScryptRoundtrip
    :: Passphrase "user"
    -> Property
prop_passphraseFromScryptRoundtrip p = monadicIO $ liftIO $ do
    hp <- encryptPasswordWithScrypt p
    checkPassphrase EncryptWithScrypt p hp `shouldBe` Right ()

prop_passphraseFromScryptRoundtripFail
    :: Passphrase "user"
    -> Passphrase "user"
    -> Property
prop_passphraseFromScryptRoundtripFail p p' =
    p /= p' ==> monadicIO $ liftIO $ do
        hp <- encryptPasswordWithScrypt p
        checkPassphrase EncryptWithScrypt p' hp
            `shouldBe` Left ErrWrongPassphrase

encryptPasswordWithScrypt
    :: Passphrase "user"
    -> IO PassphraseHash
encryptPasswordWithScrypt = encryptPassphrase' EncryptWithScrypt


instance Arbitrary (Passphrase "user") where
    arbitrary = genUserPassphrase
    shrink = shrinkUserPassphrase

instance Arbitrary PassphraseScheme where
    arbitrary = genPassphraseScheme

instance Arbitrary (Passphrase "encryption") where
    arbitrary = genEncryptionPassphrase
