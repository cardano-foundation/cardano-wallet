{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Primitive.Passphrase.LegacySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseScheme (EncryptWithScrypt)
    , checkPassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Gen
    ( genUserPassphrase
    , shrinkUserPassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Legacy
    ( PassphraseHashLength
    , encryptPassphraseTestingOnly
    , getSalt
    , haveScrypt
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (..)
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.ByteArray
    ( ByteArray
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , elements
    , forAll
    , property
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO
    )

import qualified Cardano.Wallet.Primitive.Passphrase.Legacy as Legacy

spec :: Spec
spec = describe "Legacy scrypt password encryption scheme" $ do
    scryptoniteGoldenSpec
    legacyGoldenSpec
    scryptonitePropsSpec
    when haveScrypt
        scryptPropsSpec

{-------------------------------------------------------------------------------
                        Cryptonite-based implementation
-------------------------------------------------------------------------------}

scryptoniteGoldenSpec :: Spec
scryptoniteGoldenSpec = describe "Golden tests on checkPassphraseCryptonite" $ do
    it "Verify passphrase" $ do
        let Just salt = getSalt fixturePassphraseEncrypted
        let x = encryptPassphraseTestingOnly
                    64
                    (preparePassphrase fixturePassphrase)
                    salt
        unwrap x `shouldBe` unwrap fixturePassphraseEncrypted
        -- The above results in a better error message on failure, but this is
        -- the main thing we want to test:
        Legacy.checkPassphraseCryptonite
            (preparePassphrase fixturePassphrase)
            fixturePassphraseEncrypted
            `shouldBe` True
    it "Verify wrong passphrase" $ do
        Legacy.checkPassphraseCryptonite
            (preparePassphrase fixturePassphraseWrong)
            fixturePassphraseEncrypted `shouldBe` False
    it "Verify wrong salt" $ do
        Legacy.checkPassphraseCryptonite
            (preparePassphrase fixturePassphrase)
            fixturePassphraseEncryptedWrongSalt `shouldBe` False
    it "Verify wrong hash" $ do
        Legacy.checkPassphraseCryptonite
            (preparePassphrase fixturePassphrase)
            fixturePassphraseEncryptedWrongHash `shouldBe` False
    it "getSalt" $ do
        let hex :: Passphrase "salt" -> ByteString
            hex = convertToBase Base16 . unPassphrase
        hex <$> (getSalt fixturePassphraseEncrypted)
            `shouldBe` Just "fa3a2db452198d5997a02ecb9530595a5633057e104307ddb85bb10a4307522b"

unwrap :: ByteArray b => b -> ByteString
unwrap = convertToBase Base16

-- | Default passphrase used for fixture wallets
fixturePassphrase :: Passphrase "user"
fixturePassphrase = Passphrase "cardano-wallet"

fixturePassphraseWrong :: Passphrase "user"
fixturePassphraseWrong = Passphrase "wrong"

-- | fixturePassphrase encrypted by Scrypt function
fixturePassphraseEncrypted :: PassphraseHash
fixturePassphraseEncrypted = unsafeFromHex
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"

fixturePassphraseEncryptedWrongSalt :: PassphraseHash
fixturePassphraseEncryptedWrongSalt = unsafeFromHex
    "31347c387c317c206a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"

fixturePassphraseEncryptedWrongHash :: PassphraseHash
fixturePassphraseEncryptedWrongHash = unsafeFromHex
    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
    \597a425834515177666475467578436b4d485569733d7c78324d646738\
    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
    \51303054356c654751794279732f7662753367526d726c316c657a7150\
    \43676d364e6758476d4d2f4b6438343265304b4945773d30"

{-------------------------------------------------------------------------------
                                  Golden Tests
-------------------------------------------------------------------------------}

legacyGoldenSpec :: Spec
legacyGoldenSpec =
    describe "Golden tests comparing against cardano-sl" $ do
        it "short password" $ do
            let pwd  = Passphrase "patate"
            let hash = unsafeFromHex
                    "31347c387c317c574342652b796362417576356c2b4258676a344a314c\
                    \6343675375414c2f5653393661364e576a2b7550766655513d3d7c2f37\
                    \6738486c59723174734e394f6e4e753253302b6a65515a6b5437316b45\
                    \414941366a515867386539493d"
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()

        it "normal password" $ do
            let pwd  = Passphrase @"user" "Secure Passphrase"
            let hash = unsafeFromHex
                    "31347c387c317c714968506842665966555a336f5156434c384449744b\
                    \677642417a6c584d62314d6d4267695433776a556f3d7c53672b436e30\
                    \4232766b4475682f704265335569694577633364385845756f55737661\
                    \42514e62464443353569474f4135736e453144326743346f47564c472b\
                    \524331385958326c6863552f36687a38432f496172773d3d"
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()

        it "empty password" $ do
            let pwd  = Passphrase @"user" ""
            let hash = unsafeFromHex
                    "31347c387c317c5743424875746242496c6a66734d764934314a30727a\
                    \79663076657375724954796376766a793150554e377452673d3d7c5475\
                    \3434596d6e547957546c5759674a3164494f7974474a7842632b432f78\
                    \62507657382b5135356a38303d"
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()

        it "cardano-wallet password" $ do
            let pwd  = Passphrase @"user" "cardano-wallet"
            let hash = unsafeFromHex
                    "31347c387c317c2b6a6f747446495a6a566d586f43374c6c54425a576c\
                    \597a425834515177666475467578436b4d485569733d7c78324d646738\
                    \49554a3232507235676531393575445a76583646552b7757395a6a6a2f\
                    \51303054356c654751794279732f7662753367526d726c316c657a7150\
                    \43676d364e6758476d4d2f4b6438343265304b4945773d3d"
            checkPassphrase EncryptWithScrypt pwd hash `shouldBe` Right ()

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}
scryptonitePropsSpec :: Spec
scryptonitePropsSpec =
    describe "verification cryptonite VS encryption scrypt (if available)" $ do
        it "checkPassphrase p h(p) == True" $
            property $
                prop_passphraseFromScryptRoundtrip
                    $ Legacy.checkPassphraseCryptonite . preparePassphrase
        it "p /= p' => checkPassphrase p' h(p) == False" $
            property $
                prop_passphraseFromScryptRoundtripFail
                    $ Legacy.checkPassphraseCryptonite . preparePassphrase

scryptPropsSpec :: Spec
scryptPropsSpec =
    describe "verification scrypt VS encryption scrypt (if available)" $ do
        it "checkPassphrase p h(p) == True" $
            property $
                prop_passphraseFromScryptRoundtrip
                    $ Legacy.checkPassphraseScrypt . preparePassphrase
        it "p /= p' => checkPassphrase p' h(p) == False" $
            property $
                prop_passphraseFromScryptRoundtripFail
                    $ Legacy.checkPassphraseScrypt . preparePassphrase

prop_passphraseFromScryptRoundtrip
    :: (Passphrase "user" -> PassphraseHash -> Bool)
    -> Passphrase "user"
    -> Property
prop_passphraseFromScryptRoundtrip checkPassphrase' p =
    forAll genPassphraseHashLength $ \len ->
        monadicIO $ liftIO $ do
            hp <- encryptPasswordWithScrypt len p
            checkPassphrase' p hp `shouldBe` True

prop_passphraseFromScryptRoundtripFail
    :: (Passphrase "user" -> PassphraseHash -> Bool)
    -> Passphrase "user"
    -> Passphrase "user"
    -> Property
prop_passphraseFromScryptRoundtripFail checkPassphrase' p p' =
    forAll genPassphraseHashLength $ \len ->
        p /= p' ==> monadicIO $ liftIO $ do
            hp <- encryptPasswordWithScrypt len p
            checkPassphrase' p' hp `shouldBe` False

encryptPasswordWithScrypt
    :: PassphraseHashLength
    -> Passphrase "user"
    -> IO PassphraseHash
encryptPasswordWithScrypt len =
    encryptPassphraseTestingOnly len . preparePassphrase

genPassphraseHashLength :: Gen PassphraseHashLength
genPassphraseHashLength = elements [32, 64, 128]

instance Arbitrary (Passphrase "user") where
    arbitrary = genUserPassphrase
    shrink = shrinkUserPassphrase

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: forall b. ByteArray b => ByteString -> b
unsafeFromHex = unsafeRight . convertFromBase @ByteString @b Base16
  where
    unsafeRight :: Either a b -> b
    unsafeRight = either (error "unsafeFromHex failed") id
