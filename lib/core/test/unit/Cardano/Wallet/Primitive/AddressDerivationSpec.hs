{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..), MkSomeMnemonicError (..), SomeMnemonic (..) )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , WalletKey (..)
    , getIndex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseHash (..), preparePassphrase )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    )
import Control.Monad
    ( replicateM )
import Data.Either
    ( isRight )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , arbitrarySizedBoundedIntegral
    , choose
    , expectFailure
    , genericShrink
    , oneof
    , property
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    parallel $ describe "Bounded / Enum relationship" $ do
        it "The calls Index.succ maxBound should result in a runtime err (hard)"
            prop_succMaxBoundHardIx
        it "The calls Index.pred minBound should result in a runtime err (hard)"
            prop_predMinBoundHardIx
        it "The calls Index.succ maxBound should result in a runtime err (soft)"
            prop_succMaxBoundSoftIx
        it "The calls Index.pred minBound should result in a runtime err (soft)"
            prop_predMinBoundSoftIx

    parallel $ describe "Enum Roundtrip" $ do
        it "Index @'Hardened _" (property prop_roundtripEnumIndexHard)
        it "Index @'Soft _" (property prop_roundtripEnumIndexSoft)

    parallel $ describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @DerivationIndex

    parallel $ describe "MkSomeMnemonic" $ do
        let noInDictErr =
                "Found an unknown word not present in the pre-defined dictionary. \
                \The full dictionary is available here: https://github.com/input\
                \-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"

        it "early error reported first (Invalid Entropy)" $ do
            let res = mkSomeMnemonic @'[15,18,21]
                        [ "glimpse", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError "Invalid entropy checksum: \
                \please double-check the last word of your mnemonic sentence.")

        it "early error reported first (Non-English Word)" $ do
            let res = mkSomeMnemonic @'[15,18,21]
                        [ "baguette", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError noInDictErr)

        it "early error reported first (Wrong number of words - 1)" $ do
            let res = mkSomeMnemonic @'[15,18,21]
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError "Invalid number of words: \
                \15, 18 or 21 words are expected.")

        it "early error reported first (Wrong number of words - 2)" $ do
            let res = mkSomeMnemonic @'[15]
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError "Invalid number of words: \
                \15 words are expected.")

        it "early error reported first (Error not in first constructor)" $ do
            let res = mkSomeMnemonic @'[15,18,21,24]
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError noInDictErr)

        it "early error reported first (Error not in first constructor)" $ do
            let res = mkSomeMnemonic @'[12,15,18]
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (MkSomeMnemonicError noInDictErr)

        it "successfully parse 15 words in [15,18,21]" $ do
            let res = mkSomeMnemonic @'[15,18,21]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [12,15,18]" $ do
            let res = mkSomeMnemonic @'[12,15,18]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [9,12,15]" $ do
            let res = mkSomeMnemonic @'[9,12,15]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

    parallel $ describe "Keys storing and retrieving roundtrips" $ do
        it "XPrv ShelleyKey"
            (property $ prop_roundtripXPrv @ShelleyKey)
        it "XPrv IcarusKey"
            (property $ prop_roundtripXPrv @IcarusKey)
        it "XPrv ByronKey"
            (property $ prop_roundtripXPrv @ByronKey)
        it "XPub ShelleyKey"
            (property $ prop_roundtripXPub @ShelleyKey)
        it "XPub IcarusKey"
            (property $ prop_roundtripXPub @IcarusKey)

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_succMaxBoundHardIx :: Property
prop_succMaxBoundHardIx = expectFailure $
    property $ succ (maxBound @(Index 'Hardened _)) `seq` ()

prop_predMinBoundHardIx :: Property
prop_predMinBoundHardIx = expectFailure $
    property $ pred (minBound @(Index 'Hardened _)) `seq` ()

prop_succMaxBoundSoftIx :: Property
prop_succMaxBoundSoftIx = expectFailure $
    property $ succ (maxBound @(Index 'Soft _)) `seq` ()

prop_predMinBoundSoftIx :: Property
prop_predMinBoundSoftIx = expectFailure $
    property $ pred (minBound @(Index 'Soft _)) `seq` ()

prop_roundtripEnumIndexHard :: Index 'WholeDomain 'AccountK -> Property
prop_roundtripEnumIndexHard ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripEnumIndexSoft :: Index 'Soft 'AddressK -> Property
prop_roundtripEnumIndexSoft ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripXPrv
    :: (PersistPrivateKey (k 'RootK), Eq (k 'RootK XPrv), Show (k 'RootK XPrv))
    => (k 'RootK XPrv, PassphraseHash)
    -> Property
prop_roundtripXPrv xpriv = do
    let xpriv' = (unsafeDeserializeXPrv . serializeXPrv) xpriv
    xpriv' === xpriv

prop_roundtripXPub
    ::  ( PersistPublicKey (k 'AccountK)
        , Eq (k 'AccountK XPub)
        , Show (k 'AccountK XPub)
        )
    => k 'AccountK XPub
    -> Property
prop_roundtripXPub key = do
    let key' = (unsafeDeserializeXPub . serializeXPub) key
    key' === key

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Passphrase "user") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "user"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "user"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "user")

instance {-# OVERLAPS #-} Arbitrary (Passphrase "generation") where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary PassphraseHash where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        pure $ PassphraseHash $ BA.convert $ BS.pack $ take 32 bytes

instance Arbitrary PassphraseScheme where
    arbitrary = genericArbitrary

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . CC.unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = CC.unXPrv a == CC.unXPrv b

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysSeqWithPass =<< genPassphrase (0, 16)

instance Arbitrary (ShelleyKey 'AccountK XPub) where
    shrink _ = []
    arbitrary = publicKey <$> (genRootKeysSeqWithPass =<< genPassphrase (0, 16))

instance Arbitrary (ShelleyKey 'RootK XPub) where
    shrink _ = []
    arbitrary = publicKey <$> arbitrary

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysRndWithPass =<< genPassphrase (0, 16)

instance Arbitrary (IcarusKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysIcaWithPass =<< genPassphrase (0, 16)

instance Arbitrary (IcarusKey 'AccountK XPub) where
    shrink _ = []
    arbitrary = publicKey <$> (genRootKeysIcaWithPass =<< genPassphrase (0, 16))

newtype Unencrypted a = Unencrypted { getUnencrypted :: a }
    deriving (Eq, Show)

instance Arbitrary (Unencrypted XPrv) where
    shrink _ = []
    arbitrary = Unencrypted <$> genAnyKeyWithPass mempty

data XPrvWithPass = XPrvWithPass XPrv (Passphrase "encryption")
    deriving (Eq, Show)

instance Arbitrary XPrvWithPass where
    shrink _ = []
    arbitrary = do
        pwd <- oneof
            [ genPassphrase (0, 16)
            , return $ Passphrase ""
            ]
        flip XPrvWithPass pwd <$> genAnyKeyWithPass pwd

instance Arbitrary DerivationIndex where
    arbitrary = DerivationIndex <$> arbitrarySizedBoundedIntegral
    shrink = genericShrink

genAnyKeyWithPass
    :: Passphrase "encryption"
    -> Gen XPrv
genAnyKeyWithPass pwd = oneof
    [ getRawKey
        <$> genRootKeysSeqWithPass pwd
    , getRawKey
        <$> genRootKeysRndWithPass pwd
    , getRawKey
        <$> genRootKeysIcaWithPass pwd
    ]

genRootKeysSeqWithPass
    :: Passphrase "encryption"
    -> Gen (ShelleyKey depth XPrv)
genRootKeysSeqWithPass encryptionPass = do
    s <- SomeMnemonic <$> genMnemonic @15
    g <- Just . SomeMnemonic <$> genMnemonic @12
    return $ Shelley.unsafeGenerateKeyFromSeed (s, g) encryptionPass

genRootKeysRndWithPass
    :: Passphrase "encryption"
    -> Gen (ByronKey 'RootK XPrv)
genRootKeysRndWithPass encryptionPass = Byron.generateKeyFromSeed
    <$> (SomeMnemonic <$> genMnemonic @12)
    <*> (pure encryptionPass)

genRootKeysIcaWithPass
    :: Passphrase "encryption"
    -> Gen (IcarusKey depth XPrv)
genRootKeysIcaWithPass encryptionPass = Icarus.unsafeGenerateKeyFromSeed
    <$> (SomeMnemonic <$> genMnemonic @15)
    <*> (pure encryptionPass)

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12
