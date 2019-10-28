{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivationSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub, unXPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , FromMnemonic (..)
    , FromMnemonicError (..)
    , Index
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PersistKey (..)
    , WalletKey (..)
    , XPrv
    , checkPassphrase
    , encryptPassphrase
    , getIndex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), deserializeXPubSeq, serializeXPubSeq )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Either
    ( isRight )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , expectFailure
    , genericShrink
    , property
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Bounded / Enum relationship" $ do
        it "The calls Index.succ maxBound should result in a runtime err (hard)"
            prop_succMaxBoundHardIx
        it "The calls Index.pred minBound should result in a runtime err (hard)"
            prop_predMinBoundHardIx
        it "The calls Index.succ maxBound should result in a runtime err (soft)"
            prop_succMaxBoundSoftIx
        it "The calls Index.pred minBound should result in a runtime err (soft)"
            prop_predMinBoundSoftIx

    describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @(Passphrase "encryption")
        textRoundtrip $ Proxy @NetworkDiscriminant

    describe "Enum Roundtrip" $ do
        it "Index @'Hardened _" (property prop_roundtripEnumIndexHard)
        it "Index @'Soft _" (property prop_roundtripEnumIndexSoft)

    describe "Passphrases" $ do
        it "checkPassphrase p h(p) == Right ()" $
            property prop_passphraseRoundtrip

        it "p /= p' => checkPassphrase p' h(p) == Left ErrWrongPassphrase" $
            property prop_passphraseRoundtripFail

        it "checkPassphrase fails when hash is malformed" $
            property prop_passphraseHashMalformed

    describe "FromMnemonic" $ do
        it "early error reported first (Invalid Entropy)" $ do
            let res = fromMnemonic @'[15,18,21] @"testing"
                        [ "glimpse", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid entropy checksum: \
                \please double-check the last word of your mnemonic sentence.")

        it "early error reported first (Non-English Word)" $ do
            let res = fromMnemonic @'[15,18,21] @"testing"
                        [ "baguette", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (FromMnemonicError "Found invalid (non-English) \
                \word: \"baguette\".")

        it "early error reported first (Wrong number of words - 1)" $ do
            let res = fromMnemonic @'[15,18,21] @"testing"
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid number of words: \
                \15, 18 or 21 words are expected.")

        it "early error reported first (Wrong number of words - 2)" $ do
            let res = fromMnemonic @'[15] @"testing"
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid number of words: \
                \15 words are expected.")

        it "early error reported first (Error not in first constructor)" $ do
            let res = fromMnemonic @'[15,18,21,24] @"testing"
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (FromMnemonicError "Found invalid (non-English) \
                \word: \"盗\".")

        it "early error reported first (Error not in first constructor)" $ do
            let res = fromMnemonic @'[12,15,18] @"testing"
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (FromMnemonicError "Found invalid (non-English) \
                \word: \"盗\".")

        it "successfully parse 15 words in [15,18,21]" $ do
            let res = fromMnemonic @'[15,18,21] @"testing"
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [12,15,18]" $ do
            let res = fromMnemonic @'[12,15,18] @"testing"
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [9,12,15]" $ do
            let res = fromMnemonic @'[9,12,15] @"testing"
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

    describe "Keys storing and retrieving roundtrips" $ do
        it "XPrv ShelleyKey" (property (prop_roundtripXPrv @ShelleyKey))
        it "XPrv ByronKey" (property (prop_roundtripXPrv @ByronKey))
        it "XPub" (property prop_roundtripXPub)

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

prop_roundtripEnumIndexHard :: Index 'Hardened 'AccountK -> Property
prop_roundtripEnumIndexHard ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripEnumIndexSoft :: Index 'Soft 'AddressK -> Property
prop_roundtripEnumIndexSoft ix =
    (toEnum . fromEnum) ix === ix .&&. (toEnum . fromEnum . getIndex) ix === ix

prop_roundtripXPrv
    :: (PersistKey k, Eq (k 'RootK XPrv), Show (k 'RootK XPrv))
    => (k 'RootK XPrv, Hash "encryption")
    -> Property
prop_roundtripXPrv xpriv = do
    let xpriv' = (deserializeXPrv . serializeXPrv) xpriv
    xpriv' === Right xpriv

prop_roundtripXPub
    :: ShelleyKey 'RootK XPrv
    -> Property
prop_roundtripXPub xpriv = do
    let xpub = publicKey xpriv
    let xpub' = (deserializeXPubSeq . serializeXPubSeq) xpub
    xpub' === Right xpub

prop_passphraseRoundtrip
    :: Passphrase "encryption"
    -> Property
prop_passphraseRoundtrip pwd = monadicIO $ liftIO $ do
    hpwd <- encryptPassphrase pwd
    checkPassphrase pwd hpwd `shouldBe` Right ()

prop_passphraseRoundtripFail
    :: (Passphrase "encryption", Passphrase "encryption")
    -> Property
prop_passphraseRoundtripFail (p, p') =
    p /= p' ==> monadicIO $ liftIO $ do
        hp <- encryptPassphrase p
        checkPassphrase p' hp `shouldBe` Left ErrWrongPassphrase

prop_passphraseHashMalformed
    :: Passphrase "encryption"
    -> Property
prop_passphraseHashMalformed pwd = monadicIO $ liftIO $ do
    checkPassphrase pwd (Hash mempty) `shouldBe` Left ErrWrongPassphrase

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Passphrase goal) where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance {-# OVERLAPS #-} Arbitrary (Passphrase "encryption") where
    arbitrary = do
        let p = Proxy :: Proxy "encryption"
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes

instance Arbitrary (Hash "encryption") where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysSeq

instance Arbitrary (ShelleyKey 'RootK XPub) where
    shrink _ = []
    arbitrary = publicKey <$> arbitrary

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysRnd

instance Arbitrary NetworkDiscriminant where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

genRootKeysSeq :: Gen (ShelleyKey 'RootK XPrv)
genRootKeysSeq = do
    (s, g, e) <- (,,)
        <$> genPassphrase @"seed" (16, 32)
        <*> genPassphrase @"generation" (0, 16)
        <*> genPassphrase @"encryption" (0, 16)
    return $ Seq.generateKeyFromSeed (s, g) e

genRootKeysRnd :: Gen (ByronKey 'RootK XPrv)
genRootKeysRnd = Rnd.generateKeyFromSeed
    <$> genPassphrase @"seed" (16, 32)
    <*> genPassphrase @"encryption" (0, 16)

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes
