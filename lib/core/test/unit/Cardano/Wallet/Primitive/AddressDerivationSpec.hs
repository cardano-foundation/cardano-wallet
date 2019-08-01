{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivationSpec
    ( spec
    , decodeTest1
    , decodeTest2
    , decodeTest3
    , decodeTest4
    , DecodeDerivationPath (..)
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
    , Key
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , XPrv
    , checkPassphrase
    , deserializeXPrv
    , deserializeXPub
    , encryptPassphrase
    , getIndex
    , publicKey
    , serializeXPrv
    , serializeXPub
    )
import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
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
    , property
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Text.Roundtrip
    ( textRoundtrip )


import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
    ( generateKeyFromSeed )
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
        it "XPriv" (property prop_roundtripXPriv)
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

prop_roundtripXPriv
    :: (Key 'RootK XPrv, Hash "encryption")
    -> Property
prop_roundtripXPriv xpriv = do
    let xpriv' = (deserializeXPrv . serializeXPrv) xpriv
    xpriv' === Right xpriv

prop_roundtripXPub
    :: Key 'RootK XPrv
    -> Property
prop_roundtripXPub xpriv = do
    let xpub = publicKey xpriv
    let xpub' = (deserializeXPub . serializeXPub) xpub
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

instance Arbitrary (Key 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeys

instance Arbitrary (Key 'RootK XPub) where
    shrink _ = []
    arbitrary = publicKey <$> arbitrary @(Key 'RootK XPrv)

genRootKeys :: Gen (Key 'RootK XPrv)
genRootKeys = do
    (s, g, e) <- (,,)
        <$> genPassphrase @"seed" (16, 32)
        <*> genPassphrase @"generation" (0, 16)
        <*> genPassphrase @"encryption" (0, 16)
    return $ Seq.generateKeyFromSeed (s, g) e
  where
    genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
    genPassphrase range = do
        n <- choose range
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

-- Generated on mainnet Daedalus -- first address of the initial account.
decodeTest1 :: DecodeDerivationPath
decodeTest1 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "DdzFFzCqrhsznTSvu5VS2Arte6DbsvfGL2mhezwj4T8fvJqQ4C53RYc8nrNukdpfUfxz3R5ryZTcMtFZfdq4hVkPFHD1XV2dxY7AJEon"
    , accIndex = 2147483648
    , addrIndex = 2147483648
    }

-- Generated for mainnet, first address of an additional account.
decodeTest2 :: DecodeDerivationPath
decodeTest2 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "DdzFFzCqrht2qSNuod2j3HQdxQYu7ehMnHqPMK6ZCZc1oTBfFJFTaqMF62rzWsJWZhbrN15uA4Bsp6M7t5WkqfumdnjLjZ5xRk8szuCd"
    , accIndex = 2694138340
    , addrIndex = 2512821145
    }

decodeTest3 :: DecodeDerivationPath
decodeTest3 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "37btjrVyb4KEFr9tdBDYVPUWpFAu65yfnoqWmd5aeZpBk7MKTyH5tiPZ7sFi6k4vWXS5Df7H7Z4CT4m3uJ1Ps4ck7rrzqWDmtiifXoqX2MQHSGYeon"
    , accIndex = 2147483648
    , addrIndex = 2147483648
    }

decodeTest4 :: DecodeDerivationPath
decodeTest4 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "37btjrVyb4KBbK7wGESZtW3vXSj8c8fGGHwS2b2fnCd6erjPw3Nt2Nw4RLbrYbdLbdgseiF3YaawsT1JGes9FMrW6Fuye9ANyLhkTq2EiHsnPE4qso"
    , accIndex = 3337448281
    , addrIndex = 3234874775
    }

-- | Random empty wallet. It's not possible to restore a wallet from
-- 'defMnemonic', so that's why there are two mnemonics in these tests.
addrMnemonic :: [Text]
addrMnemonic =
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Generic, Show, Eq)
