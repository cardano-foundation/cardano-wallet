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

    -- * Generators
    , genAddress
    , genLegacyAddress
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub, unXPrv )
import Cardano.Wallet.Gen
    ( genMnemonic )
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
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SomeMnemonic (..)
    , WalletKey (..)
    , XPrv
    , checkPassphrase
    , encryptPassphrase
    , getIndex
    , hex
    , unXPrvStripPub
    , unXPrvStripPubCheckRoundtrip
    , xPrvFromStrippedPubXPrv
    , xPrvFromStrippedPubXPrvCheckRoundtrip
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( KnownNetwork (..), ShelleyKey (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), PassphraseScheme (..) )
import Control.Arrow
    ( left )
import Control.Monad
    ( replicateM, (>=>) )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , NonNegative (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , classify
    , counterexample
    , expectFailure
    , label
    , oneof
    , property
    , vectorOf
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Ica
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
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
        let noInDictErr =
                "Found an unknown word not present in the pre-defined dictionary. \
                \The full dictionary is available here: https://github.com/input\
                \-output-hk/cardano-wallet/tree/master/specifications/mnemonic/english.txt"

        it "early error reported first (Invalid Entropy)" $ do
            let res = fromMnemonic @'[15,18,21]
                        [ "glimpse", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid entropy checksum: \
                \please double-check the last word of your mnemonic sentence.")

        it "early error reported first (Non-English Word)" $ do
            let res = fromMnemonic @'[15,18,21]
                        [ "baguette", "paper", "toward", "fine", "alert"
                        , "baby", "pyramid", "alone", "shaft", "force"
                        , "circle", "fancy", "squeeze", "cannon", "toilet"
                        ]
            res `shouldBe` Left (FromMnemonicError noInDictErr)

        it "early error reported first (Wrong number of words - 1)" $ do
            let res = fromMnemonic @'[15,18,21]
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid number of words: \
                \15, 18 or 21 words are expected.")

        it "early error reported first (Wrong number of words - 2)" $ do
            let res = fromMnemonic @'[15]
                        ["mom", "unveil", "slim", "abandon"
                        , "nut", "cash", "laugh", "impact"
                        , "system", "split", "depth", "sun"
                        ]
            res `shouldBe` Left (FromMnemonicError "Invalid number of words: \
                \15 words are expected.")

        it "early error reported first (Error not in first constructor)" $ do
            let res = fromMnemonic @'[15,18,21,24]
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (FromMnemonicError noInDictErr)

        it "early error reported first (Error not in first constructor)" $ do
            let res = fromMnemonic @'[12,15,18]
                        ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵"
                        ,"祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"
                        ]
            res `shouldBe` Left (FromMnemonicError noInDictErr)

        it "successfully parse 15 words in [15,18,21]" $ do
            let res = fromMnemonic @'[15,18,21]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [12,15,18]" $ do
            let res = fromMnemonic @'[12,15,18]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

        it "successfully parse 15 words in [9,12,15]" $ do
            let res = fromMnemonic @'[9,12,15]
                        ["cushion", "anxiety", "oval", "village", "choose"
                        , "shoot", "over", "behave", "category", "cruise"
                        , "track", "either", "maid", "organ", "sock"
                        ]
            res `shouldSatisfy` isRight

    describe "Keys storing and retrieving roundtrips" $ do
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

    describe "unXPrvStripPub & xPrvFromStrippedPubXPrv" $ do
        it "xPrvFromStrippedPubXPrv and unXPrvStripPub"
              (property prop_strippedPubXPrvRoundtrip1)
        it "xPrvFromStrippedPubXPrv and unXPrvStripPubCheckRoundtrip"
              (property prop_strippedPubXPrvRoundtrip2)
        it "xPrvFromStrippedPubXPrvCheckRoundtrip and unXPrvStripPub"
              (property prop_strippedPubXPrvRoundtrip3)
        it "xPrvFromStrippedPubXPrvCheckRoundtrip and unXPrvStripPubCheckRoundtrip"
              (property prop_strippedPubXPrvRoundtrip4)

        it "(xPrvFromStrippedPubXPrv bs) fails if (BS.length bs) /= 96"
            (property prop_xPrvFromStrippedPubXPrvLengthRequirement)

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
    => (k 'RootK XPrv, Hash "encryption")
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
prop_roundtripXPub xpub = do
    let xpub' = (unsafeDeserializeXPub . serializeXPub) xpub
    xpub' === xpub

prop_passphraseRoundtrip
    :: PassphraseScheme
    -> Passphrase "encryption"
    -> Property
prop_passphraseRoundtrip scheme pwd = monadicIO $ liftIO $ do
    hpwd <- encryptPassphrase scheme pwd
    checkPassphrase scheme pwd hpwd `shouldBe` Right ()

prop_passphraseRoundtripFail
    :: (PassphraseScheme, Passphrase "encryption")
    -> (PassphraseScheme, Passphrase "encryption")
    -> Property
prop_passphraseRoundtripFail (s,p) (s',p') =
    if p == p' && s == s'
    then property True
    else monadicIO $ liftIO $ do
        hp <- encryptPassphrase s p
        checkPassphrase s' p' hp `shouldBe` Left ErrWrongPassphrase

prop_passphraseHashMalformed
    :: PassphraseScheme
    -> Passphrase "encryption"
    -> Property
prop_passphraseHashMalformed scheme pwd = monadicIO $ liftIO $ do
    checkPassphrase scheme pwd (Hash mempty) `shouldBe` Left ErrWrongPassphrase

-- | xPrvFromStrippedPubXPrv and unXPrvStripPub
prop_strippedPubXPrvRoundtrip1 :: XPrvWithPass -> Property
prop_strippedPubXPrvRoundtrip1 (XPrvWithPass k enc) = do
    let bytes = unXPrvStripPub k
    let Right res = xPrvFromStrippedPubXPrv bytes
    counterexample (show . hex $ bytes) $
        if enc == Passphrase ""
        then label "no passphrase" (res === k)
        else label "passphrase" $ do
            counterexample "shoudn't roundtrip with passphrase"
                $ property $ res /= k

-- | xPrvFromStrippedPubXPrv and unXPrvStripPubCheckRoundtrip
prop_strippedPubXPrvRoundtrip2 :: XPrvWithPass -> Property
prop_strippedPubXPrvRoundtrip2 (XPrvWithPass k enc) = do
    let bytes = left show $ unXPrvStripPubCheckRoundtrip k
    let res = xPrvFromStrippedPubXPrv' <$> bytes
    counterexample (either (const "") (show . hex) bytes) $
        if enc == Passphrase ""
        then label "no passphrase" (res === Right k)
        else label "passphrase" $ do
            case res of
                Right _ ->
                    counterexample "shoudn't roundtrip with passphrase"
                        $ property False
                Left _ ->
                    label "error" True
  where
   -- The input cannot have wrong length, so we discard the possibility of
   -- @Left@.
    xPrvFromStrippedPubXPrv' = either (error . show) id . xPrvFromStrippedPubXPrv

-- | xPrvFromStrippedPubXPrvCheckRoundtrip and unXPrvStripPub
prop_strippedPubXPrvRoundtrip3 :: XPrvWithPass -> Property
prop_strippedPubXPrvRoundtrip3 (XPrvWithPass k enc) = do
    let bytes = unXPrvStripPub k
    let res = xPrvFromStrippedPubXPrvCheckRoundtrip bytes
    counterexample (show $ hex bytes) $
        if enc == Passphrase ""
        then label "no passphrase" (res === Right k)
        else label "passphrase" $ do
            case res of
                Right k' -> label "false success" $ k' /= k
                Left _ -> label "error" True

-- | xPrvFromStrippedPubXPrvCheckRoundtrip and unXPrvStripPubCheckRoundtrip
prop_strippedPubXPrvRoundtrip4 :: XPrvWithPass -> Property
prop_strippedPubXPrvRoundtrip4 (XPrvWithPass k enc) = do
    let bytes = left show $ unXPrvStripPubCheckRoundtrip k
    let res = left show . xPrvFromStrippedPubXPrvCheckRoundtrip =<< bytes
    counterexample (either (const "") (show . hex) bytes) $
        if enc == Passphrase ""
        then label "no passphrase" (res === Right k)
        else label "passphrase" $ do
            case res of
                Right _ ->
                    counterexample "shoudn't roundtrip with passphrase"
                        $ property False
                Left _ ->
                    label "error" True

prop_xPrvFromStrippedPubXPrvLengthRequirement
    :: Unencrypted XPrv
    -> NonNegative Int
    -> Property
prop_xPrvFromStrippedPubXPrvLengthRequirement (Unencrypted k) (NonNegative n) = do
    let f = toStripped >=> (return . BS.take n) >=> fromStripped
    let k' = f k
    -- A reason for writing the test using BS.take n instead of say vectorOf
    -- was guarding against
    -- https://github.com/input-output-hk/cardano-crypto/issues/67
    n < 96 ==> property $ isLeft k'
        & counterexample ("n = " ++ show n)
        & counterexample ("result = " ++ show k')
        & classify (n == 96) "== 96"
        & classify (n < 96) "< 96"
  where
    toStripped = left show . unXPrvStripPubCheckRoundtrip
    fromStripped = left show . xPrvFromStrippedPubXPrvCheckRoundtrip

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance (PassphraseMaxLength purpose, PassphraseMinLength purpose) =>
    Arbitrary (Passphrase purpose) where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy purpose
    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy purpose

instance {-# OVERLAPS #-} Arbitrary (Passphrase "generation") where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary (Hash "encryption") where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes

instance Arbitrary PassphraseScheme where
    arbitrary = genericArbitrary

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . unXPrv

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

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
    return $ Seq.unsafeGenerateKeyFromSeed (s, g) encryptionPass

genRootKeysRndWithPass
    :: Passphrase "encryption"
    -> Gen (ByronKey 'RootK XPrv)
genRootKeysRndWithPass encryptionPass = Rnd.generateKeyFromSeed
    <$> (SomeMnemonic <$> genMnemonic @12)
    <*> (pure encryptionPass)

genRootKeysIcaWithPass
    :: Passphrase "encryption"
    -> Gen (IcarusKey depth XPrv)
genRootKeysIcaWithPass encryptionPass = Ica.unsafeGenerateKeyFromSeed
    <$> (SomeMnemonic <$> genMnemonic @15)
    <*> (pure encryptionPass)

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

genAddress
    :: forall (network :: NetworkDiscriminant). (KnownNetwork network)
    => Gen Address
genAddress = oneof
    [ (\bytes -> Address (BS.pack (addrSingle @network:bytes)))
        <$> vectorOf Seq.publicKeySize arbitrary
    , (\bytes -> Address (BS.pack (addrGrouped @network:bytes)))
        <$> vectorOf (2*Seq.publicKeySize) arbitrary
    , (\bytes -> Address (BS.pack (addrAccount @network:bytes)))
        <$> vectorOf Seq.publicKeySize arbitrary
    ]

genLegacyAddress :: (Int, Int) -> Gen Address
genLegacyAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    addrPayload <- BS.pack <$> vectorOf n arbitrary
    let crc = BS.pack [26,1,2,3,4]
    return $ Address (prefix <> addrPayload <> crc)

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12
