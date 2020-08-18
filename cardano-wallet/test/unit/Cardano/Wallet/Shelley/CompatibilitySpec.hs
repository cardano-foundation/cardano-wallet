{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Crypto.Hash.Class
    ( digest )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..), DecodeStakeAddress (..), EncodeStakeAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , WalletKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Slotting
    ( fromFlatSlot )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , ChimericAccount (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , Hash (..)
    , SlotId (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock
    , TPraosStandardCrypto
    , decentralizationLevelFromPParams
    , fromTip
    , invertUnitInterval
    , toCardanoHash
    , toPoint
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkEntropy )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( forM_ )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( Ratio, (%) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Word
    ( Word64 )
import GHC.TypeLits
    ( natVal )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( Crypto (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), Point, SlotNo (..), Tip (..), getTipPoint )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , frequency
    , genericShrink
    , oneof
    , property
    , vector
    , (===)
    )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.PParams as SL

spec :: Spec
spec = do
    describe "Conversions" $
        it "toPoint' . fromTip' == getTipPoint" $ property $ \gh tip -> do
            let fromTip' = fromTip gh
            let toPoint' = toPoint gh :: W.BlockHeader -> Point (CardanoBlock TPraosStandardCrypto)
            toPoint' (fromTip' tip) === (getTipPoint tip)

    describe "Shelley StakeAddress" $ do
        prop "roundtrip / Mainnet" $ \x ->
            (decodeStakeAddress @'Mainnet . encodeStakeAddress @'Mainnet) x
            ===
            Right x

        prop "roundtrip / Testnet" $ \x ->
            (decodeStakeAddress @('Testnet 0) . encodeStakeAddress @('Testnet 0)) x
            ===
            Right x

    describe "Shelley Addresses" $ do
        prop "(Mainnet) can be deserialised by shelley ledger spec" $ \k -> do
            let Address addr = paymentAddress @'Mainnet @ShelleyKey k
            case SL.deserialiseAddr @TPraosStandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        prop "Shelley addresses from base16, bech32 and base58" $ \k -> do
            let addr@(Address bytes) = paymentAddress @'Mainnet @ShelleyKey k
            conjoin
                [ decodeAddress @'Mainnet (base16 bytes) === Right addr
                    & counterexample (show $ base16 bytes)
                , decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)
                , decodeAddress @'Mainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)
                ]

        prop "Byron addresses from base16, bech32 and base58" $ \k -> do
            let addr@(Address bytes) = paymentAddress @'Mainnet @ByronKey k
            conjoin
                [ decodeAddress @'Mainnet (base16 bytes) === Right addr
                    & counterexample (show $ base16 bytes)
                , decodeAddress @'Mainnet (bech32 bytes) === Right addr
                    & counterexample (show $ bech32 bytes)
                , decodeAddress @'Mainnet (base58 bytes) === Right addr
                    & counterexample (show $ base58 bytes)
                ]

    describe "decentralizationLevelFromPParams" $ do

        let mkDecentralizationParam :: SL.UnitInterval -> SL.PParams
            mkDecentralizationParam i = SL.emptyPParams { SL._d = i }

        let testCases :: [(Ratio Word64, Text)]
            testCases =
                [ (10 % 10,   "0.00%")
                , ( 9 % 10,  "10.00%")
                , ( 5 % 10,  "50.00%")
                , ( 1 % 10,  "90.00%")
                , ( 0 % 10, "100.00%")
                ]

        forM_ testCases $ \(input, expectedOutput) -> do
            let title = show input <> " -> " <> show expectedOutput
            let output = input
                    & SL.truncateUnitInterval
                    & mkDecentralizationParam
                    & decentralizationLevelFromPParams
                    & unDecentralizationLevel
                    & toText
            it title $ output `shouldBe` expectedOutput

    describe "Utilities" $ do

        describe "UnitInterval" $ do

            it "coverage adequate" $
                checkCoverage $ property $ \i ->
                    let half = SL.truncateUnitInterval (1 % 2) in
                    cover 10 (i == half) "i = 0.5" $
                    cover 10 (i == SL.interval0) "i = 0" $
                    cover 10 (i == SL.interval1) "i = 1" $
                    cover 10 (i > SL.interval0 && i < half) "0 < i < 0.5" $
                    cover 10 (half < i && i < SL.interval1) "0.5 < i < 1"
                    True

            it "invertUnitInterval . invertUnitInterval == id" $
                property $ \i ->
                    invertUnitInterval (invertUnitInterval i) `shouldBe` i

            it "intervalValue i + intervalValue (invertUnitInterval i) == 1" $
                property $ \i ->
                    SL.intervalValue i + SL.intervalValue (invertUnitInterval i)
                        `shouldBe` 1

            it "invertUnitInterval interval0 == interval1" $
                invertUnitInterval SL.interval0 `shouldBe` SL.interval1

            it "invertUnitInterval interval1 == interval0" $
                invertUnitInterval SL.interval1 `shouldBe` SL.interval0

            it "invertUnitInterval half == half" $
                let half = SL.truncateUnitInterval (1 % 2) in
                invertUnitInterval half `shouldBe` half

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary ChimericAccount where
    arbitrary = ChimericAccount . BS.pack <$> vector 28

instance Arbitrary (Tip (CardanoBlock TPraosStandardCrypto)) where
    arbitrary = frequency
        [ (10, return TipGenesis)
        , (90, arbitraryTip)
        ]
      where
        arbitraryTip = do
            n <- choose (0, 100)
            hash <- toCardanoHash
                . Hash
                . digest (Proxy @(HASH TPraosStandardCrypto))
                . BS.pack <$> vector 5
            return $ Tip (SlotNo n) hash (BlockNo n)

epochLength :: EpochLength
epochLength = EpochLength 10

instance Arbitrary SL.UnitInterval where
    arbitrary = oneof
        [ pure SL.interval0
        , pure SL.interval1
        , pure $ SL.truncateUnitInterval (1 % 2)
        , SL.truncateUnitInterval . (% 1000) <$> choose (0, 1000)
        ]
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> choose (0, 100)

instance Arbitrary (ShelleyKey 'AddressK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        return $ Shelley.unsafeGenerateKeyFromSeed mnemonic mempty

instance Arbitrary (ByronKey 'AddressK XPrv) where
    shrink _ = []
    arbitrary = do
        mnemonic <- arbitrary
        acctIx <- toEnum <$> arbitrary
        addrIx <- toEnum <$> arbitrary
        return $ Byron.unsafeGenerateKeyFromSeed (acctIx, addrIx) mnemonic mempty

instance (WalletKey k, Arbitrary (k 'AddressK XPrv)) => Arbitrary (k 'AddressK XPub)
  where
    shrink _ = []
    arbitrary = publicKey <$> arbitrary

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

genMnemonic
    :: forall mw ent csz.
     ( ConsistentEntropy ent mw csz
     , EntropySize mw ~ ent
     )
    => Gen (Mnemonic mw)
genMnemonic = do
        let n = fromIntegral (natVal $ Proxy @(EntropySize mw)) `div` 8
        bytes <- BS.pack <$> vector n
        let ent = unsafeMkEntropy @(EntropySize mw) bytes
        return $ entropyToMnemonic ent

instance Show XPrv where
    show _ = "<xprv>"

--
-- Helpers
--
--

base16 :: ByteString -> Text
base16 = T.decodeUtf8 . convertToBase Base16

bech32 :: ByteString -> Text
bech32 = Bech32.encodeLenient hrp . Bech32.dataPartFromBytes
  where hrp = [humanReadablePart|addr|]

base58 :: ByteString -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet
