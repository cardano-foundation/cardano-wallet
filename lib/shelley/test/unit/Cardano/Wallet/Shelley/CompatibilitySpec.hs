{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Shelley.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Crypto.Hash.Class
    ( digest )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , Mnemonic
    , SomeMnemonic (..)
    , entropyToMnemonic
    , mkMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , getRawKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
import Cardano.Wallet.Primitive.AddressDiscovery
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , Hash (..)
    , SlotId (..)
    , fromFlatSlot
    )
import Cardano.Wallet.Shelley.Compatibility
    ( ShelleyBlock
    , TPraosStandardCrypto
    , decentralizationLevelFromPParams
    , fromTip
    , invertUnitInterval
    , toPoint
    , toShelleyHash
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkEntropy )
import Control.Monad
    ( forM_ )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import GHC.TypeLits
    ( natVal )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( Crypto (..) )
import Ouroboros.Network.Block
    ( BlockNo (..), SlotNo (..), Tip (..), getTipPoint )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , checkCoverage
    , choose
    , cover
    , frequency
    , genericShrink
    , oneof
    , property
    , vector
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.PParams as SL

spec :: Spec
spec = do
    describe "Conversions" $
        it "toPoint' . fromTip' == getTipPoint" $ property $ \gh tip -> do
            let fromTip' = fromTip gh epochLength
            let toPoint' = toPoint gh epochLength
            toPoint' (fromTip' tip) === (getTipPoint tip)

    describe "Shelley Addresses" $ do
        it "(Mainnet) can be deserialised by shelley ledger spec" $
            property $ \(k::ShelleyKey 'AddressK XPrv) -> do
            let Address addr = paymentAddress @'Mainnet $ publicKey k
            case SL.deserialiseAddr @TPraosStandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        it "can deserialise golden faucet addresses" $ do
            let addr = unsafeFromHex
                    "6194986d1fc893629945058bdb0851478\
                    \fadc57711600cb1430799c95b52b2a3b7"
            case SL.deserialiseAddr @TPraosStandardCrypto addr of
                Just _ -> property True
                Nothing -> property False

        it "faucet golden address is ours" $ do
            let pwd = mempty
            let Right mw = SomeMnemonic <$> mkMnemonic @15
                    [ "day", "return", "logic", "bag", "explain", "wage"
                    , "pelican", "find", "coffee", "jar", "april", "permit"
                    , "ticket", "explain", "crime"
                    ]
            let rootK = unsafeGenerateKeyFromSeed (mw, Nothing) pwd
            let s = mkSeqStateFromRootXPrv (rootK, pwd) (toEnum 20)
            let addr = Address $ unsafeFromHex
                    "6194986d1fc893629945058bdb0851478\
                    \fadc57711600cb1430799c95b52b2a3b7"
            fst (isOurs addr s) `shouldBe` True

    describe "decentralizationLevelFromPParams" $ do

        let mkDecentralizationParam :: SL.UnitInterval -> SL.PParams
            mkDecentralizationParam i = SL.emptyPParams { SL._d = i }

        let testCases :: [(Rational, Text)]
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

instance Arbitrary (Tip ShelleyBlock) where
    arbitrary = frequency
        [ (10, return TipGenesis)
        , (90, arbitraryTip)
        ]
      where
        arbitraryTip = do
            n <- choose (0, 100)
            hash <- toShelleyHash
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
    arbitrary = ShelleyKey . getRawKey <$> genRootKeys

genRootKeys :: Gen (ShelleyKey 'RootK XPrv)
genRootKeys = do
    mnemonic <- arbitrary
    e <- genPassphrase @"encryption" (0, 16)
    return $ generateKeyFromSeed mnemonic e
  where
    genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
    genPassphrase range = do
        n <- choose range
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

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
