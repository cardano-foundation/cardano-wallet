{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xprvFromBytes, xprvToBytes )
import Cardano.Wallet
    ( ErrSelectForPayment (..)
    , FeeEstimation (..)
    , coinSelOpts
    , estimateFeeForCoinSelection
    , feeOpts
    , handleCannotCover
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection, CoinSelectionOptions )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..), FeePolicy (..), adjustForFee )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxParameters (..)
    , UTxO (..)
    , txMetadataIsNull
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley, sealShelleyTx )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTagFor
    , mkByronWitness
    , mkShelleyWitness
    , mkUnsignedTx
    , newTransactionLayer
    , _decodeSignedTx
    , _estimateMaxNumberOfInputs
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.Trans.Except
    ( catchE, runExceptT, withExceptT )
import Data.Function
    ( on, (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word8 )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , classify
    , counterexample
    , elements
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , within
    , (===)
    , (==>)
    )

import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.MetaData as MD

spec :: Spec
spec = do
    describe "decodeSignedTx testing" $ do
        prop "roundtrip for Shelley witnesses" prop_decodeSignedShelleyTxRoundtrip
        prop "roundtrip for Byron witnesses" prop_decodeSignedByronTxRoundtrip

    estimateMaxInputsTests @ShelleyKey Cardano.Mainnet
        [(1,23),(10,16),(20,9),(30,2)]
    estimateMaxInputsTests @ShelleyKey (Cardano.Testnet (Cardano.NetworkMagic 0))
        [(1,23),(10,16),(20,9),(30,2)]

    estimateMaxInputsTests @ByronKey Cardano.Mainnet
        [(1,19),(10,12),(20,6),(30,0)]
    estimateMaxInputsTests @ByronKey (Cardano.Testnet (Cardano.NetworkMagic 0))
        [(1,18),(10,12),(20,5),(30,0)]

    estimateMaxInputsTests @IcarusKey Cardano.Mainnet
        [(1,19),(10,14),(20,9),(30,4)]
    estimateMaxInputsTests @IcarusKey (Cardano.Testnet (Cardano.NetworkMagic 0))
        [(1,18),(10,13),(20,8),(30,2)]

    describe "fee calculations" $ do
        let policy :: FeePolicy
            policy = LinearFee (Quantity 100_000) (Quantity 100) (Quantity 0)

            minFee :: Maybe TxMetadata -> CoinSelection -> Integer
            minFee md = fromIntegral . getFee . minimumFee tl policy Nothing md
              where tl = testTxLayer

        it "withdrawals incur fees" $ property $ \withdrawal ->
            let
                costWith = minFee Nothing (mempty { CS.withdrawal = withdrawal })
                costWithout = minFee Nothing mempty

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                (if withdrawal == 0
                    then property $ marginalCost == 0
                    else property $ marginalCost > 0
                ) & classify (withdrawal == 0) "null withdrawal"
                & counterexample ("marginal cost: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

        it "metadata incurs fees" $ property $ \md ->
            let
                costWith = minFee (Just md) mempty
                costWithout = minFee Nothing mempty

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                property (marginalCost > 0)
                & classify (txMetadataIsNull md) "null metadata"
                & counterexample ("cost of metadata: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

    it "regression #1740 - fee estimation at the boundaries" $ do
        let utxo = UTxO $ Map.fromList
                [ ( TxIn dummyTxId 0
                  , TxOut dummyAddress (Coin 5000000)
                  )
                ]
        let recipients = NE.fromList
                [ TxOut dummyAddress (Coin 4834720)
                ]

        let wdrl = Quantity 0

        let selectCoins = flip catchE (handleCannotCover utxo wdrl recipients) $ do
                (sel, utxo') <- withExceptT ErrSelectForPaymentCoinSelection $ do
                    CS.random testCoinSelOpts recipients wdrl utxo
                withExceptT ErrSelectForPaymentFee $
                    (Fee . CS.feeBalance) <$> adjustForFee testFeeOpts utxo' sel
        res <- runExceptT $ estimateFeeForCoinSelection selectCoins

        res `shouldBe` Right (FeeEstimation 165413 165413)

estimateMaxInputsTests
    :: forall k. (TxWitnessTagFor k, Typeable k)
    => Cardano.NetworkId
    -> [(Word8, Word8)]
    -> SpecWith ()
estimateMaxInputsTests net cases = do
    let k = show $ typeRep (Proxy @k)
    describe ("estimateMaxNumberOfInputs for "<>k<>" on "<>show net) $ do
        forM_ cases $ \(nOuts, nInps) -> do
            let (o,i) = (show nOuts, show nInps)
            it ("order of magnitude, nOuts = " <> o <> " → nInps = " <> i) $
                _estimateMaxNumberOfInputs @k net (Quantity 4096) Nothing nOuts
                    `shouldBe` nInps

        prop "more outputs ==> less inputs"
            (prop_moreOutputsMeansLessInputs @k net)
        prop "less outputs ==> more inputs"
            (prop_lessOutputsMeansMoreInputs @k net)
        prop "bigger size  ==> more inputs"
            (prop_biggerMaxSizeMeansMoreInputs @k net)

prop_decodeSignedShelleyTxRoundtrip
    :: DecodeShelleySetup
    -> Property
prop_decodeSignedShelleyTxRoundtrip (DecodeShelleySetup utxo outs md slotNo pairs) = do
    let inps = Map.toList $ getUTxO utxo
    let cs = mempty { CS.inputs = inps, CS.outputs = outs }
    let unsigned = mkUnsignedTx slotNo cs md mempty []
    let addrWits = map (mkShelleyWitness unsigned) pairs
    let wits = addrWits
    let ledgerTx = Cardano.makeSignedTransaction wits unsigned
    _decodeSignedTx (Cardano.serialiseToCBOR ledgerTx)
        === Right (sealShelleyTx ledgerTx)

prop_decodeSignedByronTxRoundtrip
    :: DecodeByronSetup
    -> Property
prop_decodeSignedByronTxRoundtrip (DecodeByronSetup utxo outs slotNo network pairs) = do
    let inps = Map.toList $ getUTxO utxo
    let cs = mempty { CS.inputs = inps, CS.outputs = outs }
    let unsigned = mkUnsignedTx slotNo cs Nothing mempty []
    let byronWits = zipWith (mkByronWitness' unsigned) inps pairs
    let ledgerTx = Cardano.makeSignedTransaction byronWits unsigned

    _decodeSignedTx (Cardano.serialiseToCBOR ledgerTx)
        === Right (sealShelleyTx ledgerTx)
  where
    mkByronWitness' unsigned (_, (TxOut addr _)) =
        mkByronWitness unsigned network addr

-- | Increasing the number of outputs reduces the number of inputs.
prop_moreOutputsMeansLessInputs
    :: forall k. TxWitnessTagFor k
    => Cardano.NetworkId
    -> Quantity "byte" Word16
    -> Word8
    -> Property
prop_moreOutputsMeansLessInputs net size nOuts
    = withMaxSuccess 1000
    $ within 100000
    $ nOuts < maxBound ==>
        _estimateMaxNumberOfInputs @k net size Nothing nOuts
        >=
        _estimateMaxNumberOfInputs @k net size Nothing (nOuts + 1)

-- | Reducing the number of outputs increases the number of inputs.
prop_lessOutputsMeansMoreInputs
    :: forall k. TxWitnessTagFor k
    => Cardano.NetworkId
    -> Quantity "byte" Word16
    -> Word8
    -> Property
prop_lessOutputsMeansMoreInputs net size nOuts
    = withMaxSuccess 1000
    $ within 100000
    $ nOuts > minBound ==>
        _estimateMaxNumberOfInputs @k net size Nothing (nOuts - 1)
        >=
        _estimateMaxNumberOfInputs @k net size Nothing nOuts

-- | Increasing the max size automatically increased the number of inputs
prop_biggerMaxSizeMeansMoreInputs
    :: forall k. TxWitnessTagFor k
    => Cardano.NetworkId
    -> Quantity "byte" Word16
    -> Word8
    -> Property
prop_biggerMaxSizeMeansMoreInputs net (Quantity size) nOuts
    = withMaxSuccess 1000
    $ within 100000
    $ size < maxBound `div` 2 ==>
        _estimateMaxNumberOfInputs @k net (Quantity size) Nothing nOuts
        <=
        _estimateMaxNumberOfInputs @k net (Quantity (size * 2)) Nothing nOuts

testCoinSelOpts :: CoinSelectionOptions ()
testCoinSelOpts = coinSelOpts testTxLayer (Quantity 4096) Nothing

testFeeOpts :: FeeOptions
testFeeOpts = feeOpts testTxLayer Nothing Nothing txParams (Coin 0)
  where
    txParams  = TxParameters feePolicy txMaxSize
    feePolicy = LinearFee (Quantity 155381) (Quantity 44) (Quantity 0)
    txMaxSize = Quantity maxBound

testTxLayer :: TransactionLayer (IO Shelley) ShelleyKey
testTxLayer = newTransactionLayer @ShelleyKey Cardano.Mainnet

data DecodeShelleySetup = DecodeShelleySetup
    { inputs :: UTxO
    , outputs :: [TxOut]
    , metadata :: Maybe TxMetadata
    , ttl :: SlotNo
    , keyPasswd :: [(XPrv, Passphrase "encryption")]
    } deriving Show

data DecodeByronSetup = DecodeByronSetup
    { inputs :: UTxO
    , outputs :: [TxOut]
    , ttl :: SlotNo
    , network :: Cardano.NetworkId
    , keyPasswd :: [(XPrv, Passphrase "encryption")]
    } deriving Show

instance Arbitrary DecodeShelleySetup where
    arbitrary = do
        utxo <- arbitrary
        n <- choose (1,10)
        outs <- vectorOf n arbitrary
        md <- arbitrary
        slot <- arbitrary
        let numInps = Map.size $ getUTxO utxo
        pairs <- vectorOf numInps arbitrary
        pure $ DecodeShelleySetup utxo outs md slot pairs

instance Arbitrary Cardano.NetworkId where
    arbitrary = elements
        [ Cardano.Mainnet
        , Cardano.Testnet $ Cardano.NetworkMagic 42
        ]

instance Arbitrary DecodeByronSetup where
    arbitrary = do
        utxo <- arbitrary
        n <- choose (1,10)
        outs <- vectorOf n arbitrary
        net <- arbitrary
        let numInps = Map.size $ getUTxO utxo
        slot <- arbitrary
        pairs <- vectorOf numInps arbitrary
        pure $ DecodeByronSetup utxo outs slot net pairs

instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> choose (1, 1000)

instance Arbitrary TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ TxIn txId ix

instance Arbitrary (Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ Hash $ BS.pack bs

instance Arbitrary Coin where
    arbitrary =
        Coin <$> choose (1, 200000)

instance Arbitrary TxOut where
    arbitrary = do
        let addr = Address $ BS.pack (1:replicate 64 0)
        TxOut addr <$> arbitrary

instance Arbitrary TxMetadata where
    arbitrary = TxMetadata . MD.MetaData <$> arbitrary
    shrink (TxMetadata (MD.MetaData md)) = TxMetadata . MD.MetaData <$> shrink md

instance Arbitrary MD.MetaDatum where
    arbitrary = MD.I <$> arbitrary

instance Arbitrary UTxO where
    arbitrary = do
        n <- choose (1,10)
        inps <- vectorOf n arbitrary
        let addr = Address $ BS.pack (1:replicate 64 0)
        coins <- vectorOf n arbitrary
        let outs = map (TxOut addr) coins
        pure $ UTxO $ Map.fromList $ zip inps outs

instance Arbitrary XPrv where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        let (Just xprv) = xprvFromBytes $ BS.pack $ take 96 bytes
        pure xprv

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . xprvToBytes

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

instance Arbitrary (Passphrase "raw") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "raw"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "raw"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "raw")

instance Arbitrary (Quantity "byte" Word16) where
    arbitrary = Quantity <$> choose (128, 2048)
    shrink (Quantity size)
        | size <= 1 = []
        | otherwise = Quantity <$> shrink size

dummyAddress :: Address
dummyAddress = Address $ BS.pack $ 1 : replicate 64 0

dummyTxId :: Hash "Tx"
dummyTxId = Hash $ BS.pack $ replicate 32 0
