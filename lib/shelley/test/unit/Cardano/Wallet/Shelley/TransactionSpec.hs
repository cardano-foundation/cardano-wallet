{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , hex
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
    , TxMetadataValue (..)
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
import Data.Maybe
    ( fromJust )
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
        [(1,16),(10,10),(20,5),(30,0)]
    estimateMaxInputsTests @ByronKey (Cardano.Testnet (Cardano.NetworkMagic 0))
        [(1,15),(10,10),(20,4),(30,0)]

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
                  , TxOut (dummyAddress 0) (Coin 5000000)
                  )
                ]
        let recipients = NE.fromList
                [ TxOut (dummyAddress 0) (Coin 4834720)
                ]

        let wdrl = Quantity 0

        let selectCoins = flip catchE (handleCannotCover utxo wdrl recipients) $ do
                (sel, utxo') <- withExceptT ErrSelectForPaymentCoinSelection $ do
                    CS.random testCoinSelOpts recipients wdrl utxo
                withExceptT ErrSelectForPaymentFee $
                    (Fee . CS.feeBalance) <$> adjustForFee testFeeOpts utxo' sel
        res <- runExceptT $ estimateFeeForCoinSelection selectCoins

        res `shouldBe` Right (FeeEstimation 165413 165413)

    describe "tx binary calculations - Shelley witnesses" $ do
        let slotNo = SlotNo 7750
            md = Nothing
            calculateBinary utxo outs pairs = toBase16 (Cardano.serialiseToCBOR ledgerTx)
              where
                  toBase16 = T.decodeUtf8 . hex
                  ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
                  addrWits = map (mkShelleyWitness unsigned) pairs
                  unsigned = mkUnsignedTx slotNo cs md mempty []
                  cs = mempty { CS.inputs = inps, CS.outputs = outs }
                  inps = Map.toList $ getUTxO utxo
        it "1 input, 2 outputs" $ do
            let pairs = [dummyWit 0]
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (Coin amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 1) (Coin amtOut)
                    , TxOut (dummyAddress 2) (Coin amtChange)
                    ]
            calculateBinary utxo outs pairs `shouldBe`
                "83a400818258200000000000000000000000000000000000000000000000000000\
                \000000000000000182825839010101010101010101010101010101010101010101\
                \010101010101010101010101010101010101010101010101010101010101010101\
                \0101011a001e848082583901020202020202020202020202020202020202020202\
                \020202020202020202020202020202020202020202020202020202020202020202\
                \02021a0078175c021a0001faa403191e46a1008182582001000000000000000000\
                \000000000000000000000000000000000000000000005840d7af60ae33d2af3514\
                \11c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9\
                \bd52076f2bb36bf38dfe034a9f04658e9f56197ab80ff6"

        it "2 inputs, 3 outputs" $ do
            let pairs = [dummyWit 0, dummyWit 1]
            let amtInp = 10000000
            let amtFee = 135200
            let amtOut = 6000000
            let amtChange = 2*amtInp - 2*amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (Coin amtInp)
                      )
                    , ( TxIn dummyTxId 1
                      , TxOut (dummyAddress 1) (Coin amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 2) (Coin amtOut)
                    , TxOut (dummyAddress 3) (Coin amtOut)
                    , TxOut (dummyAddress 4) (Coin amtChange)
                    ]
            calculateBinary utxo outs pairs `shouldBe`
                "83a400828258200000000000000000000000000000000000000000000000000000\
                \000000000000008258200000000000000000000000000000000000000000000000\
                \000000000000000000010183825839010202020202020202020202020202020202\
                \020202020202020202020202020202020202020202020202020202020202020202\
                \0202020202021a005b8d8082583901030303030303030303030303030303030303\
                \030303030303030303030303030303030303030303030303030303030303030303\
                \03030303031a005b8d808258390104040404040404040404040404040404040404\
                \040404040404040404040404040404040404040404040404040404040404040404\
                \040404041a007801e0021a0002102003191e46a10082825820130ae82201d7072e\
                \6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158405835ff78c6fc5e\
                \4466a179ca659fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b52ade49b\
                \19a7208fd63a6e67a19c406b4826608fdc5307025506c307825820010000000000\
                \00000000000000000000000000000000000000000000000000005840e8e769ecd0\
                \f3c538f0a5a574a1c881775f086d6f4c845b81be9b78955728bffa7efa54297c6a\
                \5d73337bd6280205b1759c13f79d4c93f29871fc51b78aeba80ef6"

newtype GivenNumOutputs = GivenNumOutputs Word8 deriving Num
newtype ExpectedNumInputs = ExpectedNumInputs Word8 deriving Num

-- | Set of tests related to `estimateMaxNumberOfInputs` from the transaction
-- layer.
estimateMaxInputsTests
    :: forall k. (TxWitnessTagFor k, Typeable k)
    => Cardano.NetworkId
    -> [(GivenNumOutputs, ExpectedNumInputs)]
    -> SpecWith ()
estimateMaxInputsTests net cases = do
    let k = show $ typeRep (Proxy @k)
    describe ("estimateMaxNumberOfInputs for "<>k<>" on "<>show net) $ do
        forM_ cases $ \(GivenNumOutputs nOuts, ExpectedNumInputs nInps) -> do
            let (o,i) = (show nOuts, show nInps)
            it ("order of magnitude, nOuts = " <> o <> " => nInps = " <> i) $
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
prop_decodeSignedByronTxRoundtrip (DecodeByronSetup utxo outs slotNo ntwrk pairs) = do
    let inps = Map.toList $ getUTxO utxo
    let cs = mempty { CS.inputs = inps, CS.outputs = outs }
    let unsigned = mkUnsignedTx slotNo cs Nothing mempty []
    let byronWits = zipWith (mkByronWitness' unsigned) inps pairs
    let ledgerTx = Cardano.makeSignedTransaction byronWits unsigned

    _decodeSignedTx (Cardano.serialiseToCBOR ledgerTx)
        === Right (sealShelleyTx ledgerTx)
  where
    mkByronWitness' unsigned (_, (TxOut addr _)) =
        mkByronWitness unsigned ntwrk addr

-- | Increasing the number of outputs reduces the number of inputs.
prop_moreOutputsMeansLessInputs
    :: forall k. TxWitnessTagFor k
    => Cardano.NetworkId
    -> Quantity "byte" Word16
    -> Word8
    -> Property
prop_moreOutputsMeansLessInputs net size nOuts
    = withMaxSuccess 1000
    $ within 300000
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
    $ within 300000
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
    $ within 300000
    $ size < maxBound `div` 2 ==>
        _estimateMaxNumberOfInputs @k net (Quantity size) Nothing nOuts
        <=
        _estimateMaxNumberOfInputs @k net (Quantity (size * 2)) Nothing nOuts

testCoinSelOpts :: CoinSelectionOptions ()
testCoinSelOpts = coinSelOpts testTxLayer (Quantity 4096) Nothing

testFeeOpts :: FeeOptions
testFeeOpts = feeOpts testTxLayer Nothing Nothing txParams (Coin 0) mempty
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
    arbitrary = TxMetadata <$> arbitrary
    shrink (TxMetadata md) = TxMetadata <$> shrink md

instance Arbitrary TxMetadataValue where
    -- Note: test generation at the integration level is very simple. More
    -- detailed metadata tests are done at unit level.
    arbitrary = TxMetaNumber <$> arbitrary

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

dummyAddress :: Word8 -> Address
dummyAddress b =
    Address $ BS.pack $ 1 : replicate 64 b

dummyWit :: Word8 -> (XPrv, Passphrase "encryption")
dummyWit b =
    (fromJust $ xprvFromBytes $ BS.pack $ replicate 96 b, mempty)

dummyTxId :: Hash "Tx"
dummyTxId = Hash $ BS.pack $ replicate 32 0
