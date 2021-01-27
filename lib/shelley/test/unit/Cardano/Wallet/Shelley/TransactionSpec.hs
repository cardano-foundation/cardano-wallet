{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( ErrSelectAssets (..), FeeEstimation (..), estimateFee )
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
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionError (..)
    , SelectionResult (..)
    , UnableToConstructChangeError (..)
    , emptySkeleton
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..), ProtocolParameters (..), TxParameters (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), coinToInteger )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinLargePositive, shrinkCoinLargePositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxOut (..)
    , txMetadataIsNull
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( fromAllegraTx, fromShelleyTx, sealShelleyTx, toCardanoLovelace )
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
    ( ErrDecodeSignedTx (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.Trans.Except
    ( except, runExceptT )
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
    ( Word16, Word64, Word8 )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , NonEmptyList (..)
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
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Random
    ( mkQCGen )

import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
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
        prop "roundtrip for Shelley witnesses" $
            prop_decodeSignedShelleyTxRoundtrip Cardano.ShelleyBasedEraShelley
        prop "roundtrip for Shelley witnesses Allegra" $
            prop_decodeSignedShelleyTxRoundtrip Cardano.ShelleyBasedEraAllegra
        prop "roundtrip for Byron witnesses" prop_decodeSignedByronTxRoundtrip

    estimateMaxInputsTests @ShelleyKey
        [(1,114),(5,104),(10,99),(20,75),(50,34)]
    estimateMaxInputsTests @ByronKey
        [(1,73),(5,66),(10,62),(20,45),(50,16)]
    estimateMaxInputsTests @IcarusKey
        [(1,73),(5,66),(10,62),(20,45),(50,16)]

    describe "fee calculations" $ do
        let pp :: ProtocolParameters
            pp = dummyProtocolParameters
                { txParameters = dummyTxParameters
                    { getFeePolicy = LinearFee (Quantity 100_000) (Quantity 100)
                    }
                }

            minFee :: TransactionCtx -> Integer
            minFee ctx = coinToInteger $ calcMinimumCost testTxLayer pp ctx sel
              where sel = emptySkeleton

        it "withdrawals incur fees" $ property $ \wdrl ->
            let
                costWith =
                    minFee $ defaultTransactionCtx { txWithdrawal = WithdrawalSelf wdrl }
                costWithout =
                    minFee defaultTransactionCtx

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                (if wdrl == Coin 0
                    then property $ marginalCost == 0
                    else property $ marginalCost > 0
                ) & classify (wdrl == Coin 0) "null withdrawal"
                & counterexample ("marginal cost: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

        it "metadata incurs fees" $ property $ \md ->
            let
                costWith =
                    minFee $ defaultTransactionCtx { txMetadata = Just md }
                costWithout =
                    minFee defaultTransactionCtx

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                property (marginalCost > 0)
                & classify (txMetadataIsNull md) "null metadata"
                & counterexample ("cost of metadata: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

    it "regression #1740 - fee estimation at the boundaries" $ do
        let requiredCost = Coin 166029
        let runSelection = except $ Left
                $ ErrSelectAssetsSelectionError
                $ UnableToConstructChange
                $ UnableToConstructChangeError
                    { requiredCost
                    , shortfall = Coin 100000
                    }
        result <- runExceptT (estimateFee runSelection)
        result `shouldBe`
            Right (FeeEstimation (unCoin requiredCost) (unCoin requiredCost))

    -- fixme: it would be nice to repeat the tests for multiple eras
    let era = Cardano.ShelleyBasedEraAllegra

    describe "tx binary calculations - Byron witnesses - mainnet" $ do
        let slotNo = SlotNo 7750
            md = Nothing
            calculateBinary utxo outs chgs pairs =
                toBase16 (Cardano.serialiseToCBOR ledgerTx)
              where
                  toBase16 = T.decodeUtf8 . hex
                  ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
                  mkByronWitness' unsignedTx (_, (TxOut addr _)) =
                      mkByronWitness unsignedTx Cardano.Mainnet addr
                  addrWits = zipWith (mkByronWitness' unsigned) inps pairs
                  fee = toCardanoLovelace $ selectionDelta txOutCoin cs
                  Right unsigned = mkUnsignedTx era slotNo cs md mempty [] fee
                  cs = SelectionResult
                      { inputsSelected = NE.fromList inps
                      , extraCoinSource = Nothing
                      , outputsCovered = outs
                      , changeGenerated = NE.fromList chgs
                      , utxoRemaining = UTxOIndex.empty
                      }
                  inps = Map.toList $ getUTxO utxo
        it "1 input, 2 outputs" $ do
            let pairs = [dummyWit 0]
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (coinToBundle amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 1) (coinToBundle amtOut)
                    ]
            let chgs =
                    [ TxOut (dummyAddress 2) (coinToBundle amtChange)
                    ]
            calculateBinary utxo outs chgs pairs `shouldBe`
                "83a40081825820000000000000000000000000000000000000000000000000\
                \00000000000000000001828258390101010101010101010101010101010101\
                \01010101010101010101010101010101010101010101010101010101010101\
                \0101010101010101011a001e84808258390102020202020202020202020202\
                \02020202020202020202020202020202020202020202020202020202020202\
                \0202020202020202020202021a0078175c021a0001faa403191e46a1028184\
                \58200100000000000000000000000000000000000000000000000000000000\
                \0000005840d7af60ae33d2af351411c1445c79590526990bfa73cbb3732b54\
                \ef322daa142e6884023410f8be3c16e9bd52076f2bb36bf38dfe034a9f0465\
                \8e9f56197ab80f582000000000000000000000000000000000000000000000\
                \0000000000000000000041a0f6"

        it "2 inputs, 3 outputs" $ do
            let pairs = [dummyWit 0, dummyWit 1]
            let amtInp = 10000000
            let amtFee = 135200
            let amtOut = 6000000
            let amtChange = 2*amtInp - 2*amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (coinToBundle amtInp)
                      )
                    , ( TxIn dummyTxId 1
                      , TxOut (dummyAddress 1) (coinToBundle amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 2) (coinToBundle amtOut)
                    , TxOut (dummyAddress 3) (coinToBundle amtOut)
                    ]
            let chgs =
                    [ TxOut (dummyAddress 4) (coinToBundle amtChange)
                    ]
            calculateBinary utxo outs chgs pairs `shouldBe`
                "83a40082825820000000000000000000000000000000000000000000000000\
                \00000000000000000082582000000000000000000000000000000000000000\
                \00000000000000000000000000010183825839010202020202020202020202\
                \02020202020202020202020202020202020202020202020202020202020202\
                \02020202020202020202020202021a005b8d80825839010303030303030303\
                \03030303030303030303030303030303030303030303030303030303030303\
                \03030303030303030303030303030303031a005b8d80825839010404040404\
                \04040404040404040404040404040404040404040404040404040404040404\
                \04040404040404040404040404040404040404041a007801e0021a00021020\
                \03191e46a10282845820010000000000000000000000000000000000000000\
                \00000000000000000000005840e8e769ecd0f3c538f0a5a574a1c881775f08\
                \6d6f4c845b81be9b78955728bffa7efa54297c6a5d73337bd6280205b1759c\
                \13f79d4c93f29871fc51b78aeba80e58200000000000000000000000000000\
                \00000000000000000000000000000000000041a0845820130ae82201d7072e\
                \6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158405835ff78c6\
                \fc5e4466a179ca659fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b\
                \52ade49b19a7208fd63a6e67a19c406b4826608fdc5307025506c307582001\
                \01010101010101010101010101010101010101010101010101010101010101\
                \41a0f6"

    describe "tx binary calculations - Byron witnesses - testnet" $ do
        let slotNo = SlotNo 7750
            md = Nothing
            calculateBinary utxo outs chgs pairs =
                toBase16 (Cardano.serialiseToCBOR ledgerTx)
              where
                  toBase16 = T.decodeUtf8 . hex
                  ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
                  net = Cardano.Testnet (Cardano.NetworkMagic 0)
                  mkByronWitness' unsignedTx (_, (TxOut addr _)) =
                      mkByronWitness unsignedTx net addr
                  addrWits = zipWith (mkByronWitness' unsigned) inps pairs
                  fee = toCardanoLovelace $ selectionDelta txOutCoin cs
                  Right unsigned = mkUnsignedTx era slotNo cs md mempty [] fee
                  cs = SelectionResult
                    { inputsSelected = NE.fromList inps
                    , extraCoinSource = Nothing
                    , outputsCovered = outs
                    , changeGenerated = NE.fromList chgs
                    , utxoRemaining = UTxOIndex.empty
                    }
                  inps = Map.toList $ getUTxO utxo
        it "1 input, 2 outputs" $ do
            let pairs = [dummyWit 0]
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (coinToBundle amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 1) (coinToBundle amtOut)
                    ]
            let chgs =
                    [ TxOut (dummyAddress 2) (coinToBundle amtChange)
                    ]
            calculateBinary utxo outs chgs pairs `shouldBe`
                "83a40081825820000000000000000000000000000000000000000000000000\
                \00000000000000000001828258390101010101010101010101010101010101\
                \01010101010101010101010101010101010101010101010101010101010101\
                \0101010101010101011a001e84808258390102020202020202020202020202\
                \02020202020202020202020202020202020202020202020202020202020202\
                \0202020202020202020202021a0078175c021a0001faa403191e46a1028184\
                \58200100000000000000000000000000000000000000000000000000000000\
                \0000005840d7af60ae33d2af351411c1445c79590526990bfa73cbb3732b54\
                \ef322daa142e6884023410f8be3c16e9bd52076f2bb36bf38dfe034a9f0465\
                \8e9f56197ab80f582000000000000000000000000000000000000000000000\
                \0000000000000000000044a1024100f6"

        it "2 inputs, 3 outputs" $ do
            let pairs = [dummyWit 0, dummyWit 1]
            let amtInp = 10000000
            let amtFee = 135200
            let amtOut = 6000000
            let amtChange = 2*amtInp - 2*amtOut - amtFee
            let utxo = UTxO $ Map.fromList
                    [ ( TxIn dummyTxId 0
                      , TxOut (dummyAddress 0) (coinToBundle amtInp)
                      )
                    , ( TxIn dummyTxId 1
                      , TxOut (dummyAddress 1) (coinToBundle amtInp)
                      )
                    ]
            let outs =
                    [ TxOut (dummyAddress 2) (coinToBundle amtOut)
                    , TxOut (dummyAddress 3) (coinToBundle amtOut)
                    ]
            let chgs =
                    [ TxOut (dummyAddress 4) (coinToBundle amtChange)
                    ]
            calculateBinary utxo outs chgs pairs `shouldBe`
                "83a40082825820000000000000000000000000000000000000000000000000\
                \00000000000000000082582000000000000000000000000000000000000000\
                \00000000000000000000000000010183825839010202020202020202020202\
                \02020202020202020202020202020202020202020202020202020202020202\
                \02020202020202020202020202021a005b8d80825839010303030303030303\
                \03030303030303030303030303030303030303030303030303030303030303\
                \03030303030303030303030303030303031a005b8d80825839010404040404\
                \04040404040404040404040404040404040404040404040404040404040404\
                \04040404040404040404040404040404040404041a007801e0021a00021020\
                \03191e46a10282845820130ae82201d7072e6fbfc0a1884fb54636554d1494\
                \5b799125cf7ce38d477f5158405835ff78c6fc5e4466a179ca659fa85c99b8\
                \a3fba083f3f3f42ba360d479c64ef169914b52ade49b19a7208fd63a6e67a1\
                \9c406b4826608fdc5307025506c30758200101010101010101010101010101\
                \01010101010101010101010101010101010144a10241008458200100000000\
                \0000000000000000000000000000000000000000000000000000005840e8e7\
                \69ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b78955728bffa7e\
                \fa54297c6a5d73337bd6280205b1759c13f79d4c93f29871fc51b78aeba80e\
                \58200000000000000000000000000000000000000000000000000000000000\
                \00000044a1024100f6"

newtype GivenNumOutputs = GivenNumOutputs Int deriving Num
newtype ExpectedNumInputs = ExpectedNumInputs Int deriving Num

-- | Set of tests related to `estimateMaxNumberOfInputs` from the transaction
-- layer.
estimateMaxInputsTests
    :: forall k. (TxWitnessTagFor k, Typeable k)
    => [(GivenNumOutputs, ExpectedNumInputs)]
    -> SpecWith ()
estimateMaxInputsTests cases = do
    let k = show $ typeRep (Proxy @k)
    describe ("estimateMaxNumberOfInputs for "<>k) $ do
        forM_ cases $ \(GivenNumOutputs nOuts, ExpectedNumInputs nInps) -> do
            let (o,i) = (show nOuts, show nInps)
            it ("order of magnitude, nOuts = " <> o <> " => nInps = " <> i) $ do
                let outs = [ generatePure r arbitrary | r <- [ 1 .. nOuts ] ]
                length outs `shouldBe` nOuts
                _estimateMaxNumberOfInputs @k (Quantity 16384) defaultTransactionCtx outs
                    `shouldBe` nInps

        prop "more outputs ==> less inputs"
            (prop_moreOutputsMeansLessInputs @k)
        prop "bigger size  ==> more inputs"
            (prop_biggerMaxSizeMeansMoreInputs @k)

prop_decodeSignedShelleyTxRoundtrip
    :: forall era. (Cardano.IsCardanoEra era, Cardano.IsShelleyBasedEra era)
    => Cardano.ShelleyBasedEra era
    -> DecodeShelleySetup
    -> Property
prop_decodeSignedShelleyTxRoundtrip shelleyEra (DecodeShelleySetup utxo outs md slotNo pairs) = do
    let anyEra = Cardano.anyCardanoEra (Cardano.cardanoEra @era)
    let inps = Map.toList $ getUTxO utxo
    let cs = mkSelection inps
    let fee = toCardanoLovelace $ selectionDelta txOutCoin cs
    let Right unsigned = mkUnsignedTx shelleyEra slotNo cs md mempty [] fee
    let addrWits = map (mkShelleyWitness unsigned) pairs
    let wits = addrWits
    let ledgerTx = Cardano.makeSignedTransaction wits unsigned
    let expected = case shelleyEra of
            Cardano.ShelleyBasedEraShelley -> Right $ sealShelleyTx fromShelleyTx ledgerTx
            Cardano.ShelleyBasedEraAllegra -> Right $ sealShelleyTx fromAllegraTx ledgerTx
            Cardano.ShelleyBasedEraMary    -> Left ErrDecodeSignedTxNotSupported

    _decodeSignedTx anyEra (Cardano.serialiseToCBOR ledgerTx) === expected
  where
    mkSelection inps = SelectionResult
        { inputsSelected = NE.fromList inps
        , extraCoinSource = Nothing
        , outputsCovered = []
        , changeGenerated = NE.fromList outs
        , utxoRemaining = UTxOIndex.empty
        }

prop_decodeSignedByronTxRoundtrip
    :: DecodeByronSetup
    -> Property
prop_decodeSignedByronTxRoundtrip (DecodeByronSetup utxo outs slotNo ntwrk pairs) = do
    let era = Cardano.AnyCardanoEra Cardano.AllegraEra
    let shelleyEra = Cardano.ShelleyBasedEraAllegra
    let inps = Map.toList $ getUTxO utxo
    let cs = mkSelection inps
    let fee = toCardanoLovelace $ selectionDelta txOutCoin cs
    let Right unsigned = mkUnsignedTx shelleyEra slotNo cs Nothing mempty [] fee
    let byronWits = zipWith (mkByronWitness' unsigned) inps pairs
    let ledgerTx = Cardano.makeSignedTransaction byronWits unsigned

    _decodeSignedTx era (Cardano.serialiseToCBOR ledgerTx)
        === Right (sealShelleyTx fromAllegraTx ledgerTx)
  where
    mkByronWitness' unsigned (_, (TxOut addr _)) =
        mkByronWitness unsigned ntwrk addr
    mkSelection inps = SelectionResult
        { inputsSelected = NE.fromList inps
        , extraCoinSource = Nothing
        , outputsCovered = []
        , changeGenerated = NE.fromList outs
        , utxoRemaining = UTxOIndex.empty
        }

-- | Increasing the number of outputs reduces the number of inputs.
prop_moreOutputsMeansLessInputs
    :: forall k. TxWitnessTagFor k
    => Quantity "byte" Word16
    -> NonEmptyList TxOut
    -> Property
prop_moreOutputsMeansLessInputs size (NonEmpty xs)
    = withMaxSuccess 1000
    $ within 300000
    $ _estimateMaxNumberOfInputs @k size defaultTransactionCtx (tail xs)
      >=
      _estimateMaxNumberOfInputs @k size defaultTransactionCtx xs

-- | Increasing the max size automatically increased the number of inputs
prop_biggerMaxSizeMeansMoreInputs
    :: forall k. TxWitnessTagFor k
    => Quantity "byte" Word16
    -> [TxOut]
    -> Property
prop_biggerMaxSizeMeansMoreInputs size outs
    = withMaxSuccess 1000
    $ within 300000
    $ getQuantity size < maxBound `div` 2 ==>
        _estimateMaxNumberOfInputs @k size defaultTransactionCtx outs
        <=
        _estimateMaxNumberOfInputs @k ((*2) <$> size ) defaultTransactionCtx outs

testTxLayer :: TransactionLayer ShelleyKey
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

-- Coins (quantities of lovelace) must be strictly positive when included in
-- transactions.
--
instance Arbitrary Coin where
    arbitrary = genCoinLargePositive
    shrink = shrinkCoinLargePositive

instance Arbitrary TxOut where
    arbitrary = TxOut addr <$> genTokenBundleSmallRange
      where
        addr = Address $ BS.pack (1:replicate 64 0)

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

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

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin

dummyWit :: Word8 -> (XPrv, Passphrase "encryption")
dummyWit b =
    (fromJust $ xprvFromBytes $ BS.pack $ replicate 96 b, mempty)

dummyTxId :: Hash "Tx"
dummyTxId = Hash $ BS.pack $ replicate 32 0

dummyTxParameters :: TxParameters
dummyTxParameters = TxParameters
    { getFeePolicy =
        error "dummyTxParameters: getFeePolicy"
    , getTxMaxSize =
        error "dummyTxParameters: getTxMaxSize"
    }

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters = ProtocolParameters
    { decentralizationLevel =
        error "dummyProtocolParameters: decentralizationLevel"
    , txParameters =
        error "dummyProtocolParameters: txParameters"
    , desiredNumberOfStakePools =
        error "dummyProtocolParameters: desiredNumberOfStakePools"
    , minimumUTxOvalue =
        error "dummyProtocolParameters: minimumUTxOvalue"
    , stakeKeyDeposit =
        error "dummyProtocolParameters: stakeKeyDeposit"
    , hardforkEpochNo =
        error "dummyProtocolParameters: hardforkEpochNo"
    }

-- | Like generate, but the random generate is fixed to a particular seed so
-- that it generates always the same values.
generatePure :: Int -> Gen a -> a
generatePure seed (MkGen r) = r (mkQCGen seed) 30
