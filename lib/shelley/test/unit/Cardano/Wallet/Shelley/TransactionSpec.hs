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
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (Delegation, Payment)
    , Script
    , foldScript
    , serializeScript
    )
import Cardano.Wallet
    ( ErrSelectAssets (..), FeeEstimation (..), estimateFee )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.Gen
    ( genScript )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , fromHex
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
    ( SelectionError (..) )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionResult (..)
    , UnableToConstructChangeError (..)
    , emptySkeleton
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Types
    ( ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , ProtocolParameters (..)
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), coinToInteger, sumCoins )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId, TokenBundle, tokenName )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genFixedSizeTokenBundle
    , genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName), TokenPolicyId, unTokenName )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenPolicyId, shrinkTokenPolicyId )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , TxConstraints (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxOut (..)
    , TxSize (..)
    , txMetadataIsNull
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( computeTokenBundleSerializedLengthBytes
    , fromAllegraTx
    , fromAlonzoTx
    , fromMaryTx
    , fromShelleyTx
    , sealShelleyTx
    , toCardanoLovelace
    )
import Cardano.Wallet.Shelley.Transaction
    ( TxSkeleton (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor
    , estimateTxCost
    , estimateTxSize
    , mkByronWitness
    , mkShelleyWitness
    , mkTxSkeleton
    , mkUnsignedTx
    , newTransactionLayer
    , txConstraints
    , _calcScriptExecutionCost
    , _decodeSignedTx
    , _estimateMaxNumberOfInputs
    )
import Cardano.Wallet.Transaction
    ( TransactionCtx (..)
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
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Semigroup
    ( Sum (Sum), getSum, mtimesDefault )
import Data.Text
    ( Text )
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
    , Blind (..)
    , InfiniteList (..)
    , NonEmptyList (..)
    , Property
    , arbitraryPrintableChar
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , oneof
    , property
    , scale
    , vector
    , vectorOf
    , withMaxSuccess
    , within
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Random
    ( mkQCGen )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
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

    -- Note:
    --
    -- In the tests below, the expected numbers of inputs are highly sensitive
    -- to the size distribution of token bundles within generated transaction
    -- outputs.
    --
    -- If these tests fail unexpectedly, it's a good idea to check whether or
    -- not the distribution of generated token bundles has changed.
    --
    estimateMaxInputsTests @ShelleyKey
        [(1,114),(5,109),(10,103),(20,91),(50,51)]
    estimateMaxInputsTests @ByronKey
        [(1,73),(5,69),(10,65),(20,56),(50,27)]
    estimateMaxInputsTests @IcarusKey
        [(1,73),(5,69),(10,65),(20,56),(50,27)]

    describe "calculate fee execution costs" $ do
        let ppWithoutPrices :: ProtocolParameters
            ppWithoutPrices = dummyProtocolParameters
                { executionUnitPrices = Nothing
                }

        let ppWithPrices :: ProtocolParameters
            ppWithPrices = dummyProtocolParameters
                { executionUnitPrices = Just (ExecutionUnitPrices 1 1)
                }

        let unsafeFromCBORhex txt =
                let (Right bs) = fromHex $ T.encodeUtf8 txt
                in SealedTx bs

        forM_ matrixPlutusExamples $ \(title, cborHex, _) ->
            it title $ _calcScriptExecutionCost ppWithoutPrices (unsafeFromCBORhex cborHex) `shouldBe` Coin 0

        forM_ matrixPlutusExamples $ \(title, cborHex, executionUnits) -> do
            let calcPrice (ExecutionUnits {executionSteps, executionMemory}) =
                    Coin $ executionMemory + executionSteps
            let price = sumCoins $ map calcPrice executionUnits
            it title $ _calcScriptExecutionCost ppWithPrices (unsafeFromCBORhex cborHex) `shouldBe` price

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

            minFeeSkeleton :: TxSkeleton -> Integer
            minFeeSkeleton = coinToInteger . estimateTxCost pp

            estimateTxSize' :: TxSkeleton -> Integer
            estimateTxSize' = fromIntegral . unTxSize . estimateTxSize

        let (dummyAcct, dummyPath) =
                (RewardAccount mempty, DerivationIndex 0 :| [])

        it "withdrawals incur fees" $ property $ \wdrl ->
            let
                costWith =
                    minFee $ defaultTransactionCtx
                        { txWithdrawal = WithdrawalSelf dummyAcct dummyPath wdrl }
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

        it "minting incurs fees" $ property $ \assets ->
            let
                costWith =
                    minFeeSkeleton $ emptyTxSkeleton { txMintBurnAssets = assets }
                costWithout =
                    minFeeSkeleton emptyTxSkeleton

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                (if null assets
                    then property $ marginalCost == 0
                    else property $ marginalCost > 0
                )
                & classify (null assets) "null minting assets"
                & counterexample ("marginal cost: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

        it "scripts incur fees" $ property $ \scripts ->
            let
                costWith =
                    minFeeSkeleton $ emptyTxSkeleton { txScripts = scripts }
                costWithout =
                    minFeeSkeleton emptyTxSkeleton

                marginalCost :: Integer
                marginalCost = costWith - costWithout
            in
                (if null scripts
                    then property $ marginalCost == 0
                    else property $ marginalCost > 0
                )
                & classify (null scripts) "null scripts"
                & counterexample ("marginal cost: " <> show marginalCost)
                & counterexample ("cost with: " <> show costWith)
                & counterexample ("cost without: " <> show costWithout)

        it "increasing mint increases tx size at least proportianally to asset names"
            $ property $ \mints ->
            let
                assetNameLength = BS.length . unTokenName . tokenName

                lengthAssetNames = fromIntegral . getSum $
                    F.foldMap (Sum . assetNameLength) mints

                sizeWith =
                    estimateTxSize' $ emptyTxSkeleton { txMintBurnAssets = mints }
                sizeWithout =
                    estimateTxSize' emptyTxSkeleton

                marginalSize :: Integer
                marginalSize = sizeWith - sizeWithout
            in
                -- Larger asset names means more bytes in the tx which should
                -- mean a more expensive tx. Adding the mints should increase
                -- the marginal size at least as much as the size of the asset
                -- names.
                property (marginalSize >= lengthAssetNames)
                & classify (null mints) "null minting assets"
                & counterexample
                    ("asset names length: " <> show lengthAssetNames)
                & counterexample ("marginal size: " <> show marginalSize)
                & counterexample ("size with: " <> show sizeWith)
                & counterexample ("size without: " <> show sizeWithout)

        it "increasing scripts increases fee at least proportionate to size of CBOR script"
            $ property $ \scripts ->
            let
                -- Number of signatures required in the script
                numWitnesses = sum $ (foldScript (const (+ 1)) 0) <$> scripts
                sizeWitness  =    1 -- small array
                               + 34 -- vkey
                               + 66 -- signature

                -- Total size (in bytes) of the scripts when serialized
                scriptLengths = fromIntegral . getSum $
                    F.foldMap (Sum . BS.length . serializeScript ) scripts

                sizeWith =
                    estimateTxSize' $ emptyTxSkeleton { txScripts = scripts }
                sizeWithout =
                    estimateTxSize' emptyTxSkeleton

                marginalSize :: Integer
                marginalSize = sizeWith - sizeWithout
            in
                -- The entire script must be serialized when it is included in
                -- the transaction. Ensure that the marginal size increases at
                -- least as much as the size of the CBOR serialized scripts.
                --
                -- Additionally, each 'required signature' in the script means
                -- the tx will need to be witnessed by those vkeys (in the worst
                -- case).
                property
                  (marginalSize >= scriptLengths + numWitnesses * sizeWitness)
                & classify (null scripts) "no scripts"
                & classify (scriptLengths == 0) "zero script lengths"
                & classify (numWitnesses == 0) "no witnesses"
                & counterexample ("script lengths: " <> show scriptLengths)
                & counterexample
                    ("witness size: " <> show (numWitnesses * sizeWitness))
                & counterexample ("marginal size: " <> show marginalSize)
                & counterexample ("size with: " <> show sizeWith)
                & counterexample ("size without: " <> show sizeWithout)

    it "regression #1740 - fee estimation at the boundaries" $ do
        let requiredCost = Coin 166029
        let runSelection = except $ Left
                $ ErrSelectAssetsSelectionError
                $ SelectionBalanceError
                $ Balance.UnableToConstructChange
                $ Balance.UnableToConstructChangeError
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
                      , changeGenerated = chgs
                      , utxoRemaining = UTxOIndex.empty
                      }
                  inps = Map.toList $ unUTxO utxo
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
                    , changeGenerated = chgs
                    , utxoRemaining = UTxOIndex.empty
                    }
                  inps = Map.toList $ unUTxO utxo
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

    describe "Transaction constraints" $ do

        it "cost of empty transaction" $
            property prop_txConstraints_txBaseCost
        it "size of empty transaction" $
            property prop_txConstraints_txBaseSize
        it "cost of non-empty transaction" $
            property prop_txConstraints_txCost
        it "size of non-empty transaction" $
            property prop_txConstraints_txSize
        it "maximum size of output" $
            property prop_txConstraints_txOutputMaximumSize

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
                -- NOTE: These tests broke in the GHC 8.6 -> 8.10 bump,
                -- presumably due to some change in the arbitrary generation.
                -- It would be better if they weren't so fragile.
                --
                -- They also broke when bumping to lts-18.4.
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
    let inps = Map.toList $ unUTxO utxo
    let cs = mkSelection inps
    let fee = toCardanoLovelace $ selectionDelta txOutCoin cs
    let Right unsigned = mkUnsignedTx shelleyEra slotNo cs md mempty [] fee
    let addrWits = map (mkShelleyWitness unsigned) pairs
    let wits = addrWits
    let ledgerTx = Cardano.makeSignedTransaction wits unsigned
    let expected = case shelleyEra of
            Cardano.ShelleyBasedEraShelley -> Right $ sealShelleyTx fromShelleyTx ledgerTx
            Cardano.ShelleyBasedEraAllegra -> Right $ sealShelleyTx fromAllegraTx ledgerTx
            Cardano.ShelleyBasedEraMary    -> Right $ sealShelleyTx fromMaryTx ledgerTx
            Cardano.ShelleyBasedEraAlonzo  -> Right $ sealShelleyTx fromAlonzoTx ledgerTx


    _decodeSignedTx anyEra (Cardano.serialiseToCBOR ledgerTx) === expected
  where
    mkSelection inps = SelectionResult
        { inputsSelected = NE.fromList inps
        , extraCoinSource = Nothing
        , outputsCovered = []
        , changeGenerated = outs
        , utxoRemaining = UTxOIndex.empty
        }

prop_decodeSignedByronTxRoundtrip
    :: DecodeByronSetup
    -> Property
prop_decodeSignedByronTxRoundtrip (DecodeByronSetup utxo outs slotNo ntwrk pairs) = do
    let era = Cardano.AnyCardanoEra Cardano.AllegraEra
    let shelleyEra = Cardano.ShelleyBasedEraAllegra
    let inps = Map.toList $ unUTxO utxo
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
        , changeGenerated = outs
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
        let numInps = Map.size $ unUTxO utxo
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
        let numInps = Map.size $ unUTxO utxo
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
    arbitrary = genCoinPositive
    shrink = shrinkCoinPositive

instance Arbitrary TxOut where
    arbitrary = TxOut addr <$> scale (`mod` 4) genTokenBundleSmallRange
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
    , getTokenBundleMaxSize =
        error "dummyTxParameters: getMaxTokenBundleSize"
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
    , eras =
        error "dummyProtocolParameters: eras"
    , maximumCollateralInputCount =
        error "dummyProtocolParameters: maximumCollateralInputCount"
    , executionUnitPrices =
        error "dummyProtocolParameters: executionUnitPrices"
    }

-- | Like generate, but the random generate is fixed to a particular seed so
-- that it generates always the same values.
generatePure :: Int -> Gen a -> a
generatePure seed (MkGen r) = r (mkQCGen seed) 30

--------------------------------------------------------------------------------
-- Transaction constraints
--------------------------------------------------------------------------------

emptyTxSkeleton :: TxSkeleton
emptyTxSkeleton = mkTxSkeleton
    TxWitnessShelleyUTxO
    defaultTransactionCtx
    emptySkeleton

mockProtocolParameters :: ProtocolParameters
mockProtocolParameters = dummyProtocolParameters
    { txParameters = TxParameters
        { getFeePolicy = LinearFee (Quantity 1.0) (Quantity 2.0)
        , getTxMaxSize = Quantity 16384
        , getTokenBundleMaxSize = TokenBundleMaxSize $ TxSize 4000
        }
    }

mockTxConstraints :: TxConstraints
mockTxConstraints = txConstraints mockProtocolParameters TxWitnessShelleyUTxO

data MockSelection = MockSelection
    { txInputCount :: Int
    , txOutputs :: [TxOut]
    , txRewardWithdrawal :: Coin
    }
    deriving (Eq, Show)

genMockSelection :: Gen MockSelection
genMockSelection = do
    txInputCount <-
        oneof [ pure 0, choose (1, 1000) ]
    txOutputCount <-
        oneof [ pure 0, choose (1, 1000) ]
    txOutputs <- replicateM txOutputCount genTxOut
    txRewardWithdrawal <-
        Coin <$> oneof [ pure 0, choose (1, 1_000_000) ]
    pure MockSelection
        { txInputCount
        , txOutputs
        , txRewardWithdrawal
        }
  where
    genTxOut = TxOut (dummyAddress dummyByte) <$> genTokenBundleSmallRange
      where
        dummyByte :: Word8
        dummyByte = fromIntegral $ fromEnum 'A'

shrinkMockSelection :: MockSelection -> [MockSelection]
shrinkMockSelection mock =
    [ MockSelection i o r
    | (i, o, r) <- shrink (txInputCount, txOutputs, txRewardWithdrawal)
    ]
  where
    MockSelection
        { txInputCount
        , txOutputs
        , txRewardWithdrawal
        } = mock

instance Arbitrary MockSelection where
    arbitrary = genMockSelection
    shrink = shrinkMockSelection

-- Tests that using 'txBaseCost' to estimate the cost of an empty selection
-- produces a result that is consistent with the result of using
-- 'estimateTxCost'.
--
prop_txConstraints_txBaseCost :: Property
prop_txConstraints_txBaseCost =
    txBaseCost mockTxConstraints
        === estimateTxCost mockProtocolParameters emptyTxSkeleton

-- Tests that using 'txBaseSize' to estimate the size of an empty selection
-- produces a result that is consistent with the result of using
-- 'estimateTxSize'.
--
prop_txConstraints_txBaseSize :: Property
prop_txConstraints_txBaseSize =
    txBaseSize mockTxConstraints
        === estimateTxSize emptyTxSkeleton

-- Tests that using 'txConstraints' to estimate the cost of a non-empty
-- selection produces a result that is consistent with the result of using
-- 'estimateTxCost'.
--
prop_txConstraints_txCost :: MockSelection -> Property
prop_txConstraints_txCost mock =
    counterexample ("result: " <> show result) $
    counterexample ("lower bound: " <> show lowerBound) $
    counterexample ("upper bound: " <> show upperBound) $
    conjoin
        [ result >= lowerBound
        , result <= upperBound
        ]
  where
    MockSelection {txInputCount, txOutputs, txRewardWithdrawal} = mock
    result :: Coin
    result = mconcat
        [ txBaseCost mockTxConstraints
        , txInputCount `mtimesDefault` txInputCost mockTxConstraints
        , F.foldMap (txOutputCost mockTxConstraints . tokens) txOutputs
        , txRewardWithdrawalCost mockTxConstraints txRewardWithdrawal
        ]
    lowerBound = estimateTxCost mockProtocolParameters emptyTxSkeleton
        {txInputCount, txOutputs, txRewardWithdrawal}
    -- We allow a small amount of overestimation due to the slight variation in
    -- the marginal cost of an input:
    upperBound = lowerBound <> txInputCount `mtimesDefault` Coin 8

-- Tests that using 'txConstraints' to estimate the size of a non-empty
-- selection produces a result that is consistent with the result of using
-- 'estimateTxSize'.
--
prop_txConstraints_txSize :: MockSelection -> Property
prop_txConstraints_txSize mock =
    counterexample ("result: " <> show result) $
    counterexample ("lower bound: " <> show lowerBound) $
    counterexample ("upper bound: " <> show upperBound) $
    conjoin
        [ result >= lowerBound
        , result <= upperBound
        ]
  where
    MockSelection {txInputCount, txOutputs, txRewardWithdrawal} = mock
    result :: TxSize
    result = mconcat
        [ txBaseSize mockTxConstraints
        , txInputCount `mtimesDefault` txInputSize mockTxConstraints
        , F.foldMap (txOutputSize mockTxConstraints . tokens) txOutputs
        , txRewardWithdrawalSize mockTxConstraints txRewardWithdrawal
        ]
    lowerBound = estimateTxSize emptyTxSkeleton
        {txInputCount, txOutputs, txRewardWithdrawal}
    -- We allow a small amount of overestimation due to the slight variation in
    -- the marginal size of an input:
    upperBound = lowerBound <> txInputCount `mtimesDefault` TxSize 4

newtype Large a = Large { unLarge :: a }
    deriving (Eq, Show)

instance Arbitrary (Large TokenBundle) where
    arbitrary = fmap Large . genFixedSizeTokenBundle =<< choose (1, 128)

-- Tests that if a bundle is oversized (when serialized), then a comparison
-- between 'txOutputSize' and 'txOutputMaximumSize' should also indicate that
-- the bundle is oversized.
--
prop_txConstraints_txOutputMaximumSize :: Blind (Large TokenBundle) -> Property
prop_txConstraints_txOutputMaximumSize (Blind (Large bundle)) =
    checkCoverage $
    cover 10 (authenticComparison == LT)
        "authentic bundle size is smaller than maximum" $
    cover 10 (authenticComparison == GT)
        "authentic bundle size is greater than maximum" $
    counterexample
        ("authentic size: " <> show authenticSize) $
    counterexample
        ("authentic size maximum: " <> show authenticSizeMax) $
    counterexample
        ("authentic comparison: " <> show authenticComparison) $
    counterexample
        ("simulated size: " <> show simulatedSize) $
    counterexample
        ("simulated size maximum: " <> show simulatedSizeMax) $
    counterexample
        ("simulated comparison: " <> show simulatedComparison) $
    case authenticComparison of
        LT ->
            -- We can't really require anything of the simulated comparison
            -- here, as the size given by 'estimateTxSize' is allowed to be
            -- an overestimate.
            property True
        EQ ->
            -- It's extremely hard to hit this case exactly. But if this case
            -- does match, we only need to ensure that the simulated size is
            -- not an underestimate.
            simulatedComparison =/= LT
        GT ->
            -- This is the case we really care about. If the result of an
            -- authentic comparison indicates that the bundle is oversized,
            -- the simulated comparison MUST also indicate that the bundle
            -- is oversized.
            simulatedComparison === GT
  where
    authenticComparison = compare authenticSize authenticSizeMax
    simulatedComparison = compare simulatedSize simulatedSizeMax

    authenticSize :: TxSize
    authenticSize = computeTokenBundleSerializedLengthBytes bundle

    authenticSizeMax :: TxSize
    authenticSizeMax = unTokenBundleMaxSize maryTokenBundleMaxSize

    simulatedSize :: TxSize
    simulatedSize = txOutputSize mockTxConstraints bundle
    simulatedSizeMax :: TxSize
    simulatedSizeMax = txOutputMaximumSize mockTxConstraints

instance Arbitrary AssetId where
    arbitrary =
        TokenBundle.AssetId
        <$> arbitrary
        -- In the calculation of the size of the Tx, the minting of assets
        -- increases the size of the Tx by both a constant factor per asset
        -- plus a variable factor (the size of the asset name). In a typical
        -- setting, the constant factor dominantes (it's about 40 bytes per
        -- asset, whereas the size of an asset name has a maximum of 32 bytes).
        -- So we create a generator here that forces the variable factor to
        -- dominate so we can test the sanity of the estimation algorithm.
        <*> (UnsafeTokenName . BS.pack <$> vector 128)

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyId
    shrink = shrinkTokenPolicyId

instance Arbitrary (Script KeyHash) where
    arbitrary = do
        keyHashes <- vectorOf 10 arbitrary
        genScript keyHashes

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- oneof [pure Payment, pure Delegation]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary

matrixPlutusExamples :: [(String, Text, [ExecutionUnits])]
matrixPlutusExamples =
    [( "pubkey-2.json"
     , "84a60081825820888963613d2bb4c5c55cee335f724624cbc54b185ecaa2fb1eb07545ed5db421010d80018002000e800b58201dd28c3485f707dd5fb5db3ef96c5ed723c05ce5ef5439b0cc291f8a8ac6531ca30381591b6f591b6c010000332332233322232323232332232333222333222333333332222222233223333322222333222333322223322332233223322332233322233223322332233223232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323230011200123355002307812001307812001112222223335306433300433503a00600333503a00500230070012088012350810149821c048c8c8c8c8c8c8cccd5ca99b8935573e600a2400290001180109000918038900084380919826180109000980189000918039aba230031200123043357446ae8cc008480048d55d018010900091ba900323507b4988c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8cccd5ca99b8935573e602a24002900011801090009180b8900084b009199999999982e980109000980189000980209000980289000980309000980389000980409000980489000980509000980589000919a822980b890009aba23013120012335044301e1200135744602224002466a086604e240026ae88c03c480048cd4108c09848004d5d1180689000919a8209817890009aba2300b120012335040335504130321200130311200135744601224002466a072606c240026ae88c01c480048cd40f8c0f448004d5d1180289000919a81e99aa81f1822890009824090009aba23003120012304f357446ae8cc008480048d5d198010900091aba330021200123574660042400246ae8cc008480048d5d198010900091aba330021200123574660042400246ae8cc008480048d55d018010900091ba900323507a4988ccd41d800c0180088c8c8c8c8c8c8cccd5ca99b8935573e600a2400290001180109000918038900084200919826980109000980189000918229aba23003120012300d357446ae8cc008480048d55d018010900091ba90032350784988d4c11400488cdd22400066aed0c010008cd5da18068009bb24988d4c18400488cdd22400066aed0c010008cd5da1ba7001376493119ba448000cd5da1ba800137649311999999abaf2323232323232323333572a66e24d55cf980389000a400046004240024a100021040246666aaed494200048c00c48004c01848004208048cccd55da9283f91801890009802090008408091999aabb523003120012507e35746600624002100024666a6a0f6600e6ae88c00c4800488ccd4d41f4c048d5d1180209000911a84080998260020011283f840809283e83f91aba3300212001235574060042400246ea400c941e0941e0941e0941e00041e88ccccccd5d79191919191919191999ab95337126aae7cc01c48005200023002120012507f08101233335576a4a0fe4600624002600c240021020246666aaed4941f88c00c48004c01048004200048cccd55da91801890009283e9aba330031200107f233353507a30073574460062400244666a6a0f860526ae88c0104800488d420004cc1a0010008941f820004941f01f88d5d198010900091aaba0300212001237520064a0ee4a0ee4a0ee4a0ee0020f246666666aebc8c8c8c8c8c8cccd5ca99b8935573e600a24002900011801090009283e03f11999aabb52507c230031200130041200107e233335576a46006240024a0f66ae8cc00c480041f48ccd4d41e0c0d8d5d1180109000911a83e0011283d03e11aaba0300212001237520064a0ec4a0ec4a0ec4a0ec0020f04666a0dc00a010004464646464646464646666ae54cdc49aab9f300712001480008c008480048c024480041f88ccc124c00848004c00c48004c010480048c024d5d1180289000919a8161808090009aba23003120012335008303312001357446ae8cc008480048d5d198010900091aaba03002120012375200646a0e09311919191919191999ab95337126aae7cc0144800520002300212001230071200107b23304d3002120013003120012303b35744600624002466a00c6036240026ae88d5d198010900091aaba03002120012375200646a0de930911919191919191999ab953371260082400290001180109000918020900083d91a83c980109000919a8168039aba235574060082400246666ae54cdc4980109000a40044a0ee4600a240020f246aae7cc008480048dd480191a837a4c46a607a00244466e912000335768600a00666aed0cd403cc02848004008cd5da19a8031817090008009bb24988d4c10c00488cdd22400066aed0c060008cd5da19a802180b090008009bb2498488ccd4d41b800488cdd22400066aed0cd40a4010008dd924c466e91200237649303911999999abaf23232323232323232323333572a66e24d55cf980489000a400046004240024a0f00f446666aaed4941e08c00c48004c020480041e88cccd55da9283b918018900098030900083c91999aabb5250762300312001300412001078233335576a46006240024a0ea6ae8cc00c480041dc8ccd4d41c8c020d5d118020900091199a9a83a19a8099806090009aba23005120012233353507633500d30321200135744600c24002446a0f466608e00c0080044a0f00f44a0ec0f04a0e80ec46ae8cc008480048d5d198010900091aaba0300212001237520064a0dc4a0dc4a0dc4a0dc0020e046666666aebc8c8c8c8c8c8c8c8cccd5ca99b8935573e600e24002900011801090009283a83b91999aabb5250752300312001300612001077233335576a4a0e846006240026008240020ec46666aaed48c00c48004941ccd5d198018900083a9199a9a838180d1aba23003120012233353507233500930181200135744600824002446a0ec660940080044a0e80ec4a0e40e846ae8cc008480048d55d018010900091ba90032506d2506d2506d2506d00106f1223333333575e46464646464646464646666ae54cdc4980289000a400046004240024600a240020f246666aaed4941dc8c00c48004c01c480041e48cccd55da91801890009283b1aba3300612001078233353507333502e00a35744600a24002446a0ee6a0ee0044a0ea0ee46666ae54cdc4980109000a400446008240024a0e80ec46aae7cc010480048cccd55da91802890009283918010900083a11aaba03002120012375200846a0dea0dc4a0da4a0da4a0da4a0da0020de466aa03c601424002600424002466aa006600424002601a240024666a0c404804aeb44488ccd4188cd5400c008004cd54014008004cd5401c0080044488d400ccd5406c008004488c8c8dd318008019a80090008918009aa83591199a9a81c00091bb2498888cd5da19a812004001980280103608911a80199aa80c80100089119191999999abaf25067250672300237560084a0ce4a0ce0060d26a00240022460026aa0d244646666aaed48c008480048ccd4d41a0cd408c01cd5d100191199a9a83518031aba3005223506e33503d0040022506c06e2506a06c00206b235069503911223501633550170020012333505b01d01e75a4666a0b4004006044466666666a60920024466e912002335768600e0046ec92622233748900219abb430080033357686ea0008dd924c4466e912000335768600e0046ec92623374890051bb24988cdd2240186ec92622233748900319abb4375000666aed0dd40011bb2498888cdd22401066aed0dd400199abb4374e0046ec92606223333333575e4646464646464646464646464646464646464646464646464646464646464646464646666ae54cdc4980e89000a4018460042400246008240021080246666aaed48c00c480049420804c08448004210048d4204041b48cccd5ca99b89301b12001480288c00c480048c01448004208048cccd55da91801890009283f980f090008408091a83f03591999ab953371260302400290041180189000918040900083f91999aabb52507c2300312001301b1200107e233335576a4a0f646006240026008240020fa46666aaed48c00c48004941e8d5d198018900083e1199a9a83b981a9aba230181200122333535079302635744600824002446a0fa660ce0080044a0f60fa4a0f20f646ae8cc05c480048cccd5ca99b89301212001480188c00c480048c020480041e48cccd55da9283b1180189000980a8900083c11999aabb5250752300312001300412001077233335576a46006240024a0e86ae8cc00c480041d88ccd4d41c4c0bcd5d118090900091199a9a83998189aba2300412001223507733062004002250750772507307523574660222400246666ae54cdc4980609000a4008460062400246010240020e646666aaed4941c08c00c48004c03c480041c88cccd55da92837918018900098020900083891999aabb523003120012506e357466006240020e04666a6a0d660286ae88c0304800488ccd4d41b4c0acd5d1180209000911a83899830002001128378389283683791aba3300b1200123333572a66e24c0184800520022300312001230061200106d233335576a4a0d446006240026012240020d846666aaed48c00c48004941a4d5d19804090008359199a9a83318079aba2300712001223506a305a0022506806a23333572a66e24c008480052000230041200125067069235573e600c2400246666aaed4941948c00c48004c0104800419c8cccd55da9180189000928321aba33003120010662333535061300a35744600424002446a0ca60a60044a0c60ca46aae80c008480048dd48019282f9282f9282f9282f8008309199a82b804004bac2333505600200401f2335304d001233748900019abb4300300137649311119ba448008cd5da1ba70033357686e9c008cd5da1ba7001376493119a9827800919ba448000cd5da1ba8001376493119ba448008cd5da1ba800137649311999999abaf232323232323232323232323232323333572a66e24c0244800520022300212001230091200106b233335576a4a0d24600624002601a240020d646666aaed4941a08c00c48004c018480041a88cccd55da92833918018900098020900083491999aabb5230031200125066357466006240020d04666a6a0c660206ae88c0244800488ccd4d4194c048d5d118028900091199a9a833980a1aba2300612001223506b33305d0060040022506906b250670692506506723574660042400246ae8cc01c480048cccd5ca99b89300212001480008c01048004941881908d55cf98030900091999aabb5250602300312001300412001062233335576a46006240024a0be6ae8cc00c480041848ccd4d4170c018d5d1180109000911a83018298011282f03011aaba0300212001237520064a0b44a0b44a0b44a0b40020b846666666aebc8c8c8c8c8c8c8c8c8c8c8cccd5ca99b89300512001480088c008480048c014480041988cccd55da92832118018900098048900083311999aabb5230031200125063357466010240020ca4666a6a0c0603c6ae88c01c4800488d4190c164008941881908cccd5ca99b89300212001480008c010480049418418c8d55cf98030900091999aabb52505f2300312001300412001061233335576a46006240024a0bc6ae8cc00c480041808ccd4d416cc064d5d1180109000911a82f982a8011282e82f91aaba0300212001237520064a0b24a0b24a0b24a0b20020b64666a0a2004006eb08dd380091999999abaf25056250562505623505737580044a0ac0020b02446464646464646666ae54cdc49aab9f300512001480008c008480048c01c480041788cd40c8c00848004c00c480048cd402001cd5d1180189000919a8040031aba23574660042400246aae80c008480048dd480191a82924c2446464646464646666ae54cdc49aab9f300512001480008c008480048c01c480041748cd40d4c00848004c00c480048cd402401cd5d1180189000918049aba23574660042400246aae80c008480048dd480191a828a4c2446464646464646666ae54cdc49aab9f300512001480008c008480048c01c480041708cd40c8c00848004c00c480048cd402001cd5d1180189000918041aba23574660042400246aae80c008480048dd480191a82824c244646464646464646666ae54cdc4980289000a40084a06e46004240020b846666ae54cdc4980289000a400446006240024600a240020b846a06e600424002466a01a00e6ae88d55d018020900091999ab953371260042400290001281a918028900082c91aab9f3002120012375200646a09e9311919191999ab95337126004240029001101b118010900082b11999ab95337126004240029000101a918020900082b11aab9f375200646a09a931199a8248058063ad1223232300137560066a00240022460026aa0a8446666aaed4940908cd408ccd4024018d5d100118019aba300200105511223335048335500500200133550070020013355003002001112223232323232323333572a66e24d55cf980289000a400046004240024600e240020ae466aa04e600424002600624002466a0120106ae88c00c480048cd4020018d5d11aba3300212001235574060042400246ea400c8d412d26123535044001222001112223535501e0012233748900019abb433500600500233576866a00c0080026ec926123535042001222003112223333333575e4646464646464646666ae54cdc49aab9f300712001480008c008480049414c1548cccd55da92829918018900098030900082a91999aabb5250522300312001300412001054233335576a46006240024a0a26ae8cc00c4800414c8ccd4d4138cd4024020d5d118018900091199a9a82819a8058049aba230041200122350543355026004002250520542505005223574660042400246aae80c008480048dd480192825928259282592825800826891a9a8200009110011199a820001001bad2375000246666666aebc941149411494114941148d4118dd68010008239199a81ea8012822001890009000919191919191919191919191919191999ab95337126014240029003118010900091802090008289181a980109000918079aba235574060182400246666ae54cdc4980409000a400846006240024600a2400209e460606004240024601a6ae88d55d018048900091999ab9533712600a2400290011180189000918028900082611816180109000918061aba2355740600c2400246666ae54cdc4980109000a400046008240024600e2400209246aae7cc010480048c0a8c008480048dd69aba235574060042400246ea400c8d40f5262323232323232323232323232323232323232323232323232323333572a66e24c05448005200c2046230021200105b23333572a66e24c05448005200a2047230031200105b23333572a66e24c0504800520082300312001230071200105a2330413002120013003120012375a6ae88c00c480048dd61aba23574660042400246aae80c04c480048cccd5ca99b89300f12001480188c00c480048c01c480041548cc0f4c00848004c00c480048dd69aba23003120012375a6ae88d5d198010900091aaba0300e1200123333572a66e24c0284800520042300312001230071200105023303c3002120013003120012300e3574460062400246eb4d5d11aba3300212001235574060122400246666ae54cdc4980289000a400446006240024600a2400209646070600424002460126ae88d55d018030900091999ab953371260042400290001180209000918038900082411aab9f3004120012303230021200123005357446aae80c008480048dd480191a81e24c46464646464646464646464646666ae54cdc4980409000a40044600424002460102400209a466607a60042400260062400260082400246eb0d5d118028900091bac3574460062400246eb0d5d11aba330021200123574660042400246aae80c018480048cccd5ca99b89300212001480008c010480048c01c4800411c8d55cf9802090009181b180109000918029aba235574060042400246ea400c8d40ed26232323232323232323333572a66e24c010480052002230021200123004120010482303b3002120012375a6ae88d55d018030900091999ab953371260042400290001180209000918038900082311aab9f300412001230383002120012375a6ae88d55d018010900091ba900323503a4988c8c8c8c8c8c8cccd5ca99b8935573e600a24002900011801090009180389000822919815980109000980189000918039aba2300312001237586ae88d5d198010900091aaba03002120012375200646a0729311919191999ab95337126aae7cc008480052000230021200123004120010412375a6ae88d55d018010900091ba90032350384988848cc00400c0088004888888888848cccccccccc00402c02802402001c01801401000c00880048848cc00400c008800488848ccc00401000c00880044488008488488cc00401000c48004448848cc00400c0084480048848cc00400c008800448848cc00400c0084800448848cc00400c0084800448848cc00400c00848004484888c00c0104488800844888004480044880084880048004848888c010014848888c00c014848888c008014848888c00401480048848cc00400c0088004848888888c01c0208848888888cc018024020848888888c014020488888880104888888800c8848888888cc0080240208848888888cc00402402080048488c00800c888488ccc00401401000c80048488c00800c8488c00400c80048ccd400c01801c010488848ccc00401000c008480048c8cccd5ca99b8935573e6ea40092000200b230021200100a2350034984988d4c018004cdd2240006ec92623333333575e46464646666ae54cdc49aab9f300212001480008c00848004940200288cccd55da9180209000928041aaba030021200100a2375200846a00c0104a0084a0084a0084a00800200c2424460040062244002240022400240022246460020024466006646002002004002664666444666444666666664444444466446666644444666444666644446644664466446664446644664466644466446644664466446466446464664464646464646400244446600a6020002008446a60200044444444444666a6a04e666aa601e24002a01a4666a606a666ae54cdc800600081b81b11a815801128150098019101c101b009099a804a800a80d891199a9a80d8011100210010031a80090008918009aa81e910891911199a9a80b80091a805802911199803090009802801999aa98048900080380280100491a804801090009000891a9a80180091000891a9a8010009100109109198008018010900091a980100091001110919800801801100091111111110919999999998008058050048040038030028020018011000910919800801801100091109199800802001801100088910010910911980080200189000889109198008018010890009109198008018011000890911801001889100089000891091980080180109000891091980080180109000891091980080180109000890911180180208911001089110008900089100109100090009091111802002909111180180290911118010029091111800802900091091980080180110009091111111803804110911111119803004804109111111180280409111111002091111110019109111111198010048041109111111198008048041000909118010019110911998008028020019000909118010019091180080190008891918008009119801991800800801000a451c35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a300010481d879800581840000d8798082191a221a019d6038f5f6"
     , [ExecutionUnits { executionMemory = 6690, executionSteps = 27091000 }]
     )
    ]
