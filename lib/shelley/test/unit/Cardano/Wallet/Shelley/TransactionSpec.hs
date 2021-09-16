{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , CardanoEraStyle (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    , IsShelleyBasedEra (..)
    , ShelleyBasedEra (..)
    , cardanoEraStyle
    )
import Cardano.Wallet
    ( ErrSelectAssets (..)
    , ErrUpdateSealedTx (..)
    , FeeEstimation (..)
    , estimateFee
    )
import Cardano.Wallet.Api.Types
    ( ApiBalanceTransactionPostData (..), ApiT (..) )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.Gen
    ( genScript )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
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
    ( SelectionError (..), SelectionOf (..), selectionDelta )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( UnableToConstructChangeError (..), emptySkeleton )
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
    , cardanoTx
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano'
    , serialisedTx
    , txMetadataIsNull
    , txOutCoin
    , unsafeSealedTxFromBytes
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( AnyShelleyBasedEra (..)
    , computeTokenBundleSerializedLengthBytes
    , getShelleyBasedEra
    , shelleyToCardanoEra
    , toCardanoLovelace
    )
import Cardano.Wallet.Shelley.Transaction
    ( ExtraTxBodyContent (..)
    , TxSkeleton (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor
    , estimateTxCost
    , estimateTxSize
    , mkShelleyWitness
    , mkTxSkeleton
    , mkUnsignedTx
    , newTransactionLayer
    , noExtraTxBodyContent
    , txConstraints
    , updateSealedTx
    , _calcScriptExecutionCost
    , _decodeSealedTx
    , _estimateMaxNumberOfInputs
    )
import Cardano.Wallet.Transaction
    ( TransactionCtx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.Trans.Except
    ( except, runExceptT )
import Data.Aeson
    ( eitherDecode )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( on, (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
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
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( (+||), (||+) )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec
    , SpecWith
    , before_
    , describe
    , expectationFailure
    , it
    , pendingWith
    , shouldBe
    , xdescribe
    )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
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
    , frequency
    , oneof
    , property
    , scale
    , suchThatMap
    , vector
    , vectorOf
    , withMaxSuccess
    , within
    , (.||.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Gen
    ( Gen (..), listOf1 )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.QuickCheck.Random
    ( mkQCGen )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Pretty
    ( Pretty (..), (====) )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    decodeSealedTxSpec
    estimateMaxInputsSpec
    feeCalculationSpec
    feeEstimationRegressionSpec
    forAllEras binaryCalculationsSpec
    transactionConstraintsSpec
    updateSealedTxSpec

forAllEras :: (AnyCardanoEra -> Spec) -> Spec
forAllEras eraSpec = do
    eraSpec (AnyCardanoEra ByronEra)
    forAllShelleyBasedEras eraSpec

forAllShelleyBasedEras :: (AnyCardanoEra -> Spec) -> Spec
forAllShelleyBasedEras eraSpec = do
    eraSpec (AnyCardanoEra ShelleyEra)
    eraSpec (AnyCardanoEra AllegraEra)
    eraSpec (AnyCardanoEra MaryEra)
    eraSpec (AnyCardanoEra AlonzoEra)

allEras :: [(Int, AnyCardanoEra)]
allEras =
    [ (1, AnyCardanoEra ByronEra)
    , (2, AnyCardanoEra ShelleyEra)
    , (3, AnyCardanoEra AllegraEra)
    , (4, AnyCardanoEra MaryEra)
    , (5, AnyCardanoEra AlonzoEra)
    ]

eraNum :: AnyCardanoEra -> Int
eraNum e = fst $ head $ filter ((== e) . snd) allEras

shelleyEraNum :: AnyShelleyBasedEra -> Int
shelleyEraNum = eraNum . shelleyToCardanoEra

pendingOnAlonzo :: String -> ShelleyBasedEra era -> SpecWith a -> SpecWith a
pendingOnAlonzo msg era = before_ $ case era of
    Cardano.ShelleyBasedEraAlonzo -> pendingWith ("AlonzoEra: " ++ msg)
    _ -> pure ()

instance Arbitrary AnyCardanoEra where
    arbitrary = frequency $ zip [1..] $ map (pure . snd) allEras
    -- Shrink by choosing a *later* era
    shrink e = map snd $ filter ((> eraNum e) . fst) allEras

instance Arbitrary AnyShelleyBasedEra where
    arbitrary = suchThatMap (getShelleyBasedEra <$> arbitrary) id
    -- shrink = _fixme

decodeSealedTxSpec :: Spec
decodeSealedTxSpec = describe "SealedTx serialisation/deserialisation" $ do
    prop "roundtrip for Shelley witnesses" prop_sealedTxShelleyRoundtrip
    xdescribe "Not implemented yet" $ do -- TODO: [ADP-919]
        prop "roundtrip for Byron witnesses" prop_sealedTxByronRoundtrip

-- Note:
--
-- In the tests below, the expected numbers of inputs are highly sensitive
-- to the size distribution of token bundles within generated transaction
-- outputs.
--
-- If these tests fail unexpectedly, it's a good idea to check whether or
-- not the distribution of generated token bundles has changed.
--
estimateMaxInputsSpec :: Spec
estimateMaxInputsSpec = do
    estimateMaxInputsTests @ShelleyKey
        [(1,114),(5,109),(10,103),(20,91),(50,51)]
    estimateMaxInputsTests @ByronKey
        [(1,73),(5,69),(10,65),(20,56),(50,27)]
    estimateMaxInputsTests @IcarusKey
        [(1,73),(5,69),(10,65),(20,56),(50,27)]

feeCalculationSpec :: Spec
feeCalculationSpec = describe "fee calculations" $ do
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

    describe "calculate fee execution costs" $ do
        let ppWithPrices :: ProtocolParameters
            ppWithPrices = dummyProtocolParameters
                { executionUnitPrices = Just (ExecutionUnitPrices 1 1)
                }

        let unsafeFromCBORhex = unsafeSealedTxFromBytes . unsafeFromHex

        describe "with prices txs without plutus scripts" $ do
            forM_ matrixNormalTxExamples $ \(title, cborHex) ->
                it title $
                    _calcScriptExecutionCost ppWithPrices (unsafeFromCBORhex cborHex)
                    `shouldBe` Coin 0

        describe "with prices txs with plutus scripts" $ do
            let testPlutusDir = $(getTestData) </> "plutus"

            forM_ matrixPlutusExamples $ \(json, executionUnits) -> do
                let calcPrice (ExecutionUnits {executionSteps, executionMemory}) =
                        Coin $ executionMemory + executionSteps
                let price = sumCoins $ map calcPrice executionUnits
                let testFile = testPlutusDir </> json
                it json $ property $ \(_thereWillBeWalletsHere :: Int) -> monadicIO $ do
                    bs <- run $ BL.readFile testFile
                    let (Right content@(ApiBalanceTransactionPostData (ApiT sealedTx) _ _)) =
                            eitherDecode @(ApiBalanceTransactionPostData 'Mainnet) bs
                    monitor $ counterexample ("json = " <> json <> " " <> show content)
                    assert (_calcScriptExecutionCost ppWithPrices sealedTx == price)

    describe "fee calculations" $ do
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

  where
    pp :: ProtocolParameters
    pp = dummyProtocolParameters
        { txParameters = dummyTxParameters
            { getFeePolicy = fp
            }
        }
    fp = LinearFee (Quantity 100_000) (Quantity 100)

    minFee :: TransactionCtx -> Integer
    minFee ctx = coinToInteger $ calcMinimumCost testTxLayer pp ctx sel
      where sel = emptySkeleton

    minFeeSkeleton :: TxSkeleton -> Integer
    minFeeSkeleton = coinToInteger . estimateTxCost pp

    estimateTxSize' :: TxSkeleton -> Integer
    estimateTxSize' = fromIntegral . unTxSize . estimateTxSize

    (dummyAcct, dummyPath) =
        (RewardAccount mempty, DerivationIndex 0 :| [])

feeEstimationRegressionSpec :: Spec
feeEstimationRegressionSpec = describe "Regression tests" $ do
    it "#1740 Fee estimation at the boundaries" $ do
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

binaryCalculationsSpec :: AnyCardanoEra -> Spec
binaryCalculationsSpec (AnyCardanoEra era) =
    case cardanoEraStyle era of
        LegacyByronEra -> pure ()
        ShelleyBasedEra shelleyEra ->
            -- TODO: [ADP-919] tests for byron witnesses
            pendingOnAlonzo "Golden transactions not yet updated" shelleyEra $
            before_ (pendingWith ("Will return with signTx PR")) $
            binaryCalculationsSpec' shelleyEra

binaryCalculationsSpec' :: IsShelleyBasedEra era => ShelleyBasedEra era -> Spec
binaryCalculationsSpec' era = describe ("calculateBinary - "+||era||+"") $ do
    describe "Byron witnesses - mainnet" $ do
        let net = Cardano.Mainnet
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
            calculateBinary net utxo outs chgs pairs `shouldBe`
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
            calculateBinary net utxo outs chgs pairs `shouldBe`
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

    describe "Byron witnesses - testnet" $ do
        let net = Cardano.Testnet (Cardano.NetworkMagic 0)
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
            calculateBinary net utxo outs chgs pairs `shouldBe`
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
            calculateBinary net utxo outs chgs pairs `shouldBe`
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

  where
    slotNo = SlotNo 7750
    md = Nothing
    calculateBinary _net utxo outs chgs pairs =
        hex (Cardano.serialiseToCBOR ledgerTx)
      where
          ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
          mkByronWitness' _unsignedTx (_, (TxOut _addr _)) =
              error "mkByronWitness'" -- TODO: [ADP-919]
          addrWits = zipWith (mkByronWitness' unsigned) inps pairs
          fee = toCardanoLovelace $ selectionDelta txOutCoin cs
          Right unsigned = mkUnsignedTx era slotNo cs md mempty [] fee
          cs = Selection
            { inputs = NE.fromList inps
            , collateral = []
            , extraCoinSource = Coin 0
            , extraCoinSink = Coin 0
            , outputs = outs
            , change = chgs
            , assetsToMint = mempty
            , assetsToBurn = mempty
            }
          inps = Map.toList $ unUTxO utxo

transactionConstraintsSpec :: Spec
transactionConstraintsSpec = describe "Transaction constraints" $ do
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

--------------------------------------------------------------------------------
-- Roundtrip tests for SealedTx

prop_sealedTxShelleyRoundtrip
    :: AnyShelleyBasedEra
    -> AnyCardanoEra
    -> Pretty DecodeSetup
    -> Property
prop_sealedTxShelleyRoundtrip txEra@(AnyShelleyBasedEra era) currentEra (Pretty tc) = conjoin
    [ txBytes ==== serialisedTx sealedTxC
    , either (\e -> counterexample (show e) False) (compareOnCBOR tx) sealedTxB
    ]
    .||. encodingFromTheFuture txEra currentEra
  where
    tx = makeShelleyTx era tc
    txBytes = Cardano.serialiseToCBOR tx
    sealedTxC = sealedTxFromCardano' tx
    sealedTxB = sealedTxFromBytes' currentEra txBytes

makeShelleyTx :: IsShelleyBasedEra era => ShelleyBasedEra era -> DecodeSetup -> Cardano.Tx era
makeShelleyTx era testCase = Cardano.makeSignedTransaction addrWits unsigned
  where
    DecodeSetup utxo outs md slotNo pairs _netwk = testCase
    inps = Map.toList $ unUTxO utxo
    fee = toCardanoLovelace $ selectionDelta txOutCoin cs
    Right unsigned = mkUnsignedTx era slotNo cs md mempty [] fee
    addrWits = map (mkShelleyWitness unsigned) pairs
    cs = Selection
        { inputs = NE.fromList inps
        , collateral = []
        , extraCoinSource = Coin 0
        , extraCoinSink = Coin 0
        , outputs = []
        , change = outs
        -- TODO: [ADP-346]
        , assetsToMint = TokenMap.empty
        , assetsToBurn = TokenMap.empty
        }

prop_sealedTxByronRoundtrip
    :: AnyShelleyBasedEra
    -> AnyCardanoEra
    -> Pretty (ForByron DecodeSetup)
    -> Property
prop_sealedTxByronRoundtrip txEra@(AnyShelleyBasedEra era) currentEra (Pretty tc) = conjoin
    [ txBytes ==== serialisedTx sealedTxC
    , either (\e -> counterexample (show e) False) (compareOnCBOR tx) sealedTxB
    ]
    .||. encodingFromTheFuture txEra currentEra
  where
    tx = makeByronTx era tc
    txBytes = Cardano.serialiseToCBOR tx
    sealedTxC = sealedTxFromCardano' tx
    sealedTxB = sealedTxFromBytes' currentEra txBytes

makeByronTx :: IsShelleyBasedEra era => ShelleyBasedEra era -> ForByron DecodeSetup -> Cardano.Tx era
makeByronTx era testCase = Cardano.makeSignedTransaction byronWits unsigned
  where
    ForByron (DecodeSetup utxo outs _ slotNo pairs _ntwrk) = testCase
    inps = Map.toList $ unUTxO utxo
    fee = toCardanoLovelace $ selectionDelta txOutCoin cs
    Right unsigned = mkUnsignedTx era slotNo cs Nothing mempty [] fee
    -- byronWits = map (mkByronWitness unsigned ntwrk Nothing) pairs
    byronWits = map (error "makeByronTx: broken") pairs  -- TODO: [ADP-919]
    cs = Selection
        { inputs = NE.fromList inps
        , collateral = []
        , extraCoinSource = Coin 0
        , extraCoinSink = Coin 0
        , outputs = []
        , change = outs
        -- TODO: [ADP-346]
        , assetsToMint = TokenMap.empty
        , assetsToBurn = TokenMap.empty
        }

encodingFromTheFuture :: AnyShelleyBasedEra -> AnyCardanoEra -> Bool
encodingFromTheFuture tx current = shelleyEraNum tx > eraNum current

compareOnCBOR :: IsCardanoEra era => Cardano.Tx era -> SealedTx -> Property
compareOnCBOR b sealed = case cardanoTx sealed of
    InAnyCardanoEra _ a ->
        Cardano.serialiseToCBOR a ==== Cardano.serialiseToCBOR b

--------------------------------------------------------------------------------
--

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

testTxLayer :: TransactionLayer ShelleyKey SealedTx
testTxLayer = newTransactionLayer @ShelleyKey Cardano.Mainnet

newtype ForByron a = ForByron { getForByron :: a } deriving (Show, Eq)

data DecodeSetup = DecodeSetup
    { inputs :: UTxO
    , outputs :: [TxOut] -- TODO: add datums
    , metadata :: Maybe TxMetadata
    , ttl :: SlotNo
    , keyPasswd :: [(XPrv, Passphrase "encryption")]
    , network :: Cardano.NetworkId
    } deriving Show

instance Arbitrary DecodeSetup where
    arbitrary = do
        utxo <- arbitrary
        DecodeSetup utxo
            <$> listOf1 arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> vectorOf (Map.size $ unUTxO utxo) arbitrary
            <*> arbitrary

    shrink (DecodeSetup i o m t k n) =
        [ DecodeSetup i' o' m' t' k' n'
        | (i',o',m',t',k',n') <- shrink (i,o,m,t,k,n) ]

instance Arbitrary (ForByron DecodeSetup) where
    arbitrary = do
        test <- arbitrary
        pure $ ForByron (test { metadata = Nothing })

instance Arbitrary Cardano.NetworkId where
    arbitrary = elements
        [ Cardano.Mainnet
        , Cardano.Testnet $ Cardano.NetworkMagic 42
        ]

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
    arbitrary = fromJust . xprvFromBytes . BS.pack <$> vectorOf 96 arbitrary

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

mockFeePolicy :: FeePolicy
mockFeePolicy = LinearFee (Quantity 1.0) (Quantity 2.0)

mockProtocolParameters :: ProtocolParameters
mockProtocolParameters = dummyProtocolParameters
    { txParameters = TxParameters
        { getFeePolicy = mockFeePolicy
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

matrixPlutusExamples :: [(String, [ExecutionUnits])]
matrixPlutusExamples =
    [ ( "auction_1-2.json"
      , [ExecutionUnits { executionMemory = 6070, executionSteps = 24673000 }]
      )
    , ( "crowdfunding-success-4.json"
      , [ ExecutionUnits { executionMemory = 7740, executionSteps = 31186000 }
        , ExecutionUnits { executionMemory = 7740, executionSteps = 31186000 }
        , ExecutionUnits { executionMemory = 7740, executionSteps = 31186000 } ]
      )
    , ( "currency-2.json"
      , [ExecutionUnits { executionMemory = 7130, executionSteps = 28807000 }]
      )
    , ( "escrow-redeem_1-3.json"
      , [ ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 }
        , ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 } ]
      )
    , ( "escrow-redeem_2-4.json"
      , [ ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 }
        , ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 }
        , ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 } ]
      )
    , ( "escrow-refund-2.json"
      , [ ExecutionUnits { executionMemory = 10460, executionSteps = 41794000 } ]
      )
    , ( "future-increase-margin-2.json"
      , [ExecutionUnits { executionMemory = 7130, executionSteps = 28807000 }]
      )
    , ( "future-increase-margin-5.json"
      , [ ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }
        , ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }]
      )
    , ( "future-increase-margin-6.json"
      , [ExecutionUnits { executionMemory = 16920, executionSteps = 66988000 }]
      )
    , ( "future-increase-margin-7.json"
      , [ExecutionUnits { executionMemory = 16920, executionSteps = 66988000 }]
      )
    , ( "future-pay-out-2.json"
      , [ExecutionUnits { executionMemory = 7130, executionSteps = 28807000 }]
      )
    , ( "future-pay-out-5.json"
      , [ ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }
        , ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }]
      )
    , ( "future-pay-out-6.json"
      , [ExecutionUnits { executionMemory = 16920, executionSteps = 66988000 }]
      )
    , ( "future-settle-early-2.json"
      , [ExecutionUnits { executionMemory = 7130, executionSteps = 28807000 }]
      )
    , ( "future-settle-early-5.json"
      , [ ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }
        , ExecutionUnits { executionMemory = 11050, executionSteps = 44095000 }]
      )
    , ( "future-settle-early-6.json"
      , [ExecutionUnits { executionMemory = 16920, executionSteps = 66988000 }]
      )
    , ( "game-sm-success-2.json"
      , [ ExecutionUnits { executionMemory = 11860, executionSteps = 47254000 }
        , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000 }]
      )
    , ( "game-sm-success-4.json"
      , [ExecutionUnits { executionMemory = 11860, executionSteps = 47254000 }]
      )
    , ( "game-sm-success_2-2.json"
      , [ ExecutionUnits { executionMemory = 11860, executionSteps = 47254000 }
        , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000 }]
      )
    , ( "game-sm-success_2-4.json"
      , [ExecutionUnits { executionMemory = 11860, executionSteps = 47254000 }]
      )
    , ( "game-sm-success_2-6.json"
      , [ExecutionUnits { executionMemory = 11860, executionSteps = 47254000 }]
      )
    , ( "multisig-failure-2.json"
      , [ExecutionUnits { executionMemory = 7800, executionSteps = 31420000 }]
      )
    , ( "multisig-sm-10.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-11.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-2.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-3.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-4.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-5.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-6.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-7.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-8.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-sm-9.json"
      , [ExecutionUnits { executionMemory = 14140, executionSteps = 56146000 }]
      )
    , ( "multisig-success-2.json"
      , [ExecutionUnits { executionMemory = 7800, executionSteps = 31420000 }]
      )
    , ( "ping-pong-2.json"
      , [ExecutionUnits { executionMemory = 11620, executionSteps = 46318000 }]
      )
    , ( "ping-pong-3.json"
      , [ExecutionUnits { executionMemory = 11620, executionSteps = 46318000 }]
      )
    , ( "ping-pong_2-2.json"
      , [ExecutionUnits { executionMemory = 11620, executionSteps = 46318000 }]
      )
    --, ( "prism-3.json"
    --  , [ ExecutionUnits { executionMemory = 12710, executionSteps = 50569000 }
    --    , ExecutionUnits { executionMemory = 6870, executionSteps = 27793000 }]
    --  ) -- Error in $[0]: there should be one 'lovelace' in 'value'
    , ( "pubkey-2.json"
     , [ExecutionUnits { executionMemory = 6690, executionSteps = 27091000 }]
     )
    --, ( "stablecoin_1-2.json"
    --  , [ ExecutionUnits { executionMemory = 17940, executionSteps = 70966000 }
    --    , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000}]
    --  ) -- Error in $[0]: Value should not be empty
    , ( "stablecoin_1-3.json"
      , [ ExecutionUnits { executionMemory = 17940, executionSteps = 70966000 }
        , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000}]
      )
    , ( "stablecoin_1-4.json"
      , [ ExecutionUnits { executionMemory = 17940, executionSteps = 70966000 }
        , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000}]
      )
    --, ( "stablecoin_2-2.json"
    --  , [ ExecutionUnits { executionMemory = 17940, executionSteps = 70966000 }
    --    , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000}]
    --  ) --Error in $[0]: Value should not be empty
    , ( "stablecoin_2-3.json"
      , [ ExecutionUnits { executionMemory = 17940, executionSteps = 70966000 }
        , ExecutionUnits { executionMemory = 4880, executionSteps = 20032000}]
      )
    , ( "token-account-2.json"
      , [ExecutionUnits { executionMemory = 6950, executionSteps = 28105000 }]
      )
    , ( "token-account-5.json"
      , [ExecutionUnits { executionMemory = 7900, executionSteps = 31810000 }]
      )
    , ( "uniswap-10.json"
      , [ ExecutionUnits { executionMemory = 13310, executionSteps = 52909000 }
        , ExecutionUnits { executionMemory = 4710, executionSteps = 19369000 }]
      )
    , ( "uniswap-2.json"
      , [ ExecutionUnits { executionMemory = 7490, executionSteps = 30211000 } ]
      )
    , ( "uniswap-7.json"
      , [ExecutionUnits { executionMemory = 6950, executionSteps = 28105000 }]
      )
    --, ( "uniswap-9.json"
    --  , [ ExecutionUnits { executionMemory = 13310, executionSteps = 52909000 }
    --    , ExecutionUnits { executionMemory = 4710, executionSteps = 19369000 }]
    --  ) -- Error in $[0]: there should be one 'lovelace' in 'value'
    , ( "vesting-2.json"
      , [ExecutionUnits { executionMemory = 10250, executionSteps = 40975000 }]
      )
    ]

matrixNormalTxExamples :: [(String, ByteString)]
matrixNormalTxExamples =
    [ ( "multiple outputs tx"
      , "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236c\
        \a8f1770e07fa22ba8648000d80018482583901a65f0e7aea387adbc109\
        \123a571cfd8d0d139739d359caaf966aa5b9a062de6ec013404d4f9909\
        \877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a82583901ac9a56\
        \280ec283eb7e12146726bfe68dcd69c7a85123ce2f7a10e7afa062de6e\
        \c013404d4f9909877d452fc57dfe4f8b67f94e0ea1e8a0ba1a000f422a\
        \825839011a2f2f103b895dbe7388acc9cc10f90dc4ada53f46c841d2ac\
        \44630789fc61d21ddfcbd4d43652bf05c40c346fa794871423b65052d7\
        \614c1b0000000ba42b176a82583901c59701fee28ad31559870ecd6ea9\
        \2b143b1ce1b68ccb62f8e8437b3089fc61d21ddfcbd4d43652bf05c40c\
        \346fa794871423b65052d7614c1b0000000ba42b176a021a000234d803\
        \198ceb0e80a0f5f6"
      )
    , ( "single output tx"
      , "84a600818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1\
        \770e07fa22ba86480d0d800182825839010acce4f85ade867308f048fe4516\
        \c0383b38cc04602ea6f7a6a1e75f29450899547b0e4bb194132452d45fea30\
        \212aebeafc69bca8744ea61a002dc67e8258390110a9b4666ba80e4878491d\
        \1ac20465c9893a8df5581dc705770626203d4d23fe6a7acdda5a1b41f56100\
        \f02bfa270a3c560c4e55cf8312331b00000017484721ca021a0001ffb80319\
        \8d280e80a0f5f6"
      )
    ]

updateSealedTxSpec :: Spec
updateSealedTxSpec = do
    describe "updateSealedTx" $ do
        describe "no existing key witnesses" $ do
            it "combines ins, outs and sets new fee"
                $ property prop_updateSealedTx

            -- TODO [ADP-1140]: These should be mergable with the property
            -- above, if we include the PAB examples in the arbitrary instance.
            --
            -- But we will also need to pattern match on the fee modifier,
            -- currently a `Coin -> Coin`, so we might need to replace it with
            -- either `Maybe Coin` or even just `Coin`.
            describe "updateSealedTx tx noExtraTxBodyContent == Right tx" $ do
                let testPlutusDir = $(getTestData) </> "plutus"
                let matrix =
                        [ "auction_1-2.json"
                        , "crowdfunding-success-4.json"
                        , "currency-2.json"
                        , "escrow-redeem_1-3.json"
                        , "escrow-redeem_2-4.json"
                        , "escrow-refund-2.json"
                        , "future-increase-margin-2.json"
                        , "future-increase-margin-5.json"
                        , "future-increase-margin-6.json"
                        , "future-increase-margin-7.json"
                        , "future-pay-out-2.json"
                        , "future-pay-out-5.json"
                        , "future-pay-out-6.json"
                        , "future-settle-early-2.json"
                        , "future-settle-early-5.json"
                        , "future-settle-early-6.json"
                        , "game-sm-success-2.json"
                        , "game-sm-success-4.json"
                        , "game-sm-success_2-2.json"
                        , "game-sm-success_2-4.json"
                        , "game-sm-success_2-6.json"
                        , "multisig-failure-2.json"
                        , "multisig-sm-10.json"
                        , "multisig-sm-11.json"
                        , "multisig-sm-2.json"
                        , "multisig-sm-3.json"
                        , "multisig-sm-4.json"
                        , "multisig-sm-5.json"
                        , "multisig-sm-6.json"
                        , "multisig-sm-7.json"
                        , "multisig-sm-8.json"
                        , "multisig-sm-9.json"
                        , "multisig-success-2.json"
                        , "ping-pong-2.json"
                        , "ping-pong-3.json"
                        , "ping-pong_2-2.json"
                        --, "prism-3.json" -- Error in $[0]: there should be one 'lovelace' in 'value'
                        , "pubkey-2.json"
                        --, "stablecoin_1-2.json" -- Error in $[0]: Value should not be empty
                        , "stablecoin_1-3.json"
                        , "stablecoin_1-4.json"
                        --, "stablecoin_2-2.json" --Error in $[0]: Value should not be empty
                        , "stablecoin_2-3.json"
                        , "token-account-2.json"
                        , "token-account-5.json"
                        , "uniswap-10.json"
                        , "uniswap-2.json"
                        , "uniswap-7.json"
                        --, "uniswap-9.json" -- Error in $[0]: there should be one 'lovelace' in 'value'
                        , "vesting-2.json"
                        ]
                forM_ matrix $ \json -> do
                    let testFile = testPlutusDir </> json
                    it json $ do
                        bs <- BL.readFile testFile
                        let decodeResult = eitherDecode @(ApiBalanceTransactionPostData 'Mainnet) bs
                        case decodeResult of
                            Left e -> expectationFailure $ show e
                            Right (ApiBalanceTransactionPostData (ApiT tx) _ _ ) -> do
                                updateSealedTx noExtraTxBodyContent tx
                                    `shouldBe` Right tx

                forM_ matrixNormalTxExamples $ \(title, hexBytes) -> do
                    it title $ do
                        case sealedTxFromBytes $ unsafeFromHex hexBytes of
                            Left e -> expectationFailure $ show e
                            Right tx -> do
                                updateSealedTx noExtraTxBodyContent tx
                                    `shouldBe` Right tx

        describe "existing key witnesses" $ do
            it "returns `Left err` with noExtraTxBodyContent" $ do
                -- Could be argued that it should instead return `Right tx`.
                case sealedTxFromBytes $ unsafeFromHex txWithInputsOutputsAndWits of
                    Left e -> expectationFailure $ show e
                    Right tx -> do
                        print $ updateSealedTx noExtraTxBodyContent tx
                        updateSealedTx noExtraTxBodyContent tx
                            `shouldBe` Left (ErrExistingKeyWitnesses 2)

            it "returns `Left err` when extra body content is non-empty" $ do
                pendingWith "todo: add test data"

newtype PartialTx = PartialTx SealedTx
    deriving (Show, Eq)

instance Arbitrary PartialTx where
    arbitrary = fmap PartialTx $ elements $ mconcat
        [ map (either (error . show) id . sealedTxFromBytes . unsafeFromHex . snd)
            matrixNormalTxExamples
        ]

prop_updateSealedTx :: PartialTx -> [TxIn] -> [TxOut] -> Coin -> Property
prop_updateSealedTx (PartialTx tx) extraIns extraOuts newFee = do
    let extra = ExtraTxBodyContent extraIns extraOuts (const newFee)
    let tx' = either (error . show) id
            $ updateSealedTx extra tx

    conjoin
        [ ins tx' === ins tx <> Set.fromList extraIns
        , outs tx' === outs tx <> Set.fromList extraOuts
        , fee tx' === Just newFee
        ]
  where
    ins = Set.fromList . map fst . view #resolvedInputs . _decodeSealedTx
    outs = Set.fromList . view #outputs . _decodeSealedTx
    fee = view #fee . _decodeSealedTx


txWithInputsOutputsAndWits :: ByteString
txWithInputsOutputsAndWits =
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
