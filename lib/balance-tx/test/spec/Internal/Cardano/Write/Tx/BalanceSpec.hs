{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Use null" -}
{- HLINT ignore "Use camelCase" -}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Cardano.Write.Tx.BalanceSpec
    ( spec
    ) where

import Cardano.Api.Ledger
    ( mkDelegStakeTxCert
    , mkRegTxCert
    , mkUnRegTxCert
    )
import Cardano.Binary
    ( ToCBOR
    , serialize'
    , unsafeDeserialize'
    )
import Cardano.Crypto.Wallet
    ( XPrv
    , toXPub
    )
import Cardano.Ledger.Alonzo.Plutus.TxInfo
    ( AlonzoContextError (..)
    )
import Cardano.Ledger.Api
    ( AllegraEraTxBody (..)
    , AlonzoEraTxBody (..)
    , BootstrapWitness
    , EraTx (witsTxL)
    , EraTxBody (..)
    , EraTxWits (bootAddrTxWitsL, scriptTxWitsL)
    , MaryEraTxBody (..)
    , ShelleyEraTxBody (..)
    , TransactionScriptFailure (..)
    , ValidityInterval (..)
    , addrTxOutL
    , addrTxWitsL
    , allInputsTxBodyF
    , bodyTxL
    , coinTxOutL
    , collateralReturnTxBodyL
    , mkBasicTx
    , mkBasicTxOut
    , ppCoinsPerUTxOByteL
    , ppMaxTxSizeL
    , ppMinFeeAL
    , serialiseAddr
    , totalCollateralTxBodyL
    )
import Cardano.Ledger.Api.UTxO
    ( EraUTxO (getProducedValue)
    )
import Cardano.Ledger.Babbage.TxInfo
    ( BabbageContextError (..)
    )
import Cardano.Ledger.BaseTypes
    ( unsafeNonZero
    )
import Cardano.Ledger.Conway.TxInfo
    ( ConwayContextError (..)
    )
import Cardano.Ledger.Core
    ( Era
    )
import Cardano.Ledger.Keys.Bootstrap
    ( makeBootstrapWitness
    )
import Cardano.Ledger.Shelley.API
    ( Credential (..)
    , KeyHash (..)
    , StrictMaybe (SJust, SNothing)
    , Withdrawals (..)
    )
import Cardano.Ledger.Val
    ( coin
    , (<->)
    )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic)
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.Wallet.Address.Constants
    ( maxLengthAddressForByron
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , hex
    , paymentAddress
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( byronKey
    )
import Cardano.Wallet.Address.Discovery
    ( GenChange (genChange)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    , mkRndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    (
    )
import Cardano.Wallet.Address.Encoding
    ( toHDPayloadAddress
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , NetworkId (..)
    , SNetworkId (..)
    , withSNetworkId
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Control.Arrow
    ( left
    )
import Control.Lens
    ( set
    , (%~)
    , (.~)
    , (^.)
    )
import Control.Monad
    ( forM
    , forM_
    , replicateM
    )
import Control.Monad.Random
    ( evalRand
    )
import Control.Monad.Trans.Except
    ( runExcept
    , runExceptT
    )
import Control.Monad.Trans.State.Strict
    ( evalState
    , state
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isDigit
    )
import Data.Coerce
    ( coerce
    )
import Data.Default
    ( Default (..)
    )
import Data.Either
    ( isRight
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Functor.Identity
    ( Identity
    )
import Data.Generics.Internal.VL.Lens
    ( over
    , view
    )
import Data.Group
    ( Group (invert)
    )
import Data.IntCast
    ( intCast
    )
import Data.List
    ( isSuffixOf
    , sortOn
    )
import Data.Maybe
    ( catMaybes
    , fromJust
    )
import Data.SOP.Counting
    ( exactlyOne
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Tuple
    ( swap
    )
import Data.Word
    ( Word8
    )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , fixedF
    , fmt
    , nameF
    , pretty
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import Generics.SOP
    ( NP (Nil)
    )
import Internal.Cardano.Write.Eras
    ( AnyRecentEra (..)
    , Babbage
    , CardanoApiEra
    , InAnyRecentEra (..)
    , IsRecentEra (recentEra)
    , RecentEra (..)
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    , toInAnyRecentEra
    )
import Internal.Cardano.Write.Tx
    ( Address
    , Coin (..)
    , Datum (..)
    , FeePerByte (..)
    , Tx
    , TxIn
    , TxOut
    , TxOutInRecentEra (..)
    , UTxO (..)
    , Value
    , deserializeTx
    , fromCardanoApiTx
    , serializeTx
    , toCardanoApiTx
    , unsafeUtxoFromTxOutsInRecentEra
    )
import Internal.Cardano.Write.Tx.Balance
    ( ChangeAddressGen (..)
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrUpdateTx (..)
    , PartialTx (..)
    , Redeemer (..)
    , StakeKeyDepositLookup
        ( StakeKeyDepositAssumeCurrent
        , StakeKeyDepositMap
        )
    , TxFeeUpdate (UseNewTxFee)
    , TxUpdate (..)
    , UTxOAssumptions (..)
    , balanceTx
    , constructUTxOIndex
    , lookupStakeKeyDeposit
    , mergeSignedValue
    , noTxUpdate
    , splitSignedValue
    , stakeCredentialsWithRefunds
    , toWalletUTxO
    , updateTx
    )
import Internal.Cardano.Write.Tx.Gen
    ( mockPParams
    )
import Internal.Cardano.Write.Tx.Sign
    ( KeyWitnessCounts (..)
    , estimateKeyWitnessCounts
    , estimateSignedTxMinFee
    , estimateSignedTxSize
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( sizeOf_BootstrapWitnesses
    )
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    )
import Internal.Cardano.Write.Tx.TxWithUTxO
    ( pattern TxWithUTxO
    , type TxWithUTxO
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    , mkSlotLength
    )
import Ouroboros.Consensus.Config
    ( SecurityParam (..)
    )
import Ouroboros.Network.Block
    ( SlotNo (..)
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( takeExtension
    , (</>)
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenFromSeed
    )
import Test.Cardano.Ledger.Mary.Arbitrary
    (
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , pendingWith
    , runIO
    , shouldBe
    )
import Test.Hspec.Core.Spec
    ( SpecM
    )
import Test.Hspec.Golden
    ( Golden (..)
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , NonNegative (..)
    , Property
    , arbitraryBoundedEnum
    , arbitrarySizedNatural
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , frequency
    , label
    , listOf
    , oneof
    , property
    , quickCheckWith
    , scale
    , shrinkBoundedEnum
    , shrinkList
    , shrinkMapBy
    , stdArgs
    , suchThat
    , tabulate
    , vectorOf
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Extra
    ( DisjointPair
    , genDisjointPair
    , genericRoundRobinShrink
    , getDisjointPair
    , shrinkDisjointPair
    , shrinkMapToSubmaps
    , shrinkNatural
    , (.>=.)
    , (<:>)
    , (<@>)
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    )
import Test.Utils.Paths
    ( getTestData
    )
import Test.Utils.Pretty
    ( Pretty (..)
    )
import Text.Read
    ( readMaybe
    )
import Prelude

import qualified Cardano.Address as CA
import qualified Cardano.Address.Derivation as CA
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Byron as ByronApi
import qualified Cardano.Api.Gen as CardanoApi
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway.Core as Ledger
import qualified Cardano.Ledger.Val as Value
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Slot as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Cardano.Wallet.Address.Derivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Convert
import qualified Cardano.Wallet.Primitive.Types.Address as W
    ( Address (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin as W
    ( Coin (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Coin.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
    ( Hash (..)
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
    ( TokenBundle
    )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundle.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.Constraints as W
    ( TxSize (..)
    )
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
    ( TxOut (..)
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Internal.Cardano.Write.Tx as Write
import qualified Internal.Cardano.Write.Tx.TxWithUTxO as TxWithUTxO
import qualified Internal.Cardano.Write.Tx.TxWithUTxO.Gen as TxWithUTxO
import qualified Ouroboros.Consensus.HardFork.History as HF
import qualified Test.Hspec.Extra as Hspec

--------------------------------------------------------------------------------
-- Specifications
--------------------------------------------------------------------------------

spec :: Spec
spec = do
    forAllRecentEras $ \era -> do
        spec_balanceTx era
        spec_updateTx era

    -- The exact test expectations depend on the test data which was generated
    -- in either Alonzo or Babbage. Only running in Babbage should be fine for
    -- now. When Babbage is dropped we could regenerate the test txs.
    spec_estimateSignedTxSize RecentEraBabbage
  where
    forAllRecentEras
        :: (forall era. IsRecentEra era => RecentEra era -> Spec) -> Spec
    forAllRecentEras tests = do
        describe "Conway" $ tests RecentEraConway
        describe "Babbage" $ tests RecentEraBabbage

spec_balanceTx :: forall era. IsRecentEra era => RecentEra era -> Spec
spec_balanceTx era = describe "balanceTx" $ do
    let moreDiscardsAllowed = stdArgs{maxDiscardRatio = 100}
    it "doesn't balance transactions with existing 'totalCollateral'"
        $ quickCheckWith moreDiscardsAllowed
        $ prop_balanceTxExistingTotalCollateral era

    it "doesn't balance transactions with existing 'returnCollateral'"
        $ quickCheckWith moreDiscardsAllowed
        $ prop_balanceTxExistingReturnCollateral era

    it "does not balance transactions if no inputs can be created"
        $ property (prop_balanceTxUnableToCreateInput era)

    it "produces valid transactions or fails"
        $ property (prop_balanceTxValid era)

    describe "bootstrap witnesses" $ do
        -- Used in 'estimateTxSize', and in turn used by coin-selection
        let coinSelectionEstimatedSize :: Natural -> Natural
            coinSelectionEstimatedSize = W.unTxSize . sizeOf_BootstrapWitnesses

        let withNoKeyWits tx =
                tx
                    & (witsTxL . addrTxWitsL) .~ mempty
                    & (witsTxL . bootAddrTxWitsL) .~ mempty

        let measuredWitSize :: Tx era -> Natural
            measuredWitSize tx =
                fromIntegral
                    $ serializedSize tx
                        - serializedSize (withNoKeyWits tx)

        let evaluateMinimumFeeSize :: Tx era -> Natural
            evaluateMinimumFeeSize tx =
                fromIntegral
                    $ Write.unCoin
                    $ estimateSignedTxMinFee
                        pp
                        inputsHaveNoRefScripts
                        (withNoKeyWits tx)
                        (KeyWitnessCounts 0 (fromIntegral $ length wits))
              where
                wits = tx ^. witsTxL . bootAddrTxWitsL

                -- Dummy PParams to ensure a Coin-delta corresponds to a
                -- size-delta.
                pp = Ledger.emptyPParams & set ppMinFeeAL (Ledger.Coin 1)

                -- Dummy UTxO lookup telling the ledger the inputs aren't
                -- bringing reference scripts into scope
                inputsHaveNoRefScripts =
                    utxoPromisingInputsHaveAddress dummyAddr tx

        let evaluateMinimumFeeDerivedWitSize :: Tx era -> Natural
            evaluateMinimumFeeDerivedWitSize tx =
                evaluateMinimumFeeSize tx
                    - evaluateMinimumFeeSize (withNoKeyWits tx)

        it "coin-selection's size estimation == balanceTx's size estimation"
            $ property
            $ prop_bootstrapWitnesses era
            $ \n tx -> do
                let balanceSize = evaluateMinimumFeeDerivedWitSize tx
                let csSize = coinSelectionEstimatedSize $ intCast n
                csSize === balanceSize
        -- >= would suffice, but we can be stronger

        it "balanceTx's size estimation >= measured serialized size"
            $ property
            $ prop_bootstrapWitnesses era
            $ \n tx -> do
                let estimated = evaluateMinimumFeeDerivedWitSize tx
                let measured = measuredWitSize tx
                let overestimation
                        | estimated > measured = estimated - measured
                        | otherwise = 0

                let tabulateOverestimation =
                        tabulate "overestimation/wit"
                            $ if n == 0
                                then [show overestimation <> " (but with no wits)"]
                                else [show $ overestimation `div` fromIntegral n]

                estimated .>=. measured
                    & tabulateOverestimation

    balanceTxGoldenSpec era

    describe "change address generation" $ do
        let balance'
                :: PartialTx era
                -> Either (ErrBalanceTx era) (Tx era, DummyChangeState)
            balance' =
                balanceTxWithDummyChangeState
                    AllKeyPaymentCredentials
                    dustUTxO
                    testStdGenSeed

        -- We could generate arbitrary tx bodies to test with, but by
        -- limiting ourselves to 'paymentPartialTx' with a fixed number of
        -- payments 1) ensures balancing always succeeds 2) makes it easy to
        -- have separate 'it' statements for different expectations of the same
        -- test case.
        let nPayments = 10
        let paymentOuts :: [TxOut era]
            paymentOuts =
                replicate nPayments
                    $ mkBasicTxOut dummyAddr (ada 1)

        let ptx = paymentPartialTx paymentOuts

        -- True for values of nPayments small enough not to cause
        -- 'ErrBalanceTxMaxSizeLimitExceeded' or ErrMakeChange
        let nChange = max nPayments 1
        let changeState0 = DummyChangeState 0
        let expectedChange =
                flip evalState changeState0
                    $ replicateM nChange
                    $ state @Identity dummyChangeAddrGen.genChangeAddress

        let
            address :: TxOut era -> Address
            address = view addrTxOutL

        let (tx, changeState') =
                either (error . show) id $ balance' ptx

        it "assigns change addresses as expected"
            $ map address (outputs tx)
            `shouldBe` (map address paymentOuts ++ expectedChange)

        it "returns a change state that corresponds to the addresses used" $ do
            changeState' `shouldBe` DummyChangeState{nextUnusedIndex = nChange}

    it "assigns minimal ada quantities to outputs without ada" $ do
        let (out :: TxOut era) = mkBasicTxOut dummyAddr (lovelace 0)
        let (out' :: TxOut era) = mkBasicTxOut dummyAddr (lovelace 866_310)
        let tx =
                either (error . show) id
                    $ balance
                    $ paymentPartialTx [out]
        let outs = F.toList $ tx ^. bodyTxL . outputsTxBodyL

        let pp = case era of
                RecentEraBabbage ->
                    def
                        & ppCoinsPerUTxOByteL .~ testParameter_coinsPerUTxOByte
                RecentEraConway ->
                    def
                        & ppCoinsPerUTxOByteL .~ testParameter_coinsPerUTxOByte
        case outs of
            (firstOut : _) -> do
                Write.isBelowMinimumCoinForTxOut pp firstOut
                    `shouldBe` False
                firstOut `shouldBe` out'
            [] -> expectationFailure "Expected non-empty outputs"

    describe "effect of txMaxSize on coin selection" $ do
        let balanceWithDust =
                testBalanceTx
                    dustWallet
                    mockPParams
                    dummyTimeTranslation
                    testStdGenSeed

        it "tries to select 2x the payment amount" $ do
            let tx =
                    balanceWithDust
                        $ paymentPartialTx
                            [ mkBasicTxOut dummyAddr (ada 50)
                            ]
            totalOutput <$> tx `shouldBe` Right (Coin 100_000_000)

        it "falls back to 1x if out of space" $ do
            let tx =
                    balanceWithDust
                        $ paymentPartialTx
                            [ mkBasicTxOut dummyAddr (ada 100)
                            ]
            totalOutput <$> tx `shouldBe` Right (Coin 102_000_000)

        it "otherwise fails with ErrBalanceTxMaxSizeLimitExceeded" $ do
            let tx =
                    balanceWithDust
                        $ paymentPartialTx
                            [ mkBasicTxOut dummyAddr (ada 200)
                            ]
            tx
                `shouldBe` Left
                    ( ErrBalanceTxMaxSizeLimitExceeded
                        { size = W.TxSize $ case era of
                            RecentEraConway -> 28_232
                            RecentEraBabbage -> 28_226
                        , maxSize = W.TxSize 16_384
                        }
                    )

    describe "stake key deposit lookup" $ do
        let stakeCred =
                KeyHashObj
                    $ KeyHash
                        "00000000000000000000000000000000000000000000000000000000"
        let partialTxWithRefund :: Coin -> PartialTx era
            partialTxWithRefund r =
                PartialTx
                    { tx =
                        mkBasicTx
                            $ mkBasicTxBody
                            & certsTxBodyL
                                .~ StrictSeq.fromList
                                    [ mkUnRegTxCert stakeCred
                                    ]
                    , stakeKeyDeposits =
                        StakeKeyDepositMap $ Map.singleton stakeCred r
                    , extraUTxO = mempty
                    , redeemers = mempty
                    , timelockKeyWitnessCounts = mempty
                    }
        let adaProduced r =
                coin
                    . getProducedValue mockPParams (error "no pool regs")
                    . view bodyTxL
                    . either (error . show) id
                    . balance
                    $ partialTxWithRefund r

        it "affects the tx balance" $ property $ \(refund :: Coin) -> do
            -- The smallest refund
            let noRefund = Coin 0

            -- All else equal, increasing the value of the refund should
            -- increase the change, and thus the produced ada value of the
            -- balanced tx.
            (adaProduced refund <-> adaProduced noRefund)
                `shouldBe` (refund <-> noRefund)

        describe "if missing" $ do
            describe "using StakeKeyDepositMap mempty"
                $ it "fails"
                $ do
                    let partialTx =
                            (partialTxWithRefund (Coin 1_000_000))
                                { stakeKeyDeposits = StakeKeyDepositMap mempty
                                }
                    case balance partialTx of
                        Left ErrBalanceTxUnresolvedRefunds{} -> return ()
                        Right tx ->
                            expectationFailure
                                $ "Expected ErrBalanceTxUnresolvedRefunds; got "
                                    <> show tx
                        Left otherErr ->
                            expectationFailure
                                $ "Expected ErrBalanceTxUnresolvedRefunds; got "
                                    <> show otherErr

            describe "using StakeKeyDepositAssumeCurrent"
                $ it "succeeds (but may be wrong)"
                $ do
                    let partialTx =
                            (partialTxWithRefund (Coin 1_000_000))
                                { stakeKeyDeposits = StakeKeyDepositAssumeCurrent
                                }
                    case balance partialTx of
                        Right tx -> do
                            ( coin
                                    . getProducedValue mockPParams (error "no pool regs")
                                    . view bodyTxL
                                    $ tx
                                )
                                `shouldBe` (adaProduced (Coin 2_000_000))
                        Left _ -> return ()

    describe "when passed unresolved inputs" $ do
        it "fails with ErrBalanceTxUnresolvedInputs" $ do
            let txin = W.TxIn (W.Hash $ B8.replicate 32 '3') 10
            -- 1 output, 1 input without utxo entry
            let partialTx :: PartialTx era
                partialTx =
                    addExtraTxIns [txin]
                        $ paymentPartialTx
                            [ mkBasicTxOut dummyAddr (ada 1)
                            ]
            balance partialTx
                `shouldBe` Left
                    ( ErrBalanceTxUnresolvedInputs
                        . NESet.singleton
                        $ Convert.toLedger txin
                    )

        describe "with redeemers"
            $ it "fails with ErrBalanceTxUnresolvedInputs"
            $ do
                let withNoUTxO :: PartialTx era -> PartialTx era
                    withNoUTxO ptx = ptx{extraUTxO = Write.UTxO mempty}

                balance (withNoUTxO pingPong_2)
                    `shouldBe` Left
                        ( ErrBalanceTxUnresolvedInputs
                            . NESet.singleton
                            . Convert.toLedger
                            $ W.TxIn (W.Hash "11111111111111111111111111111111") 0
                        )

    describe "when validity interval is too far in the future" $ do
        let withValidityBeyondHorizon =
                withValidityInterval
                    $ ValidityInterval SNothing (SJust beyondHorizon)
        describe "with some Plutus redeemers" $ do
            it "fails with TimeTranslationPastHorizon" $ do
                case left toInAnyRecentEra $ balance (withValidityBeyondHorizon pingPong_2) of
                    Left
                        ( InConway
                                ( ErrBalanceTxAssignRedeemers
                                        ( ErrAssignRedeemersScriptFailure
                                                _redeemer
                                                ( ContextError
                                                        ( BabbageContextError
                                                                ( AlonzoContextError
                                                                        ( TimeTranslationPastHorizon
                                                                                _pastHoriozon
                                                                            )
                                                                    )
                                                            )
                                                    )
                                            )
                                    )
                            ) -> return ()
                    Left
                        ( InBabbage
                                ( ErrBalanceTxAssignRedeemers
                                        ( ErrAssignRedeemersScriptFailure
                                                _redeemer
                                                ( ContextError
                                                        ( AlonzoContextError
                                                                ( TimeTranslationPastHorizon
                                                                        _pastHoriozon
                                                                    )
                                                            )
                                                    )
                                            )
                                    )
                            ) -> return ()
                    other ->
                        expectationFailure
                            $ "Expected pastHorizon failure; got " <> show other

        describe "with no redeemers" $ do
            it "succeeds at balancing" $ do
                case balance (withValidityBeyondHorizon pingPong_1) of
                    Right _tx -> return ()
                    other ->
                        expectationFailure
                            $ "Expected (Right tx); got " <> show other

    describe "when a redeemer is missing" $ do
        it "balancing succeeds (currently)" $ do
            -- Plutus script inputs must have a corresponding redeemer. If one
            -- is missing, the resuling tx will not be acceptable by the ledger.
            -- Instead of the current behaviour, it might make more sense to
            -- fail.
            --
            -- The only reason for succeeding might be if a party, say Bob, only
            -- wants to add a redeemer, revealing the redeemer value, after
            -- seeing Alice partially balance the transaction. However it seems
            -- too unclear both whether 1) this is actually is useful, and
            -- 2) whether it would work techincally, aside from lack of
            -- protective guard in balanceTx, so failing might still be saner.
            let withNoRedeemers = over #redeemers (const [])
            case balance (withNoRedeemers pingPong_2) of
                Right _tx -> pure ()
                other ->
                    expectationFailure
                        $ "Expected (Right tx); got " <> show other

    describe "when a redeemer points to an input that doesn't exist" $ do
        it "fails with ErrAssignRedeemersTargetNotFound" $ do
            let tid = W.Hash $ B8.replicate 32 '1'

            -- With ix 1 instead of 0, making it point to an input which
            -- doesn't exist in the tx.
            let faultyRedeemer =
                    RedeemerSpending
                        (unsafeFromHex "D87A80")
                        (Convert.toLedger (W.TxIn tid 1))

            let withFaultyRedeemer =
                    over #redeemers $ mapFirst $ const faultyRedeemer

            balance (withFaultyRedeemer pingPong_2)
                `shouldBe` Left
                    ( ErrBalanceTxAssignRedeemers
                        (ErrAssignRedeemersTargetNotFound faultyRedeemer)
                    )

    describe "Merging and splitting signed values" $ do
        it "prop_mergeSignedValue_invert_swap"
            $ prop_mergeSignedValue_invert_swap
            & property
            & checkCoverage
        it "prop_splitSignedValue_invert_swap"
            $ prop_splitSignedValue_invert_swap
            & property
            & checkCoverage
        it "prop_mergeSignedValue_splitSignedValue"
            $ prop_mergeSignedValue_splitSignedValue
            & property
            & checkCoverage
        it "prop_splitSignedValue_mergeSignedValue"
            $ prop_splitSignedValue_mergeSignedValue
            & property
            & checkCoverage
  where
    outputs = F.toList . view (bodyTxL . outputsTxBodyL)

    mapFirst f (x : xs) = f x : xs
    mapFirst _ [] = error "mapFirst: empty list"

    horizon = SlotNo 20
    beyondHorizon = SlotNo 21

    wallet = mkTestWallet (utxo [Coin 5_000_000])

    -- Wallet with only small utxos, and enough of them to fill a tx in the
    -- tests below.
    dustWallet = mkTestWallet dustUTxO
    dustUTxO =
        UTxO
            $ Map.fromList
            $ [ ( Convert.toLedger $ W.TxIn (W.Hash $ B8.replicate 32 '1') ix
                , mkBasicTxOut dummyAddr (ada 1)
                )
              | ix <- [0 .. 500]
              ]

    balance =
        testBalanceTx
            wallet
            mockPParams
            (dummyTimeTranslationWithHorizon horizon)
            testStdGenSeed

    utxoWithValues values = UTxO $ Map.fromList $ zip ins outs
      where
        ins = map (Convert.toLedger . W.TxIn dummyHash) [0 ..]
        outs = map (mkBasicTxOut dummyAddr) values
        dummyHash = W.Hash $ B8.replicate 32 '0'

    utxo :: [Coin] -> UTxO era
    utxo coins = utxoWithValues $ map Value.inject coins

    dummyAddr =
        Convert.toLedgerAddress
            $ W.Address
            $ unsafeFromHex
                "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    totalOutput :: IsRecentEra era => Tx era -> Coin
    totalOutput tx =
        F.foldMap (view coinTxOutL) (view (bodyTxL . outputsTxBodyL) tx)
            <> tx
            ^. bodyTxL . feeTxBodyL

balanceTxGoldenSpec
    :: forall era. IsRecentEra era => RecentEra era -> Spec
balanceTxGoldenSpec era = describe "balance goldens" $ do
    it "testPParams"
        $ let name = "testPParams"
              dir = goldenDir </> "binary"
              pparams = mockPParams @era
          in  Golden
                { output = pparams
                , encodePretty = show
                , writeToFile = \fp -> T.writeFile fp . T.pack . toCBORHex
                , readFromFile =
                    (unsafeDeserialize' . unsafeFromHex . B8.pack <$>) . readFile
                , goldenFile = dir </> name </> "golden"
                , actualFile = Just (dir </> name </> "actual")
                , failFirstTime = False
                }

    describe "balanced binaries" $ do
        let dir = goldenDir </> "binary" </> "balanced"
        let walletUTxO = utxo [Coin 5_000_000]
        it "pingPong_2" $ do
            let ptx = pingPong_2
            let tx =
                    either (error . show) id
                        $ testBalanceTx
                            (mkTestWallet walletUTxO)
                            mockPParams
                            dummyTimeTranslation
                            testStdGenSeed
                            ptx

            let name = "pingPong_2"
            Golden
                { output = tx
                , encodePretty = show
                , writeToFile = \fp x ->
                    T.writeFile fp
                        . T.pack
                        . B8.unpack
                        . hex
                        . serializeTx @era
                        $ x
                , readFromFile =
                    fmap (deserializeTx . unsafeFromHex . B8.pack)
                        . readFile
                , goldenFile = dir </> name </> "golden"
                , actualFile = Just (dir </> name </> "actual")
                , failFirstTime = False
                }

    describe "results when varying wallet balance (1 UTxO)" $ do
        test "pingPong_1" pingPong_1
        test "pingPong_2" pingPong_2
        test "delegate" delegate
        test "1ada-payment" payment
  where
    toCBORHex :: ToCBOR a => a -> String
    toCBORHex = B8.unpack . hex . serialize'

    goldenDir = $(getTestData) </> "balanceTx" </> eraName
      where
        eraName = case era of
            RecentEraConway -> "conway"
            RecentEraBabbage -> "babbage"

    test :: String -> PartialTx era -> Spec
    test name partialTx = it name $ do
        goldenText
            name
            (map (mkGolden partialTx . Coin) defaultWalletBalanceRange)
      where
        defaultWalletBalanceRange = [0, 50_000 .. 4_000_000]

        goldenText :: String -> [BalanceTxGolden] -> Golden Text
        goldenText name' res =
            Golden
                { output = fmt $ blockListF' "" build res
                , encodePretty = T.unpack
                , writeToFile = T.writeFile
                , readFromFile = T.readFile
                , goldenFile = goldenDir </> name' </> "golden"
                , actualFile = Just (goldenDir </> name' </> "actual")
                , failFirstTime = False
                }
        mkGolden
            :: PartialTx era
            -> Coin
            -> BalanceTxGolden
        mkGolden ptx c =
            let
                walletUTxO = utxo [c]
                res =
                    testBalanceTx
                        (mkTestWallet walletUTxO)
                        mockPParams
                        dummyTimeTranslation
                        testStdGenSeed
                        ptx
                combinedUTxO =
                    mconcat
                        [ view #extraUTxO ptx
                        , walletUTxO
                        ]
            in
                case res of
                    Right tx ->
                        BalanceTxGoldenSuccess
                            c
                            (txFee tx)
                            (minFee tx combinedUTxO)
                    Left e ->
                        BalanceTxGoldenFailure c (show e)

    utxo :: [Coin] -> UTxO era
    utxo coins = UTxO $ Map.fromList $ zip ins outs
      where
        ins = map (Convert.toLedger . W.TxIn dummyHash) [0 ..]
        outs = map (mkBasicTxOut addr . Value.inject) coins
        dummyHash = W.Hash $ B8.replicate 32 '0'

    addr =
        Convert.toLedger
            $ W.Address
            $ unsafeFromHex
                "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    payment :: PartialTx era
    payment =
        paymentPartialTx
            [ mkBasicTxOut addr (ada 1)
            ]

    delegate :: PartialTx era
    delegate =
        PartialTx
            (mkBasicTx body)
            mempty
            mempty
            (StakeKeyDepositMap mempty)
            mempty
      where
        body :: TxBody era
        body =
            mkBasicTxBody
                & certsTxBodyL .~ StrictSeq.fromList certs

        dummyStakeKey =
            KeyHashObj
                $ KeyHash
                    "00000000000000000000000000000000000000000000000000000000"
        dummyPool =
            KeyHash
                "00000000000000000000000000000000000000000000000000000001"
        certs =
            [ mkRegTxCert dummyStakeKey
            , mkDelegStakeTxCert dummyStakeKey dummyPool
            ]

    minFee
        :: IsRecentEra era
        => Tx era
        -> UTxO era
        -> Coin
    minFee tx u =
        estimateSignedTxMinFee
            mockPParams
            u
            tx
            (estimateKeyWitnessCounts u tx mempty)

spec_estimateSignedTxSize
    :: forall era. IsRecentEra era => RecentEra era -> Spec
spec_estimateSignedTxSize _era = describe "estimateSignedTxSize" $ do
    txBinaries <- runIO signedTxTestData
    describe "equals the binary size of signed txs"
        $ forAllGoldens txBinaries test
  where
    test
        :: String
        -> ByteString
        -> Tx era
        -> IO ()
    test _name bs tx = do
        let pparams :: Write.PParams era
            pparams = mockPParams
            witCount dummyAddr =
                estimateKeyWitnessCounts
                    (utxoPromisingInputsHaveAddress dummyAddr tx)
                    tx
                    mempty
            noScripts = Map.null $ tx ^. witsTxL . scriptTxWitsL
            noBootWits = Set.null $ tx ^. witsTxL . bootAddrTxWitsL
            testDoesNotYetSupport x =
                pendingWith $ "Test setup does not work for txs with " <> x

            signedBinarySize = W.TxSize $ fromIntegral $ BS.length bs

        case (noScripts, noBootWits) of
            (True, True) -> do
                estimateSignedTxSize pparams (witCount vkCredAddr) tx
                    `shouldBeInclusivelyWithin` ( signedBinarySize - correction
                                                , signedBinarySize
                                                )
            (False, False) ->
                testDoesNotYetSupport "bootstrap wits + scripts"
            (True, False) ->
                estimateSignedTxSize pparams (witCount bootAddr) tx
                    `shouldBeInclusivelyWithin` ( signedBinarySize - correction
                                                , signedBinarySize + bootWitsCanBeLongerBy
                                                )
            (False, True) -> testDoesNotYetSupport "scripts"
      where
        -- Apparently the cbor encoding used by the ledger for size checks
        -- (`toCBORForSizeComputation`) is a few bytes smaller than the actual
        -- serialized size for these goldens.
        correction = W.TxSize 6

    forAllGoldens
        :: [(String, ByteString)]
        -> ( String
             -> ByteString
             -> Tx era
             -> IO ()
           )
        -> Spec
    forAllGoldens goldens f = forM_ goldens $ \(name, bs) ->
        it name
            $ let
                tx :: Tx era
                tx = deserializeTx bs
                msg =
                    unlines
                        [ B8.unpack $ hex bs
                        , show $ Pretty tx
                        ]
              in
                Hspec.counterexample msg $ f name bs tx

    -- An address with a vk payment credential. For the test above, this is the
    -- only aspect which matters.
    vkCredAddr =
        Convert.toLedger
            $ W.Address
            $ unsafeFromHex
                "6000000000000000000000000000000000000000000000000000000000"

    -- This is a short bootstrap address retrieved from
    -- "byron-address-format.md".
    bootAddr =
        Convert.toLedger
            $ W.Address
            $ unsafeFromHex
                "82d818582183581cba970ad36654d8dd8f74274b733452ddeab9a62a397746be3c42ccdda0001a9026da5b"

    -- With more attributes, the address can be longer. This value was chosen
    -- /experimentally/ to make the tests pass. The ledger has been validating
    -- new outputs with bootstrap addresses have attributes not larger than 64
    -- bytes. The ledger has done so since the middle of the Byron era.
    -- Address attributes are included in the bootstrap witnesses.
    --
    -- NOTE: If we had access to the real UTxO set for the inputs of the test
    -- txs, we wouldn't need this fuzziness. Related: ADP-2987.
    bootWitsCanBeLongerBy = W.TxSize 45

spec_updateTx :: forall era. IsRecentEra era => RecentEra era -> Spec
spec_updateTx _era = describe "updateTx" $ do
    describe "no existing key witnesses" $ do
        txs <- readTestTransactions
        forM_ txs $ \(filepath, tx :: Tx era) -> do
            prop ("with TxUpdate: " <> filepath)
                $ prop_updateTx tx

    describe "existing key witnesses" $ do
        signedTxs <- runIO signedTxTestData

        it "returns `Left err` with noTxUpdate" $ do
            -- Could be argued that it should instead return `Right tx`.
            let tx :: Tx era
                tx = case signedTxs of
                    ((_, bs) : _) -> deserializeTx bs
                    [] -> error "signedTxs unexpectedly empty"
            let res =
                    updateTx
                        tx
                        noTxUpdate

            res `shouldBe` Left (ErrUpdateTxExistingKeyWitnesses 1)

        it "returns `Left err` when extra body content is non-empty" $ do
            pendingWith "todo: add test data"
  where
    readTestTransactions
        :: SpecM a [(FilePath, Tx era)]
    readTestTransactions = runIO $ do
        let dir = $(getTestData) </> "plutus"
        paths <- listDirectory dir
        flip foldMap paths $ \f ->
            -- Ignore reject files
            if ".rej" `isSuffixOf` takeExtension f
                then pure []
                else do
                    contents <- BS.readFile (dir </> f)
                    pure [(f, deserializeTx $ unsafeFromHex contents)]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_balanceTxExistingReturnCollateral
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> SuccessOrFailure (BalanceTxArgs era)
    -> Property
prop_balanceTxExistingReturnCollateral
    _era
    (SuccessOrFailure balanceTxArgs) =
        withMaxSuccess 10
            $ hasReturnCollateral tx
                && not (hasInsCollateral tx)
                && not (hasTotalCollateral tx)
            ==> case testBalanceTx wallet protocolParams timeTranslation seed partialTx of
                Left err -> ErrBalanceTxExistingReturnCollateral === err
                e -> counterexample (show e) False
      where
        BalanceTxArgs
            { protocolParams
            , timeTranslation
            , wallet
            , partialTx
            , seed
            } =
                balanceTxArgs
        PartialTx{tx} = partialTx

prop_balanceTxExistingTotalCollateral
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> SuccessOrFailure (BalanceTxArgs era)
    -> Property
prop_balanceTxExistingTotalCollateral
    _era
    (SuccessOrFailure balanceTxArgs) =
        withMaxSuccess 10
            $ hasTotalCollateral tx
                && not (hasInsCollateral tx)
                && not (hasReturnCollateral tx)
            ==> case testBalanceTx wallet protocolParams timeTranslation seed partialTx of
                Left err -> ErrBalanceTxExistingTotalCollateral === err
                e -> counterexample (show e) False
      where
        BalanceTxArgs
            { protocolParams
            , timeTranslation
            , wallet
            , partialTx
            , seed
            } =
                balanceTxArgs
        PartialTx{tx} = partialTx

-- If 'balanceTx' is able to balance a transaction, then repeating the attempt
-- with all potential inputs removed (from the partial transaction and the
-- UTxO set) should always fail with 'ErrBalanceTxUnableToCreateInput'.
--
-- Note that we /cannot/ expect 'balanceTx' to fail with
-- 'ErrBalanceTxUnableToCreateInput' in all situations where there are no
-- potential inputs available, since:
--
-- 1. only at most one failure condition is ever reported by 'balanceTx';
-- 2. there can be multiple competing failure conditions;
-- 3. the order in which failure conditions are checked is unspecified.
--
prop_balanceTxUnableToCreateInput
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Success (BalanceTxArgs era)
    -> Property
prop_balanceTxUnableToCreateInput
    _era
    (Success balanceTxArgs) =
        withMaxSuccess 10
            $ testBalanceTx
                (eraseWalletUTxOSet wallet)
                protocolParams
                timeTranslation
                seed
                (erasePartialTxInputList partialTx)
                === Left ErrBalanceTxUnableToCreateInput
      where
        BalanceTxArgs
            { protocolParams
            , timeTranslation
            , wallet
            , partialTx
            , seed
            } =
                balanceTxArgs

        erasePartialTxInputList :: PartialTx era -> PartialTx era
        erasePartialTxInputList = over #tx (set (bodyTxL . inputsTxBodyL) mempty)

        eraseWalletUTxOSet :: Wallet era -> Wallet era
        eraseWalletUTxOSet (Wallet utxoAssumptions _utxo changeAddressGen) =
            Wallet utxoAssumptions mempty changeAddressGen

-- NOTE: 'balanceTx' relies on estimating the number of witnesses that
-- will be needed. The correctness of this estimation is not tested here.
--
-- TODO: Ensure scripts are well tested
--   - Ensure we have coverage for normal plutus contracts
prop_balanceTxValid
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> SuccessOrFailure (BalanceTxArgs era)
    -> Property
prop_balanceTxValid
    era
    (SuccessOrFailure balanceTxArgs) =
        withMaxSuccess 1_000 $ do
            let combinedUTxO =
                    view #extraUTxO partialTx
                        <> walletUTxO
            let originalTx = view #tx partialTx
            let originalBalance = txBalance originalTx combinedUTxO
            let originalOuts = outputs originalTx
            let classifications =
                    classify
                        (hasZeroAdaOutputs originalTx)
                        "partial tx had zero ada outputs"
                        . classify
                            (hasZeroAdaOutputs originalTx)
                            "partial tx had zero ada outputs"
                        . classify
                            (length originalOuts > 0)
                            "has payment outputs"
                        . classify
                            (length originalOuts > 5)
                            ">5 payment outputs"
                        . classify
                            (length originalOuts > 10)
                            ">10 payment outputs"
                        . classify
                            (length originalOuts > 20)
                            ">20 payment outputs"
                        . classify
                            (length originalOuts > 100)
                            ">100 payment outputs"
            let coverage res =
                    checkCoverage . cover 6 (isRight res) "success"
            let res =
                    testBalanceTx
                        wallet
                        protocolParams
                        timeTranslation
                        seed
                        partialTx
            coverage res $ classifications $ case res of
                Right tx -> counterexample ("\nResult: " <> show (Pretty tx)) $ do
                    label "success"
                        $ classify
                            (originalBalance == mempty)
                            "already balanced"
                        $ classify
                            (txFee tx > Coin 1_000_000)
                            "fee above 1 ada"
                        $ classify
                            (hasInsCollateral tx)
                            "balanced tx has collateral"
                        $ conjoin
                            [ txBalance tx combinedUTxO === mempty
                            , prop_validSize tx combinedUTxO
                            , prop_minfeeIsCovered tx combinedUTxO
                            , let
                                minUTxOValue = Coin 999_978
                                upperBoundCostOfOutput = Coin 1_000
                              in
                                -- Coin selection should only pay more fees than
                                -- required when it can't afford to create a
                                -- change output with the minimumUTxOValue.
                                prop_expectFeeExcessSmallerThan
                                    (minUTxOValue <> upperBoundCostOfOutput)
                                    tx
                                    combinedUTxO
                                    -- FIXME [ADP-2419] Re-enable when we have stricter
                                    -- validation. Will otherwise fail with:
                                    --
                                    -- @
                                    --     --match balanceTx --seed 139473932`
                                    -- @
                                    --
                                    -- , prop_outputsSatisfyMinAdaRequirement tx
                            ]
                Left (ErrBalanceTxAssetsInsufficient err) -> do
                    let shortfall = view #shortfall err
                        shortfallOfAda = Value.coin shortfall /= mempty
                        shortfallOfNonAdaAssets = not (Value.isAdaOnly shortfall)
                    counterexample (show err)
                        $ case (shortfallOfAda, shortfallOfNonAdaAssets) of
                            (False, False) ->
                                -- This case should never occur, as the existence
                                -- of a shortfall implies that we are short of at
                                -- least one asset.
                                property False
                            (True, False) ->
                                label "shortfall of ada"
                                    $ property True
                            (False, True) ->
                                label "shortfall of non-ada assets"
                                    $ property True
                            (True, True) ->
                                label "shortfall of both ada and non-ada assets"
                                    $ property True
                Left (ErrBalanceTxExistingKeyWitnesses _) ->
                    label "existing key wits" $ property True
                Left
                    ( ErrBalanceTxOutputError
                            ( ErrBalanceTxOutputErrorOf
                                    _index
                                    (ErrBalanceTxOutputAdaQuantityInsufficient{})
                                )
                        ) ->
                        label "output below minCoinValue" $ property True
                Left (ErrBalanceTxExistingCollateral) ->
                    label "existing collateral" True
                Left (ErrBalanceTxExistingTotalCollateral) ->
                    label "existing total collateral" True
                Left (ErrBalanceTxExistingReturnCollateral) ->
                    label "existing collateral return outputs" True
                Left ErrBalanceTxMaxSizeLimitExceeded{} ->
                    label "maxTxSize limit exceeded" $ property True
                Left ErrBalanceTxUnableToCreateInput ->
                    label "unable to create input" $ property True
                Left
                    ( ErrBalanceTxInternalError
                            (ErrUnderestimatedFee delta candidateTx nWits)
                        ) ->
                        let counterexampleText =
                                unlines
                                    [ "underestimated fee by "
                                        <> pretty (Convert.toWalletCoin delta)
                                    , "candidate tx: " <> show (Pretty candidateTx)
                                    , "assuming key witness count: " <> show nWits
                                    ]
                        in  counterexample counterexampleText $ property False
                Left
                    ( ErrBalanceTxAssignRedeemers
                            (ErrAssignRedeemersScriptFailure _ e)
                        ) ->
                        prop_transactionScriptFailure e
                Left ErrBalanceTxUnableToCreateChange{} ->
                    label "unable to create change" $ property True
                Left ErrBalanceTxInputResolutionConflicts{} ->
                    label "input resolution conflicts" $ property True
                Left ErrBalanceTxUnresolvedRefunds{} ->
                    label "unresolved refunds" $ property True
                Left err ->
                    label "other error"
                        $ counterexample ("balanceTx failed: " <> show err) False
      where
        BalanceTxArgs
            { protocolParams
            , timeTranslation
            , wallet
            , partialTx
            , seed
            } =
                balanceTxArgs
        Wallet _ walletUTxO _ = wallet

        prop_transactionScriptFailure
            :: TransactionScriptFailure era -> Property
        prop_transactionScriptFailure = \case
            RedeemerPointsToUnknownScriptHash{} ->
                succeedWithLabel "RedeemerPointsToUnknownScriptHash"
            MissingScript{} ->
                succeedWithLabel "MissingScript"
            MissingDatum{} ->
                succeedWithLabel "MissingDatum"
            ValidationFailure{} ->
                succeedWithLabel "ValidationFailure"
            UnknownTxIn{} ->
                succeedWithLabel "UnknownTxIn"
            InvalidTxIn{} ->
                succeedWithLabel "InvalidTxIn"
            IncompatibleBudget{} ->
                succeedWithLabel "IncompatibleBudget"
            NoCostModelInLedgerState{} ->
                succeedWithLabel "NoCostModelInLedgerState"
            ContextError e ->
                case era of
                    RecentEraBabbage -> prop_babbageContextError e
                    RecentEraConway -> prop_conwayContextError e
          where
            prop_babbageContextError :: BabbageContextError era -> Property
            prop_babbageContextError = \case
                AlonzoContextError (TranslationLogicMissingInput _) ->
                    succeedWithLabel "TranslationLogicMissingInput"
                AlonzoContextError (TimeTranslationPastHorizon _) ->
                    succeedWithLabel "TimeTranslationPastHorizon"
                ByronTxOutInContext _ ->
                    succeedWithLabel "ByronTxOutInContext"
                RedeemerPointerPointsToNothing _ ->
                    succeedWithLabel "RedeemerPointerPointsToNothing"
                InlineDatumsNotSupported _ ->
                    succeedWithLabel "InlineDatumsNotSupported"
                ReferenceScriptsNotSupported _ ->
                    succeedWithLabel "ReferenceScriptsNotSupported"
                ReferenceInputsNotSupported _ ->
                    succeedWithLabel "ReferenceInputsNotSupported"

            prop_conwayContextError :: ConwayContextError era -> Property
            prop_conwayContextError = \case
                BabbageContextError e -> prop_babbageContextError e
                CertificateNotSupported _ ->
                    succeedWithLabel "CertificateNotSupported"
                PlutusPurposeNotSupported _ ->
                    succeedWithLabel "PlutusPurposeNotSupported"
                CurrentTreasuryFieldNotSupported _ ->
                    succeedWithLabel "CurrentTreasuryFieldNotSupported"
                VotingProceduresFieldNotSupported _ ->
                    succeedWithLabel "VotingProceduresFieldNotSupported"
                ProposalProceduresFieldNotSupported _ ->
                    succeedWithLabel "ProposalProceduresFieldNotSupported"
                TreasuryDonationFieldNotSupported _ ->
                    succeedWithLabel "TreasuryDonationFieldNotSupported"
                ReferenceInputsNotDisjointFromInputs _ ->
                    succeedWithLabel "ReferenceInputsNotDisjointFromInputs"

        succeedWithLabel l = label l $ property True

        prop_expectFeeExcessSmallerThan
            :: Coin
            -> Tx era
            -> UTxO era
            -> Property
        prop_expectFeeExcessSmallerThan lim tx utxo = do
            let fee = txFee tx
            let minfee = minFee tx utxo
            let delta = fee <-> minfee
            let msg =
                    unwords
                        [ "The fee"
                        , showInParens fee
                        , "was"
                        , show delta
                        , "larger than the minimum fee"
                        , showInParens minfee <> ","
                        , "which is a larger difference than"
                        , show lim
                        ]
            counterexample msg $ property $ delta < lim
          where
            showInParens x = "(" <> show x <> ")"

        prop_minfeeIsCovered
            :: Tx era
            -> UTxO era
            -> Property
        prop_minfeeIsCovered tx utxo = do
            let fee = txFee tx
            let minfee = minFee tx utxo
            let delta = minfee <-> fee
            let msg =
                    unwords
                        [ "The minimum fee was"
                        , show minfee
                        , "but the actual fee,"
                        , show fee
                        , "was lower by"
                        , show delta
                        ]
            counterexample msg $ property $ fee >= minfee

        prop_validSize
            :: Tx era
            -> UTxO era
            -> Property
        prop_validSize tx utxo = do
            let (W.TxSize size) =
                    estimateSignedTxSize
                        protocolParams
                        ( estimateKeyWitnessCounts
                            utxo
                            tx
                            partialTx.timelockKeyWitnessCounts
                        )
                        tx
            let limit = intCast $ protocolParams ^. ppMaxTxSizeL
            let msg =
                    unwords
                        [ "The tx size "
                        , show size
                        , "must be lower than the maximum size "
                        , show limit
                        , ", tx:"
                        , show tx
                        ]
            counterexample msg $ property (size <= limit)

        _prop_outputsSatisfyMinAdaRequirement
            :: Tx era
            -> Property
        _prop_outputsSatisfyMinAdaRequirement tx = do
            conjoin $ map valid (outputs tx)
          where
            valid :: TxOut era -> Property
            valid out =
                counterexample msg
                    $ property
                    $ not
                    $ Write.isBelowMinimumCoinForTxOut protocolParams out
              where
                msg =
                    unwords
                        [ "ada quantity is"
                        , "below minimum requirement"
                        , "\nin\n"
                        , show out
                        , "\n"
                        , "Suggested ada quantity (may overestimate requirement):"
                        , show
                            $ Write.computeMinimumCoinForTxOut
                                protocolParams
                                out
                        ]

        hasZeroAdaOutputs :: Tx era -> Bool
        hasZeroAdaOutputs tx =
            any hasZeroAda (tx ^. bodyTxL . outputsTxBodyL)
          where
            hasZeroAda o = (o ^. coinTxOutL) == Ledger.Coin 0

        minFee
            :: Tx era
            -> UTxO era
            -> Coin
        minFee tx utxo =
            estimateSignedTxMinFee
                protocolParams
                utxo
                tx
                ( estimateKeyWitnessCounts
                    utxo
                    tx
                    partialTx.timelockKeyWitnessCounts
                )

        txBalance
            :: Tx era
            -> UTxO era
            -> Value
        txBalance tx u =
            Write.evaluateTransactionBalance
                protocolParams
                ( lookupStakeKeyDeposit
                    (view #stakeKeyDeposits partialTx)
                    protocolParams
                )
                u
                (tx ^. bodyTxL)

        outputs :: Tx era -> [TxOut era]
        outputs = F.toList . view (bodyTxL . outputsTxBodyL)

{-# ANN
    prop_bootstrapWitnesses
    ("HLint: ignore Eta reduce" :: String)
    #-}
prop_bootstrapWitnesses
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> (Word8 -> Tx era -> Property)
    -> Word8
    -- ^ Number of bootstrap witnesses.
    --
    -- Testing with [0, 255] should be sufficient.
    -> CardanoApi.NetworkId
    -- ^ Network - will be encoded inside the witness.
    -> Index 'WholeDomain 'AccountK
    -- ^ Account index - will be encoded inside the witness.
    -> Index 'WholeDomain 'CredFromKeyK
    -- ^ Index for the first of the 'n' addresses.
    -> Property
prop_bootstrapWitnesses _era p n net accIx addr0Ix =
    let
        -- Start incrementing the ixs upward, and if we reach 'maxBound', loop
        -- around, to ensure we always have 'n' unique indices.
        addrIxs =
            take (fromIntegral n)
                $ [addr0Ix .. maxBound] ++ filter (< addr0Ix) [minBound .. addr0Ix]

        body = mkBasicTxBody

        wits :: [BootstrapWitness]
        wits = map (dummyWitForIx body) addrIxs

        tx =
            mkBasicTx body
                & (witsTxL . bootAddrTxWitsL) .~ Set.fromList wits
    in
        p n tx
  where
    rootK = Byron.generateKeyFromSeed dummyMnemonic mempty
    pwd = mempty

    dummyWitForIx
        :: TxBody era
        -> Index 'WholeDomain 'CredFromKeyK
        -> BootstrapWitness
    dummyWitForIx body ix =
        let
            accK = Byron.deriveAccountPrivateKey pwd rootK accIx
            addrKeyAtIx i = Byron.deriveAddressPrivateKey pwd accK i

            addrK = addrKeyAtIx $ toEnum $ fromEnum ix
            addr = case net of
                CardanoApi.Mainnet ->
                    paymentAddress SMainnet $ over byronKey toXPub addrK
                CardanoApi.Testnet _magic ->
                    -- The choice of network magic here is not important. The
                    -- size of the witness will not be affected by it. What may
                    -- affect the size, is the 'CardanoApi.NetworkId' we pass to
                    -- 'mkByronWitness' above.
                    withSNetworkId (NTestnet 0) $ \testnet ->
                        paymentAddress testnet $ over byronKey toXPub addrK
        in
            mkByronWitness
                body
                net
                addr
                (view byronKey addrK, pwd)

    -- TODO [ADP-2675] Avoid duplication with "Shelley.Transaction"
    -- https://cardanofoundation.atlassian.net/browse/ADP-2675
    mkByronWitness
        :: TxBody era
        -> CardanoApi.NetworkId
        -> W.Address
        -> (XPrv, Passphrase "encryption")
        -> BootstrapWitness
    mkByronWitness body network addr encryptedKey =
        makeBootstrapWitness txHash (decrypt encryptedKey) addrAttr
      where
        txHash = Crypto.castHash $ Crypto.hashWith serialize' body

        decrypt (xprv, pwd') =
            CC.SigningKey
                $ Crypto.HD.xPrvChangePass pwd' BS.empty xprv

        addrAttr =
            Byron.mkAttributes
                $ Byron.AddrAttributes
                    (toHDPayloadAddress addr)
                    (ByronApi.toByronNetworkMagic network)

prop_updateTx
    :: forall era
     . IsRecentEra era
    => Tx era
    -> Set W.TxIn
    -> Set W.TxIn
    -> [W.TxOut]
    -> W.Coin
    -> Property
prop_updateTx tx extraInputs extraCollateral extraOutputs newFee = do
    let extra =
            TxUpdate
                { extraInputs
                , extraCollateral
                , extraOutputs
                , extraInputScripts = []
                , feeUpdate = UseNewTxFee newFee
                }
    let tx' =
            either (error . show) id
                $ updateTx tx extra
    conjoin
        [ inputs tx'
            === inputs tx
                <> Set.map fromWalletTxIn extraInputs
        , outputs tx'
            === (outputs tx)
                <> StrictSeq.fromList (fromWalletTxOut <$> extraOutputs)
        , fee tx' === Convert.toLedger newFee
        , collateralIns tx'
            === collateralIns tx
                <> Set.map fromWalletTxIn extraCollateral
        ]
  where
    inputs = view (bodyTxL . inputsTxBodyL)
    outputs = view (bodyTxL . outputsTxBodyL)
    collateralIns = view (bodyTxL . collateralInputsTxBodyL)
    fee = view (bodyTxL . feeTxBodyL)

prop_mergeSignedValue_invert_swap
    :: (W.TokenBundle, W.TokenBundle) -> Property
prop_mergeSignedValue_invert_swap p@(b1, b2) =
    invert (mergeSignedValue p) === mergeSignedValue (swap p)
        & cover
            10
            (tokenBundleSize b1 > 0 && tokenBundleSize b2 > 0)
            "tokenBundleSize b1 > 0 && tokenBundleSize b2 > 0"

prop_splitSignedValue_invert_swap :: MixedSign Value -> Property
prop_splitSignedValue_invert_swap (MixedSign v) =
    splitSignedValue (invert v) === swap (splitSignedValue v)
        & cover
            10
            (valueHasNegativeAndPositiveParts v)
            "valueHasNegativeAndPositiveParts v"

prop_mergeSignedValue_splitSignedValue
    :: DisjointPair W.TokenBundle -> Property
prop_mergeSignedValue_splitSignedValue (getDisjointPair -> (b1, b2)) =
    (splitSignedValue . mergeSignedValue) (b1, b2) === (b1, b2)
        & cover
            10
            (tokenBundleSize b1 > 0 && tokenBundleSize b2 > 0)
            "tokenBundleSize b1 > 0 && tokenBundleSize b2 > 0"

prop_splitSignedValue_mergeSignedValue :: MixedSign Value -> Property
prop_splitSignedValue_mergeSignedValue (MixedSign v) =
    (mergeSignedValue . splitSignedValue) v === v
        & cover
            10
            (valueHasNegativeAndPositiveParts v)
            "valueHasNegativeAndPositiveParts v"

--------------------------------------------------------------------------------
-- Arguments for balanceTx
--------------------------------------------------------------------------------

-- | A set of arguments for the 'balanceTx' function.
data BalanceTxArgs era = BalanceTxArgs
    { wallet :: !(Wallet era)
    , protocolParams :: !(Write.PParams era)
    , timeTranslation :: !TimeTranslation
    , seed :: !StdGenSeed
    , partialTx :: !(PartialTx era)
    }
    deriving stock (Generic, Show)

-- | Applies the 'balanceTx' function to the given arguments.
applyBalanceTxArgs
    :: IsRecentEra era
    => BalanceTxArgs era
    -> Either (ErrBalanceTx era) (Tx era)
applyBalanceTxArgs
    (BalanceTxArgs wallet protocolParams timeTranslation seed partialTx) =
        (testBalanceTx wallet protocolParams timeTranslation seed partialTx)

-- | A set of arguments that will always lead to success.
newtype Success a = Success a
    deriving newtype (Show)

-- | A set of arguments that can either lead to success or to failure.
newtype SuccessOrFailure a = SuccessOrFailure a
    deriving newtype (Show)

instance IsRecentEra era => Arbitrary (Success (BalanceTxArgs era)) where
    arbitrary = coerce genBalanceTxArgsForSuccess
    shrink = coerce shrinkBalanceTxArgsForSuccess

instance
    IsRecentEra era
    => Arbitrary (SuccessOrFailure (BalanceTxArgs era))
    where
    arbitrary = coerce genBalanceTxArgsForSuccessOrFailure
    shrink = coerce shrinkBalanceTxArgsForSuccessOrFailure

genBalanceTxArgsForSuccess
    :: forall era
     . IsRecentEra era
    => Gen (BalanceTxArgs era)
genBalanceTxArgsForSuccess =
    -- For the moment, we use the brute force tactic of repeatedly generating
    -- arguments until we have a set of arguments that leads to success:
    genBalanceTxArgsForSuccessOrFailure
        `suchThat` (isRight . applyBalanceTxArgs)

shrinkBalanceTxArgsForSuccess
    :: forall era
     . IsRecentEra era
    => BalanceTxArgs era
    -> [BalanceTxArgs era]
shrinkBalanceTxArgsForSuccess =
    filter (isRight . applyBalanceTxArgs)
        . shrinkBalanceTxArgsForSuccessOrFailure

genBalanceTxArgsForSuccessOrFailure
    :: forall era
     . IsRecentEra era
    => Gen (BalanceTxArgs era)
genBalanceTxArgsForSuccessOrFailure =
    BalanceTxArgs
        <$> arbitrary @(Wallet era)
        <*> genProtocolParams
        <*> genTimeTranslation
        <*> arbitrary @StdGenSeed
        <*> arbitrary @(PartialTx era)
  where
    genProtocolParams = pure mockPParams
    genTimeTranslation = pure dummyTimeTranslation

shrinkBalanceTxArgsForSuccessOrFailure
    :: forall era
     . IsRecentEra era
    => BalanceTxArgs era
    -> [BalanceTxArgs era]
shrinkBalanceTxArgsForSuccessOrFailure =
    genericRoundRobinShrink
        <@> shrink @(Wallet era)
        <:> shrinkProtocolParams
        <:> shrinkTimeTranslation
        <:> shrink @StdGenSeed
        <:> shrink @(PartialTx era)
        <:> Nil
  where
    shrinkProtocolParams = const []
    shrinkTimeTranslation = const []

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

-- | Encapsulates both a 'ChangeAddressGen s' and the 's' required for the
-- generator. This allows properties like 'prop_balanceTxValid' to
-- easily generate arbitrary change address generators.
data AnyChangeAddressGenWithState where
    AnyChangeAddressGenWithState
        :: forall s
         . ChangeAddressGen s
        -> s
        -> AnyChangeAddressGenWithState

data BalanceTxGolden
    = BalanceTxGoldenSuccess
        Coin
        -- ^ Wallet balance
        Coin
        -- ^ Fee
        Coin
        -- ^ Minimum fee
    | BalanceTxGoldenFailure Coin String
    deriving (Eq, Show)

newtype DummyChangeState = DummyChangeState {nextUnusedIndex :: Int}
    deriving (Show, Eq)

-- | Encapsulates a value that may be negative or positive or a mixture of both.
newtype MixedSign a = MixedSign a
    deriving (Eq, Show)

data Wallet era
    = Wallet UTxOAssumptions (UTxO era) AnyChangeAddressGenWithState
    deriving (Show) via ShowBuildable (Wallet era)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- Ideally merge with 'updateTx'
addExtraTxIns
    :: IsRecentEra era
    => [W.TxIn]
    -> PartialTx era
    -> PartialTx era
addExtraTxIns extraIns =
    #tx . bodyTxL . inputsTxBodyL %~ (<> toLedgerInputs extraIns)
  where
    toLedgerInputs =
        Set.map Convert.toLedger . Set.fromList

-- | Wrapper for testing convenience. Does hide the monad 'm', tracing, and the
-- updated 'changeState'. Does /not/ specify mock values for things like
-- protocol parameters. This is up to the caller to provide.
testBalanceTx
    :: forall era
     . IsRecentEra era
    => Wallet era
    -> Write.PParams era
    -> TimeTranslation
    -> StdGenSeed
    -> PartialTx era
    -> Either (ErrBalanceTx era) (Tx era)
testBalanceTx
    ( Wallet
            utxoAssumptions
            utxo
            (AnyChangeAddressGenWithState genChangeAddr s)
        )
    protocolParameters
    timeTranslation
    seed
    partialTx =
        (`evalRand` stdGenFromSeed seed) $ runExceptT $ do
            (transactionInEra, _nextChangeState) <-
                balanceTx
                    protocolParameters
                    timeTranslation
                    utxoAssumptions
                    utxoIndex
                    genChangeAddr
                    s
                    partialTx
            pure transactionInEra
      where
        utxoIndex = constructUTxOIndex utxo

-- | Also returns the updated change state
balanceTxWithDummyChangeState
    :: forall era
     . IsRecentEra era
    => UTxOAssumptions
    -> UTxO era
    -> StdGenSeed
    -> PartialTx era
    -> Either
        (ErrBalanceTx era)
        (Tx era, DummyChangeState)
balanceTxWithDummyChangeState utxoAssumptions utxo seed partialTx =
    (`evalRand` stdGenFromSeed seed)
        $ runExceptT
        $ balanceTx
            mockPParams
            dummyTimeTranslation
            utxoAssumptions
            utxoIndex
            dummyChangeAddrGen
            (DummyChangeState 0)
            partialTx
  where
    utxoIndex = constructUTxOIndex utxo

fromWalletTxIn :: W.TxIn -> TxIn
fromWalletTxIn = Convert.toLedger

fromWalletTxOut :: forall era. IsRecentEra era => W.TxOut -> TxOut era
fromWalletTxOut = case recentEra @era of
    RecentEraBabbage -> Convert.toBabbageTxOut
    RecentEraConway -> Convert.toConwayTxOut

hasInsCollateral
    :: forall era
     . IsRecentEra era
    => Tx era
    -> Bool
hasInsCollateral =
    not . Set.null . view (bodyTxL . collateralInputsTxBodyL)

hasReturnCollateral
    :: forall era
     . IsRecentEra era
    => Tx era
    -> Bool
hasReturnCollateral tx =
    case tx ^. bodyTxL . collateralReturnTxBodyL of
        SJust _ -> True
        SNothing -> False

hasTotalCollateral
    :: forall era
     . IsRecentEra era
    => Tx era
    -> Bool
hasTotalCollateral tx =
    case tx ^. bodyTxL . totalCollateralTxBodyL of
        SJust _ -> True
        SNothing -> False

mkTestWallet :: IsRecentEra era => UTxO era -> Wallet era
mkTestWallet utxo =
    Wallet AllKeyPaymentCredentials utxo dummyShelleyChangeAddressGen

paymentPartialTx :: IsRecentEra era => [TxOut era] -> PartialTx era
paymentPartialTx txouts =
    PartialTx
        (mkBasicTx body)
        mempty
        mempty
        (StakeKeyDepositMap mempty)
        mempty
  where
    body =
        mkBasicTxBody
            & outputsTxBodyL
                .~ StrictSeq.fromList txouts

serializedSize
    :: forall era
     . IsRecentEra era
    => Tx era
    -> Int
serializedSize = BS.length . serializeTx

-- | Checks for membership in the given closed interval [a, b]
shouldBeInclusivelyWithin :: (Ord a, Show a) => a -> (a, a) -> IO ()
x `shouldBeInclusivelyWithin` (a, b) =
    if a <= x && x <= b
        then pure ()
        else
            expectationFailure
                $ unwords
                    [ show x
                    , "not in the expected interval"
                    , "[" <> show a <> ", " <> show b <> "]"
                    ]

tokenBundleSize :: W.TokenBundle -> Int
tokenBundleSize = Set.size . W.TokenBundle.getAssets

valueHasNegativeAndPositiveParts :: Value -> Bool
valueHasNegativeAndPositiveParts v =
    tokenBundleSize b1 > 0 && tokenBundleSize b2 > 0
  where
    (b1, b2) = splitSignedValue v

withValidityInterval
    :: IsRecentEra era
    => ValidityInterval
    -> PartialTx era
    -> PartialTx era
withValidityInterval vi = #tx . bodyTxL %~ vldtTxBodyL .~ vi

cardanoToWalletTxOut
    :: forall era
     . IsRecentEra era
    => CardanoApi.TxOut CardanoApi.CtxUTxO (CardanoApiEra era)
    -> W.TxOut
cardanoToWalletTxOut =
    toWallet . CardanoApi.toShelleyTxOut shelleyBasedEra
  where
    shelleyBasedEra = shelleyBasedEraFromRecentEra (recentEra @era)

    toWallet :: TxOut era -> W.TxOut
    toWallet x = case recentEra @era of
        RecentEraBabbage -> Convert.fromBabbageTxOut x
        RecentEraConway -> Convert.fromConwayTxOut x

txFee :: IsRecentEra era => Tx era -> Coin
txFee tx = tx ^. bodyTxL . feeTxBodyL

-- | Construct a dummy 'UTxO era' where all inputs of the 'Tx era' resolve to
-- outputs with the given 'Address'.
utxoPromisingInputsHaveAddress
    :: forall era
     . (HasCallStack, IsRecentEra era)
    => Address
    -> Tx era
    -> UTxO era
utxoPromisingInputsHaveAddress addr tx =
    unsafeUtxoFromTxOutsInRecentEra [(i, txOut) | i <- allInputs tx]
  where
    allInputs :: Tx era -> [TxIn]
    allInputs body = Set.toList $ body ^. (bodyTxL . allInputsTxBodyF)

    txOut :: TxOutInRecentEra
    txOut = TxOutInRecentEra addr mempty NoDatum Nothing

ada :: Integer -> Value
ada a = Value.inject (Coin $ a * 1_000_000)

lovelace :: Integer -> Value
lovelace l = Value.inject $ Coin l

--------------------------------------------------------------------------------
-- Test values
--------------------------------------------------------------------------------

dummyChangeAddrGen :: ChangeAddressGen DummyChangeState
dummyChangeAddrGen =
    ChangeAddressGen
        { genChangeAddress = \(DummyChangeState i) ->
            (addressAtIx $ CA.unsafeMkIndex $ toEnum i, DummyChangeState $ succ i)
        , maxLengthChangeAddress = addressAtIx minBound
        }
  where
    rootK = Shelley.genMasterKeyFromMnemonic dummyMnemonic mempty
    accK = Shelley.deriveAccountPrivateKey rootK minBound
    stakeK = CA.toXPub <$> Shelley.deriveDelegationPrivateKey accK

    addressAtIx
        :: CA.Index 'CA.Soft 'CA.PaymentK
        -> Write.Address
    addressAtIx ix =
        Convert.toLedgerAddress
            $ convert
            $ Shelley.delegationAddress
                Shelley.shelleyMainnet
                (Shelley.PaymentFromExtendedKey paymentK)
                (Shelley.DelegationFromExtendedKey stakeK)
      where
        paymentK =
            CA.toXPub
                <$> Shelley.deriveAddressPrivateKey accK Shelley.UTxOInternal ix

    convert = W.Address . CA.unAddress

dummyMnemonic :: SomeMnemonic
dummyMnemonic =
    SomeMnemonic
        $ either
            (error . show)
            id
            (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")

-- Byron style addresses, corresponding to the change addresses generated by
-- "byron wallets".
dummyByronChangeAddressGen :: AnyChangeAddressGenWithState
dummyByronChangeAddressGen =
    AnyChangeAddressGenWithState
        dummyChangeAddressGen
        (mkRndState byronRootK 0)
  where
    byronRootK = Byron.generateKeyFromSeed mw mempty
    mw =
        SomeMnemonic
            $ either
                (error . show)
                id
                (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")
    pwd = mempty

    dummyChangeAddressGen :: ChangeAddressGen (RndState 'Mainnet)
    dummyChangeAddressGen =
        ChangeAddressGen
            (first Convert.toLedgerAddress <$> genChange (byronRootK, pwd))
            (Convert.toLedgerAddress maxLengthAddressForByron)

-- | Shelley base addresses, corresponding to the change addresses generated by
-- normal shelley wallets.
dummyShelleyChangeAddressGen :: AnyChangeAddressGenWithState
dummyShelleyChangeAddressGen =
    AnyChangeAddressGenWithState
        dummyChangeAddrGen
        (DummyChangeState 0)

dummyTimeTranslation :: TimeTranslation
dummyTimeTranslation =
    timeTranslationFromEpochInfo
        (Slotting.SystemStart (posixSecondsToUTCTime 0))
        ( Slotting.fixedEpochInfo
            (Slotting.EpochSize 21_600)
            (Slotting.slotLengthFromSec 1)
        )

-- | A dummy 'TimeTranslation' for testing 'PastHorizonException's.
dummyTimeTranslationWithHorizon :: SlotNo -> TimeTranslation
dummyTimeTranslationWithHorizon horizon =
    timeTranslationFromEpochInfo systemStart epochInfo
  where
    slotLength = 1
    t0 = HF.initBound
    t1 =
        HF.Bound
            (RelativeTime $ fromIntegral $ slotLength * (unSlotNo horizon))
            horizon
            (CardanoApi.EpochNo 1)

    era1Params =
        HF.defaultEraParams (SecurityParam (unsafeNonZero 2)) (mkSlotLength 1)
    summary =
        HF.summaryWithExactly
            (exactlyOne (HF.EraSummary t0 (HF.EraEnd t1) era1Params))

    systemStart :: Slotting.SystemStart
    systemStart = Slotting.SystemStart (posixSecondsToUTCTime 0)

    epochInfo :: Slotting.EpochInfo (Either PastHorizonException)
    epochInfo =
        Slotting.hoistEpochInfo
            runExcept
            (HF.interpreterToEpochInfo (HF.mkInterpreter summary))

mainnetFeePerByte :: FeePerByte
mainnetFeePerByte = FeePerByte 44

pingPong_1 :: IsRecentEra era => PartialTx era
pingPong_1 = PartialTx tx mempty mempty (StakeKeyDepositMap mempty) mempty
  where
    tx =
        deserializeTx
            $ unsafeFromHex
            $ mconcat
                [ "84a30080018183581d714d72cf569a339a18a7d9302313983f56e0d96cd4"
                , "5bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04ba"
                , "bed17320d8d1b9ff9ad086e86f44ec0200a10481d87980f5f6"
                ]

pingPong_2 :: IsRecentEra era => PartialTx era
pingPong_2 =
    PartialTx
        { tx =
            deserializeTx
                $ mconcat
                    [ unsafeFromHex "84a30081825820"
                    , tid
                    , unsafeFromHex
                        "00018183581d714d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e848058208392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e0200a20381591b72591b6f01000033233332222333322223322332232323332223233322232333333332222222232333222323333222232323322323332223233322232323322332232323333322222332233223322332233223322223223223232533530333330083333573466e1d40192004204f23333573466e1d401d2002205123333573466e1d40212000205323504b35304c3357389201035054310004d49926499263333573466e1d40112004205323333573466e1d40152002205523333573466e1d40192000205723504b35304c3357389201035054310004d49926499263333573466e1cd55cea8012400046601664646464646464646464646666ae68cdc39aab9d500a480008cccccccccc064cd409c8c8c8cccd5cd19b8735573aa004900011980f981d1aba15002302c357426ae8940088d4164d4c168cd5ce2481035054310005b49926135573ca00226ea8004d5d0a80519a8138141aba150093335502e75ca05a6ae854020ccd540b9d728169aba1500733502704335742a00c66a04e66aa0a8098eb4d5d0a8029919191999ab9a3370e6aae754009200023350213232323333573466e1cd55cea80124000466a05266a084eb4d5d0a80118239aba135744a00446a0ba6a60bc66ae712401035054310005f49926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502733504275a6ae854008c11cd5d09aba2500223505d35305e3357389201035054310005f49926135573ca00226ea8004d5d09aba2500223505935305a3357389201035054310005b49926135573ca00226ea8004d5d0a80219a813bae35742a00666a04e66aa0a8eb88004d5d0a801181c9aba135744a00446a0aa6a60ac66ae71241035054310005749926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea00290031180f181d9aba135573ca00646666ae68cdc3a801240084603a608a6ae84d55cf280211999ab9a3370ea00690011180e98181aba135573ca00a46666ae68cdc3a80224000460406eb8d5d09aab9e50062350503530513357389201035054310005249926499264984d55cea80089baa001357426ae8940088d4124d4c128cd5ce249035054310004b49926104a1350483530493357389201035054350004a4984d55cf280089baa001135573a6ea80044d55ce9baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa078446666aae7c004940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06c6a606e66ae71241035054310003849926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235032353033335738921035054310003449926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540d888c8cccd55cf80112804919a80419aa81898031aab9d5002300535573ca00460086ae8800c0b84d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c490103505431000244992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce249035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55cea80089baa001212230020032122300100320011122232323333573466e1cd55cea80124000466aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004448848cc00400c008448004488800c488800848880048004488800c488800848880048004448c8c00400488cc00cc008008004c8c8cc88cc88c8ccc888c8c8c8c8c8ccc888ccc888ccc888c8cccc8888c8cc88c8cccc8888c8cc88c8cc88c8ccc888c8c8cc88c8c8cc88c8c8c8cccc8888c8c8c8c8c8cc88c8cc88cc88ccccccccccccc8888888888888c8c8c8c8c8cccccccc88888888cc88cc88cc88cc88c8ccccc88888c8cc88cc88cc88c8cc88cc88cc88c8cc88c8c8c8cccc8888cccc8888c8888d4d540400108888c8c8c94cd4c24004ccc0140280240205400454cd4c24004cd5ce249025331000910115001109101153353508101003215335309001333573466e1cccc109400cd4c07800488004c0580212002092010910115002153353090013357389201025332000910115002109101150011533535080013300533501b00833303e03f5001323355306012001235355096010012233550990100233553063120012353550990100122335509c0100233704900080080080099a809801180a003003909a9aa84a8080091911a9a80f00091299a984a0098050010a99a984a00999aa9837090009a835283491a9aa84d8080091199aa9838890009a836a83611a9aa84f0080091199ab9a3370e900000084e0084d808008008a8020a99a984a0099ab9c49102533300095011500410950113535501e00522253353097013333355027253335301400113374a90001bb14984cdd2a40046ec52613374a90021bb149800c008c8cd400541d141d4488cc008cd40ac01cccc124128018cd4078034c07c04400403c4264044cd5ce249025335000980113535501a0012225335309301333335502325301d00100300200100b109501133573892010253340009401133573892010253360008f0113530220052235302d002222222222253353508b013303000a00b2135303a0012235303e0012220021350a10135309d0133573892010253300009e01498cccd5403488d4d404c008894ccd4c02400c54ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f054ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f04d41f4cd542400554034cd4058019419894ccd4c008004421c04421c044220048882280541e0488800c488800848880048004488800c48880084888004800444ccd5401d416541654164494cd4d41b8004848cd4168cd5421404d4c03000888004cd4168cd54214040052002505b505b12505a235355081013530100012235301b00222222222225335350793301e00a00b213530280012235302c00122235303100322335308701002230930116253353508201004213355098010020011309301161308a01162200211222212333300100500400300211200120011122212333001004003002112001122123300100300212001221233001003002200111222225335307533355304f120013504b504a235300b002223301500200300415335307533355304f120013504b504a235300b002223530160022222222222353501500d22533530840133355305e120013505450562353025001223304b00200400c10860113357389201024c30000850100315335307533355304f120013504b504a235300b002223530160022222222222353501300d22533530840133355305e12001350545056235302700122253353507a00121533530890133305108501003006153353507b330623019007009213308501001002108a01108a011089015335350763301b00c00d2135302500122353029001222333553055120012235302e00222235303300822353035005225335309301333308401004003002001133506f0090081008506701113508c01353088013357389201024c6600089014984218044cd5ce2481024c3100085010021077150741507415074122123300100300212001122123300100300212001221233001003002200122533335300300121505f21505f21505f2133355304612001504a235300d001225335306f3303300200413506300315062003212222300400521222230030052122223002005212222300100520013200135506c22233333333333353019001235300500322222222225335307153353506333355304b12001504f253353072333573466e3c0300041d01cc4d41980045419400c841d041c841cc4cd5ce249024c340007222353006004222222222253353506453353506433355304c1200150502353550790012253353075333573466e3c00803c1dc1d84d41a400c541a000884d419cd4d541e40048800454194854cd4c1ccccd5cd19baf00100c0750741075150701506f235300500322222222225335307133355304b120013504150432333573466ebc0300041d01cccd54c108480048d4d541e00048800400841cc4cd5ce249024c320007222225335306a333573466e1cd4c0200188888888888ccc09801c0380300041b01ac41b04cd5ce2481024c390006b22235300700522222222225335307333355304d1200135043504523530160012225335350690012153353078333040074003010153353506a35301601422222222223305b01b0022153353079333573466e3c0040081ec1e84d4c07401488cccc1b0008004c1d005541b841e841e441e441e002441d44cd5ce249024c6200074225335306833303002f0013335530331200150175045353006004222222222233355303d120012235301600222235301b00322335307100225335307a333573466e3c0500041f01ec4cd415801401c401c801d413c02441a84cd5ce2481024c610006925335306733302f02e001353005003222222222233355304b12001501f235301400122200200910691335738921024c36000682533530673335530411200135037503923300500400100110691335738921024c640006825335306733302f02e001353005003222222222233355304b12001501f23530120012235301600122200200a106913357389201024c35000682353005003222222222253353506333355304b12001504f235301200122533530743303800200e1350680031506700a213530120012235301600122253353506900121507610791506f22353006004222222222253353506433355304c120015050235301300122533530753303900200f1350690031506800a2107513357389201024c380007323530050032222222222353503100b22353503500222353503500822353503900222533530793333333222222253335306d33350640070060031533530800100215335308001005133350610070010041081011333506100700100410810113335061007001004333333335064075225335307b333573466e1c0080041f41f041ac54cd4c1ecccd5cd19b8900200107d07c1069106a22333573466e200080041f41f010088ccd5cd19b8900200107c07d22333573466e200080041f01f4894cd4c1ecccd5cd19b8900200107d07c10011002225335307b333573466e240080041f41f04008400401801401c00800400c41ec4cd5ce249024c330007a222222222212333333333300100b00a009008007006005004003002200122123300100300220012221233300100400300220012212330010030022001212222222300700822122222223300600900821222222230050081222222200412222222003221222222233002009008221222222233001009008200113350325001502f13001002222335530241200123535505a00122335505d002335530271200123535505d001223355060002333535502500123300a4800000488cc02c0080048cc02800520000013301c00200122337000040024446464600200a640026aa0b64466a6a05e0029000111a9aa82e00111299a982c199ab9a3371e0040120b40b22600e0022600c006640026aa0b44466a6a05c0029000111a9aa82d80111299a982b999ab9a3371e00400e0b20b020022600c00642444444444444601801a4424444444444446601601c01a42444444444444601401a44442444444444444666601202001e01c01a444244444444444466601001e01c01a4424444444444446600e01c01a42444444444444600c01a42444444444444600a01a42444444444444600801a42444444444444600601a4424444444444446600401c01a42444444444444600201a400224424660020060042400224424660020060042400244a66a607c666ae68cdc79a9801801110011a98018009100102001f8999ab9a3370e6a6006004440026a60060024400208007e207e442466002006004400244666ae68cdc480100081e81e111199aa980a890009a808a80811a9aa82100091199aa980c090009a80a280991a9aa82280091199a9aa8068009198052400000244660160040024660140029000000998020010009119aa98050900091a9aa8200009119aa821801199a9aa804000919aa98070900091a9aa8220009119aa8238011aa80780080091199aaa80401c801000919aa98070900091a9aa8220009119aa8238011aa806800800999aaa80181a001000888911199aa980209000a80a99aa98050900091a9aa8200009119aa8218011aa805800999aa980209000911a9aa82080111299a981e999aa980b890009a806a80791a9aa82200091198050010028030801899a80c802001a80b00099aa98050900091a9aa820000911919aa8220019800802990009aa82291299a9a80c80089aa8058019109a9aa82300111299a982119806001004099aa80800380089803001801190009aa81f1108911299a9a80a800880111099802801199aa980389000802802000889091118018020891091119801002802089091118008020890008919a80891199a9a803001910010010009a9a80200091000990009aa81c110891299a9a8070008a80811099a808980200119aa980309000802000899a80111299a981800108190800817891091980080180109000899a80191299a9816801080088170168919a80591199a9a802001910010010009a9a8010009100089109198008018010900091299a9a80d999aa980189000a80391a9aa81800091299a9816199ab9a3375e00200a05c05a26a0400062a03e002426a03c6a6aa060002440042a038640026aa05e4422444a66a6a00c00226a6a01400644002442666a6a01800a440046008004666aa600e2400200a00800222440042442446600200800624002266a00444a66a6a02c004420062002a02a24424660020060042400224446a6a008004446a6a00c00644a666a6026666a01400e0080042a66a604c00620022050204e2050244246600200600424002244464646464a666a6a01000c42a666a6a01200c42a666a6a0140104260082c260062c2a666a6a01400e4260082c260062c202a20262a666a6a01200e4260082c260062c2a666a6a01200c4260082c260062c20282a666a6a01000a42024202620222a666a6a01000a42a666a6a01200e42600a2c260082c2a666a6a01200c42600a2c260082c202820242a666a6a01000c42600a2c260082c2a666a6a01000a42600a2c260082c20264a666a6a01000a42a666a6a01200e42a666a6a01400e42666a01e014004002260222c260222c260202c20262a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c202420224a666a6a00e00842a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c20242a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c202220204a666a6a00c00642a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c20222a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c2020201e4a666a6a00a00442a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c20202a666a6a00a00642a666a6a00c00642666a01600c0040022601a2c2601a2c260182c201e201c2424446006008224440042244400224002246a6a0040024444444400e244444444246666666600201201000e00c00a008006004240024c244400624440042444002400244446466a601800a466a601a0084a66a602c666ae68cdc780100080c00b8a801880b900b919a9806802100b9299a980b199ab9a3371e00400203002e2a006202e2a66a6a00a00642a66a6a00c0044266a6014004466a6016004466a601e004466a60200044660280040024034466a6020004403446602800400244403444466a601a0084034444a66a6036666ae68cdc380300180e80e0a99a980d999ab9a3370e00a00403a03826602e00800220382038202a2a66a6a00a0024202a202a2424460040062244002240024244600400644424466600200a00800640024244600400642446002006400244666ae68cdc780100080480411199ab9a3370e00400201000e266ae712401024c630000413357389201024c370000313357389201024c64000021220021220012001235006353002335738921024c6700003498480048004448848cc00400c008448004498448c8c00400488cc00cc0080080050482d87a80d87980f5f6"
                    ]
        , extraUTxO =
            Write.unsafeUtxoFromTxOutsInRecentEra
                [
                    ( Write.unsafeMkTxIn tid 0
                    , TxOutInRecentEra
                        ( Write.unsafeAddressFromBytes
                            $ unsafeFromHex
                            $ mconcat
                                [ "714d72cf569a339a18a7d93023139"
                                , "83f56e0d96cd45bdcb1d6512dca6a"
                                ]
                        )
                        (ada 2)
                        ( Write.DatumHash
                            $ fromJust
                            $ Write.datumHashFromBytes
                            $ unsafeFromHex
                            $ mconcat
                                [ "923918e403bf43c34b4ef6b48eb2ee04"
                                , "babed17320d8d1b9ff9ad086e86f44ec"
                                ]
                        )
                        Nothing
                    )
                ]
        , stakeKeyDeposits = StakeKeyDepositMap mempty
        , redeemers =
            [ RedeemerSpending
                (unsafeFromHex "D87A80")
                (Convert.toLedger (W.TxIn (W.Hash tid) 0))
            ]
        , timelockKeyWitnessCounts = mempty
        }
  where
    tid = B8.replicate 32 '1'

-- | A collection of signed transaction bytestrings useful for testing.
--
-- These bytestrings can be regenerated by running the integration tests
-- with lib/unit/test/data/signedTxs/genData.patch applied.
signedTxTestData :: IO [(FilePath, ByteString)]
signedTxTestData = do
    let dir = $(getTestData) </> "signedTxs"
    files <- listDirectory dir
    fmap (sortOn (goldenIx . fst) . catMaybes) . forM files $ \name ->
        if ".cbor" `isSuffixOf` name
            then Just . (name,) <$> BS.readFile (dir </> name)
            else pure Nothing
  where
    goldenIx :: FilePath -> Maybe Int
    goldenIx = readMaybe . takeWhile isDigit

testParameter_coinsPerUTxOByte :: Ledger.CoinPerByte
testParameter_coinsPerUTxOByte =
    Ledger.CoinPerByte $ Coin 4_310

testStdGenSeed :: StdGenSeed
testStdGenSeed = StdGenSeed 0

--------------------------------------------------------------------------------
-- Arbitrary instances, generators, and shrinkers
--------------------------------------------------------------------------------

instance Arbitrary AnyRecentEra where
    arbitrary =
        elements
            [ AnyRecentEra RecentEraBabbage
            , AnyRecentEra RecentEraConway
            ]

instance
    CardanoApi.IsCardanoEra era
    => Arbitrary (CardanoApi.AddressInEra era)
    where
    arbitrary = CardanoApi.genAddressInEra CardanoApi.cardanoEra

instance
    CardanoApi.IsCardanoEra era
    => Arbitrary (CardanoApi.TxOutDatum ctx era)
    where
    arbitrary = CardanoApi.genTxOutDatum CardanoApi.cardanoEra

instance Arbitrary CardanoApi.NetworkId where
    arbitrary =
        oneof
            [ pure CardanoApi.Mainnet
            , CardanoApi.Testnet . CardanoApi.NetworkMagic <$> arbitrary
            ]

-- Coins (quantities of lovelace) must be strictly positive when included in
-- transactions.
--
instance Arbitrary W.Coin where
    arbitrary = W.genCoinPositive
    shrink = W.shrinkCoinPositive

instance Arbitrary FeePerByte where
    arbitrary =
        frequency
            [ (1, pure mainnetFeePerByte)
            , (7, FeePerByte <$> arbitrarySizedNatural)
            ]
    shrink (FeePerByte x) =
        FeePerByte <$> shrinkNatural x

instance Arbitrary (W.Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ W.Hash $ BS.pack bs

instance Arbitrary (Index 'WholeDomain depth) where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

-- At the time of writing, the 'Arbitrary' instance for 'Value' only generates
-- values without any negative components.
--
-- In order to allow for generation of values that may have both negative and
-- positive components, use the following instance:
--
instance Arbitrary (MixedSign Value) where
    arbitrary = MixedSign <$> ((<>) <$> genNegative <*> genPositive)
      where
        genNegative = arbitrary <&> invert
        genPositive = arbitrary
    shrink (MixedSign v) = MixedSign <$> shrink v

instance IsRecentEra era => Arbitrary (PartialTx era) where
    arbitrary = do
        (ptx :: PartialTx era) <- partialTxFromTxWithUTxO <$> genTxWithUTxO
        let deregCerts = F.toList $ stakeCredentialsWithRefunds $ view #tx ptx
        keyDeposits <-
            (StakeKeyDepositMap . Map.fromList)
                <$> (occasionallyDropElems =<< mapM (\x -> (x,) <$> genDeposit) deregCerts)
        return $ ptx{stakeKeyDeposits = keyDeposits}
      where
        genDeposit :: Gen Coin
        genDeposit =
            oneof
                [ Coin . (1_000_000 *) . intCast . getNonNegative
                    <$> arbitrary @(NonNegative Int)
                , CardanoApi.genCoin
                ]
        occasionallyDropElems :: forall a. [a] -> Gen [a]
        occasionallyDropElems l = do
            -- Probability of dropping an element ≈ 1/50 or 2%
            ixToDrop <- choose (0, 50 * length l)
            return $ filterByIx (/= ixToDrop) l

        filterByIx :: forall a. (Int -> Bool) -> [a] -> [a]
        filterByIx f = map snd . filter (\(ix, _) -> f ix) . zip [0 ..]

    shrink =
        shrinkMapBy
            partialTxFromTxWithUTxO
            txWithUTxOFromPartialTx
            shrinkTxWithUTxO

partialTxFromTxWithUTxO
    :: IsRecentEra era => TxWithUTxO era -> PartialTx era
partialTxFromTxWithUTxO (TxWithUTxO tx extraUTxO) =
    PartialTx
        { tx
        , extraUTxO
        , redeemers
        , timelockKeyWitnessCounts
        , stakeKeyDeposits
        }
  where
    -- This embedding uses the following constants:
    redeemers = []
    timelockKeyWitnessCounts = mempty
    stakeKeyDeposits = StakeKeyDepositAssumeCurrent

txWithUTxOFromPartialTx
    :: IsRecentEra era => PartialTx era -> TxWithUTxO era
txWithUTxOFromPartialTx PartialTx{tx, extraUTxO} =
    TxWithUTxO.constructFiltered tx extraUTxO

genTxWithUTxO :: IsRecentEra era => Gen (TxWithUTxO era)
genTxWithUTxO = TxWithUTxO.generate genTxForBalancing genTxIn genTxOut

shrinkTxWithUTxO
    :: IsRecentEra era => TxWithUTxO era -> [TxWithUTxO era]
shrinkTxWithUTxO = TxWithUTxO.shrinkWith shrinkTx shrinkUTxOToSubsets
  where
    shrinkUTxOToSubsets :: IsRecentEra era => UTxO era -> [UTxO era]
    shrinkUTxOToSubsets = shrinkMapBy UTxO unUTxO shrinkMapToSubmaps

instance Arbitrary StdGenSeed where
    arbitrary = StdGenSeed . fromIntegral @Int <$> arbitrary

instance Arbitrary W.TokenBundle where
    arbitrary = W.genTokenBundleSmallRange
    shrink = W.shrinkTokenBundleSmallRange

instance Arbitrary (DisjointPair W.TokenBundle) where
    arbitrary = genDisjointPair W.genTokenBundle
    shrink = shrinkDisjointPair W.shrinkTokenBundle

instance Arbitrary W.TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ W.TxIn txId ix

instance Arbitrary W.TxOut where
    arbitrary =
        W.TxOut addr <$> scale (`mod` 4) W.genTokenBundleSmallRange
      where
        addr = W.Address $ BS.pack (1 : replicate 56 0)
    shrink (W.TxOut addr bundle) =
        [ W.TxOut addr bundle'
        | bundle' <- W.shrinkTokenBundleSmallRange bundle
        ]

instance forall era. IsRecentEra era => Arbitrary (Wallet era) where
    arbitrary =
        oneof
            [ Wallet AllKeyPaymentCredentials
                <$> genWalletUTxO genShelleyVkAddr
                <*> pure dummyShelleyChangeAddressGen
            , Wallet AllByronKeyPaymentCredentials
                <$> genWalletUTxO genByronVkAddr
                <*> pure dummyByronChangeAddressGen
            ]
      where
        genShelleyVkAddr :: Gen (CardanoApi.AddressInEra (CardanoApiEra era))
        genShelleyVkAddr =
            fmap (CardanoApi.shelleyAddressInEra era)
                $ CardanoApi.makeShelleyAddress
                    <$> CardanoApi.genNetworkId
                    <*> CardanoApi.genPaymentCredential -- only vk credentials
                    <*> CardanoApi.genStakeAddressReference
          where
            era = shelleyBasedEraFromRecentEra (recentEra @era)

        genByronVkAddr :: Gen (CardanoApi.AddressInEra (CardanoApiEra era))
        genByronVkAddr =
            CardanoApi.byronAddressInEra
                <$> CardanoApi.genAddressByron

        genWalletUTxO
            :: Gen (CardanoApi.AddressInEra (CardanoApiEra era))
            -> Gen (UTxO era)
        genWalletUTxO genAddr =
            scale (* 2)
                $ UTxO . Map.fromList <$> listOf genEntry
          where
            genEntry = (,) <$> genIn <*> genOut
              where
                genIn :: Gen TxIn
                genIn = fromWalletTxIn <$> W.genTxIn

                genOut :: Gen (TxOut era)
                genOut =
                    fmap (fromWalletTxOut . cardanoToWalletTxOut)
                        $ CardanoApi.TxOut
                            <$> genAddr
                            <*> scale (* 2) (CardanoApi.genTxOutValue era)
                            <*> pure CardanoApi.TxOutDatumNone
                            <*> pure CardanoApi.ReferenceScriptNone
                  where
                    era = cardanoEraFromRecentEra (recentEra @era)

    shrink (Wallet utxoAssumptions utxo changeAddressGen) =
        [ Wallet utxoAssumptions utxo' changeAddressGen
        | utxo' <- shrinkToUTxOSubset utxo
        ]
      where
        shrinkToUTxOSubset =
            take 16
                . fmap (UTxO . Map.fromList)
                . shrinkList shrinkEntry
                . Map.toList
                . utxoToMap
          where
            utxoToMap (UTxO m) = m

        shrinkEntry _ = []

genTxForBalancing :: forall era. IsRecentEra era => Gen (Tx era)
genTxForBalancing =
    fromCardanoApiTx <$> CardanoApi.genTxForBalancing cardanoEra
  where
    cardanoEra = cardanoEraFromRecentEra (recentEra :: RecentEra era)

genTxIn :: Gen TxIn
genTxIn = fromWalletTxIn <$> W.genTxIn

genTxOut :: forall era. IsRecentEra era => Gen (TxOut era)
genTxOut =
    -- NOTE: genTxOut does not generate quantities larger than
    -- `maxBound :: Word64`, however users could supply these. We
    -- should ideally test what happens, and make it clear what
    -- code, if any, should validate.
    CardanoApi.toShelleyTxOut shelleyBasedEra
        <$> CardanoApi.genTxOut cardanoEra
  where
    cardanoEra = cardanoEraFromRecentEra (recentEra :: RecentEra era)
    shelleyBasedEra = shelleyBasedEraFromRecentEra (recentEra :: RecentEra era)

-- | For writing shrinkers in the style of https://stackoverflow.com/a/14006575
prependOriginal :: (t -> [t]) -> t -> [t]
prependOriginal shrinker x = x : shrinker x

shrinkFee :: Ledger.Coin -> [Ledger.Coin]
shrinkFee (Ledger.Coin 0) = []
shrinkFee _ = [Ledger.Coin 0]

shrinkScriptData
    :: Era (CardanoApi.ShelleyLedgerEra era)
    => CardanoApi.TxBodyScriptData era
    -> [CardanoApi.TxBodyScriptData era]
shrinkScriptData CardanoApi.TxBodyNoScriptData = []
shrinkScriptData
    ( CardanoApi.TxBodyScriptData
            era
            (Alonzo.TxDats dats)
            (Alonzo.Redeemers redeemers)
        ) = case [ CardanoApi.TxBodyScriptData
                    era
                    (Alonzo.TxDats dats')
                    (Alonzo.Redeemers redeemers')
                 | dats' <-
                    dats
                        : (Map.fromList <$> shrinkList (const []) (Map.toList dats))
                 , redeemers' <-
                    redeemers
                        : (Map.fromList <$> shrinkList (const []) (Map.toList redeemers))
                 ] of
        (_ : rest) -> rest
        [] -> error "shrinkScriptData: unexpected empty shrink list"

shrinkSeq
    :: Foldable t => (a -> [a]) -> t a -> [StrictSeq.StrictSeq a]
shrinkSeq shrinkElem =
    map StrictSeq.fromList . shrinkList shrinkElem . F.toList

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet shrinkElem = map Set.fromList . shrinkList shrinkElem . F.toList

shrinkStrictMaybe :: StrictMaybe a -> [StrictMaybe a]
shrinkStrictMaybe = \case
    SNothing -> []
    SJust _ -> [SNothing]

shrinkTx :: forall era. IsRecentEra era => Tx era -> [Tx era]
shrinkTx =
    shrinkMapBy fromCardanoApiTx toCardanoApiTx shrinkCardanoApiTx
  where
    shrinkCardanoApiTx = case recentEra @era of
        RecentEraBabbage -> shrinkTxBabbage
        RecentEraConway -> \_ -> [] -- no shrinker implemented yet

shrinkTxBabbage
    :: CardanoApi.Tx CardanoApi.BabbageEra
    -> [CardanoApi.Tx CardanoApi.BabbageEra]
shrinkTxBabbage (CardanoApi.Tx bod wits) =
    [CardanoApi.Tx bod' wits | bod' <- shrinkTxBodyBabbage bod]

shrinkTxBodyBabbage
    :: CardanoApi.TxBody CardanoApi.BabbageEra
    -> [CardanoApi.TxBody CardanoApi.BabbageEra]
shrinkTxBodyBabbage
    (CardanoApi.ShelleyTxBody e bod scripts scriptData aux val) =
        case [ CardanoApi.ShelleyTxBody e bod' scripts' scriptData' aux' val'
             | bod' <- prependOriginal shrinkLedgerTxBody bod
             , aux' <- aux : filter (/= aux) [Nothing]
             , scriptData' <- prependOriginal shrinkScriptData scriptData
             , scripts' <- prependOriginal (shrinkList (const [])) scripts
             , val' <-
                val
                    : filter
                        (/= val)
                        [ CardanoApi.TxScriptValidity
                            CardanoApi.AlonzoEraOnwardsBabbage
                            CardanoApi.ScriptValid
                        ]
             ] of
            (_ : rest) -> rest
            [] -> error "shrinkTxBodyBabbage: unexpected empty shrink list"
      where
        shrinkLedgerTxBody
            :: Ledger.TxBody Babbage
            -> [Ledger.TxBody Babbage]
        shrinkLedgerTxBody body = case [ body
                                            & withdrawalsTxBodyL .~ wdrls'
                                            & outputsTxBodyL .~ outs'
                                            & inputsTxBodyL .~ ins'
                                            & certsTxBodyL .~ certs'
                                            & mintTxBodyL .~ mint'
                                            & reqSignerHashesTxBodyL .~ rsh'
                                            & updateTxBodyL .~ updates'
                                            & feeTxBodyL .~ txfee'
                                            & vldtTxBodyL .~ vldt'
                                            & scriptIntegrityHashTxBodyL .~ adHash'
                                       | wdrls' <-
                                            prependOriginal
                                                shrinkWdrl
                                                (body ^. withdrawalsTxBodyL)
                                       , outs' <-
                                            prependOriginal
                                                (shrinkSeq (const []))
                                                (body ^. outputsTxBodyL)
                                       , ins' <-
                                            prependOriginal
                                                (shrinkSet (const []))
                                                (body ^. inputsTxBodyL)
                                       , certs' <-
                                            prependOriginal
                                                (shrinkSeq (const []))
                                                (body ^. certsTxBodyL)
                                       , mint' <-
                                            prependOriginal
                                                shrinkValue
                                                (body ^. mintTxBodyL)
                                       , rsh' <-
                                            prependOriginal
                                                (shrinkSet (const []))
                                                (body ^. reqSignerHashesTxBodyL)
                                       , updates' <-
                                            prependOriginal
                                                shrinkStrictMaybe
                                                (body ^. updateTxBodyL)
                                       , txfee' <-
                                            prependOriginal
                                                shrinkFee
                                                (body ^. feeTxBodyL)
                                       , vldt' <-
                                            prependOriginal
                                                shrinkValidity
                                                (body ^. vldtTxBodyL)
                                       , adHash' <-
                                            prependOriginal
                                                shrinkStrictMaybe
                                                (body ^. scriptIntegrityHashTxBodyL)
                                       ] of
            (_ : rest) -> rest
            [] -> error "shrinkLedgerTxBody: unexpected empty shrink list"

        shrinkValidity (ValidityInterval a b) = case [ ValidityInterval a' b'
                                                     | a' <- prependOriginal shrinkStrictMaybe a
                                                     , b' <- prependOriginal shrinkStrictMaybe b
                                                     ] of
            (_ : rest) -> rest
            [] -> error "shrinkValidity: unexpected empty shrink list"

        shrinkValue :: (Eq a, Monoid a) => a -> [a]
        shrinkValue v = filter (/= v) [mempty]

shrinkWdrl :: Withdrawals -> [Withdrawals]
shrinkWdrl (Withdrawals m) =
    map (Withdrawals . Map.fromList)
        $ shrinkList shrinkWdrl' (Map.toList m)
  where
    shrinkWdrl' (acc, Ledger.Coin c) =
        [ (acc, Ledger.Coin c')
        | c' <- filter (>= 1) $ shrink c
        ]

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A convenient wrapper type that allows values of any type with a 'Buildable'
--   instance to be pretty-printed through the 'Show' interface.
newtype ShowBuildable a = ShowBuildable a
    deriving newtype (Arbitrary)

instance Buildable a => Show (ShowBuildable a) where
    show (ShowBuildable x) = pretty x

instance Buildable UTxOAssumptions where
    build = \case
        AllKeyPaymentCredentials -> "AllKeyPaymentCredentials"
        AllByronKeyPaymentCredentials -> "AllByronKeyPaymentCredentials"
        AllScriptPaymentCredentialsFrom scriptTemplate _scriptLookup ->
            nameF "AllScriptPaymentCredentialsFrom"
                $ blockListF [nameF "scriptTemplate" $ build scriptTemplate]

instance Buildable AnyChangeAddressGenWithState where
    build (AnyChangeAddressGenWithState (ChangeAddressGen g maxLengthAddr) s0) =
        blockListF
            [ nameF "changeAddr0"
                $ build
                $ show
                $ fst
                $ g s0
            , nameF "max address length"
                $ build
                $ BS.length
                $ serialiseAddr maxLengthAddr
            ]

-- CSV with the columns: wallet_balance,(fee,minfee | error)
instance Buildable BalanceTxGolden where
    build = \case
        BalanceTxGoldenFailure c err ->
            mconcat
                [ lovelaceF c
                , ","
                , build (T.pack err)
                ]
        BalanceTxGoldenSuccess c fee minfee ->
            mconcat
                [ lovelaceF c
                , ","
                , lovelaceF fee
                , ","
                , lovelaceF minfee
                ]
      where
        lovelaceF (Coin c) = fixedF 6 (fromIntegral c / 1e6 :: Double)

instance IsRecentEra era => Buildable (Wallet era) where
    build (Wallet assumptions utxo changeAddressGen) =
        nameF "Wallet"
            $ mconcat
                [ nameF "assumptions" $ build assumptions
                , nameF "changeAddressGen" $ build changeAddressGen
                , nameF "utxo" $ pretty $ toWalletUTxO utxo
                ]

--------------------------------------------------------------------------------
-- Miscellaneous orphan instances
--------------------------------------------------------------------------------

instance Show TimeTranslation where
    show = const "TimeTranslation"
