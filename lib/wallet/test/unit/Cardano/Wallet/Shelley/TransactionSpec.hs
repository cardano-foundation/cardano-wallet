{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use null" -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.TransactionSpec (spec) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, toXPub, xprvFromBytes, xprvToBytes, xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (Delegation, Payment, Policy)
    , Script
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
    , TxOutValue (TxOutAdaOnly, TxOutValue)
    )
import Cardano.Api.Extra
    ( asAnyShelleyBasedEra, withShelleyBasedTx )
import Cardano.Api.Gen
    ( genAddressInEra
    , genEncodingBoundaryLovelace
    , genSignedValue
    , genTx
    , genTxBodyContent
    , genTxForBalancing
    , genTxInEra
    , genTxOut
    , genTxOutDatum
    , genValueForTxOut
    , genWitnesses
    )
import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.Ledger.Alonzo.TxInfo
    ( TranslationError (..) )
import Cardano.Ledger.Era
    ( Era )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing), Wdrl (..) )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval (ValidityInterval) )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic), entropyToMnemonic, mkEntropy )
import Cardano.Numeric.Util
    ( power )
import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( SelectionBalanceError (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionOutputErrorInfo (..)
    , UnableToConstructChangeError (..)
    , WalletUTxO (..)
    , balanceMissing
    , emptySkeleton
    , selectionDelta
    )
import Cardano.Wallet
    ( ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrSelectAssets (..)
    , ErrUpdateSealedTx (..)
    , FeeEstimation (..)
    , PartialTx (..)
    , balanceTransaction
    , estimateFee
    , posAndNegFromCardanoValue
    , signTransaction
    )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.Gen
    ( genMnemonic, genScript )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (delegationAddress)
    , Depth (..)
    , DerivationIndex (..)
    , NetworkDiscriminant (..)
    , deriveRewardAccount
    , getRawKey
    , hex
    , liftRawKey
    , paymentAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState, defaultAddressPoolGap, mkSeqStateFromRootXPrv, purposeCIP1852 )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( estimateMaxWitnessRequiredPerInput )
import Cardano.Wallet.Primitive.Model
    ( Wallet (..), unsafeInitWallet )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    , hoistTimeInterpreter
    , mkSingleEraInterpreter
    , mkTimeInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (ActiveSlotCoefficient)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (EpochLength)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , LinearFunction (..)
    , ProtocolParameters (..)
    , SlotLength (SlotLength)
    , SlottingParameters (..)
    , StartTime (StartTime)
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , getGenesisBlockDate
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( minimumUTxOForShelleyBasedEra )
import Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( testParameter_coinsPerUTxOByte_Babbage
    , testParameter_coinsPerUTxOWord_Alonzo
    )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId, TokenBundle, tokenName )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName), TokenPolicyId, unTokenName )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenPolicyId, shrinkTokenPolicyId )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx
    , TxMetadata (..)
    , TxMetadataValue (..)
    , cardanoTxIdeallyNoLaterThan
    , getSealedTxWitnesses
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , serialisedTx
    , txMetadataIsNull
    , withinEra
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..), TxSize (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutTokenBundle )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Read.Primitive.Tx.Features.Integrity
    ( txIntegrity )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx )
import Cardano.Wallet.Shelley.Compatibility
    ( computeTokenBundleSerializedLengthBytes
    , fromCardanoLovelace
    , fromCardanoValue
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoValue
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toBabbageTxOut, toLedgerTokenBundle )
import Cardano.Wallet.Shelley.Transaction
    ( EraConstraints
    , TxSkeleton (..)
    , TxUpdate (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor
    , costOfIncreasingCoin
    , distributeSurplusDelta
    , estimateTxCost
    , estimateTxSize
    , maximumCostOfIncreasingCoin
    , mkByronWitness
    , mkDelegationCertificates
    , mkShelleyWitness
    , mkTxSkeleton
    , mkUnsignedTx
    , newTransactionLayer
    , noTxUpdate
    , sizeOfCoin
    , txConstraints
    , updateSealedTx
    , _decodeSealedTx
    , _distributeSurplus
    , _estimateMaxNumberOfInputs
    , _maxScriptExecutionCost
    )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrAssignRedeemers (..)
    , ErrMoreSurplusNeeded (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeAndChange (TxFeeAndChange)
    , TxFeeUpdate (..)
    , Withdrawal (..)
    , WitnessCountCtx (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_, replicateM )
import Control.Monad.Random
    ( MonadRandom (..)
    , Rand
    , Random (randomR, randomRs)
    , StdGen
    , evalRand
    , random
    , randoms
    )
import Control.Monad.Trans.Except
    ( except, runExceptT )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Default
    ( Default (..) )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( on, (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.List
    ( isSuffixOf, nub )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromJust, fromMaybe, isJust )
import Data.Ord
    ( comparing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Ratio
    ( (%) )
import Data.Semigroup
    ( Sum (Sum), getSum, mtimesDefault )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF', fmt, nameF, pretty, (+||), (||+) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), mkSlotLength )
import Ouroboros.Consensus.Config
    ( SecurityParam (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage )
import Ouroboros.Consensus.Util.Counting
    ( exactlyOne )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import System.Directory
    ( listDirectory )
import System.FilePath
    ( takeExtension, (</>) )
import System.Random.StdGenSeed
    ( StdGenSeed (..), stdGenFromSeed )
import Test.Hspec
    ( Spec
    , SpecWith
    , describe
    , expectationFailure
    , it
    , pendingWith
    , runIO
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Core.Spec
    ( SpecM )
import Test.Hspec.Golden
    ( Golden (..) )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , InfiniteList (..)
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , Testable
    , arbitraryPrintableChar
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllShow
    , frequency
    , label
    , liftShrink2
    , listOf
    , oneof
    , property
    , scale
    , shrinkList
    , shrinkMapBy
    , suchThat
    , vector
    , vectorOf
    , withMaxSuccess
    , within
    , (.||.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( chooseNatural, report )
import Test.QuickCheck.Gen
    ( Gen (..), listOf1 )
import Test.QuickCheck.Random
    ( QCGen, mkQCGen )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Pretty
    ( Pretty (..), (====) )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Gen as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Serialization as Ledger
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Val as Value
import qualified Cardano.Tx.Balance.Internal.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as TxOutGen
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import Cardano.Wallet.Write.Tx
    ( AnyRecentEra (..)
    , RecentEra (..)
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    )
import qualified Cardano.Wallet.Write.Tx as WriteTx
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Ouroboros.Consensus.HardFork.History.EraParams as HF
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.HardFork.History.Summary as HF
import qualified PlutusCore as Plutus
import qualified Test.Hspec.Extra as Hspec

spec :: Spec
spec = do
    decodeSealedTxSpec
    forAllEras estimateMaxInputsSpec
    forAllEras feeCalculationSpec
    feeEstimationRegressionSpec
    forAllRecentEras binaryCalculationsSpec
    transactionConstraintsSpec
    updateSealedTxSpec
    balanceTransactionSpec
    distributeSurplusSpec
    estimateSignedTxSizeSpec
    describe "Sign transaction" $ do
        spec_forAllEras
            "signTransaction adds reward account witness when necessary"
            prop_signTransaction_addsRewardAccountKey
        spec_forAllEras
            "signTransaction adds extra key witnesses when necessary"
            prop_signTransaction_addsExtraKeyWitnesses
        spec_forAllEras
            "signTransaction adds tx in witnesses when necessary"
            prop_signTransaction_addsTxInWitnesses
        spec_forAllEras
            "signTransaction adds collateral witnesses when necessary"
            prop_signTransaction_addsTxInCollateralWitnesses
        spec_forAllEras
            "signTransaction never removes witnesses"
            prop_signTransaction_neverRemovesWitnesses
        spec_forAllEras
            "signTransaction never changes tx body"
            prop_signTransaction_neverChangesTxBody
        spec_forAllEras
            "signTransaction preserves script integrity"
            prop_signTransaction_preservesScriptIntegrity

spec_forAllEras
    :: Testable prop => String -> (AnyCardanoEra -> prop) -> Spec
spec_forAllEras description p =
    describe description $
    forAllEras
        $ \(AnyCardanoEra era) -> it (show era)
        $ property
        $ p (AnyCardanoEra era)

instance Arbitrary SealedTx where
    arbitrary = sealedTxFromCardano <$> genTx

showTransactionBody
    :: forall era
     . IsCardanoEra era
    => Cardano.TxBodyContent Cardano.BuildTx era
    -> String
showTransactionBody =
    either Cardano.displayError show . Cardano.makeTransactionBody

unsafeMakeTransactionBody
    :: forall era
     . IsCardanoEra era
    => Cardano.TxBodyContent Cardano.BuildTx era
    -> Cardano.TxBody era
unsafeMakeTransactionBody =
    either (error . Cardano.displayError) id . Cardano.makeTransactionBody

stakeAddressForKey
    :: SL.Network
    -> XPub
    -> Cardano.StakeAddress
stakeAddressForKey net pubkey =
    Cardano.StakeAddress
        net (SL.KeyHashObj (SL.KeyHash $ hash pubkey)
            :: SL.Credential 'SL.Staking Crypto.StandardCrypto)
  where
    hash :: XPub -> Crypto.Hash Crypto.Blake2b_224 a
    hash = fromJust . Crypto.hashFromBytes . blake2b224 . xpubPublicKey

withdrawalForKey
    :: SL.Network
    -> XPub
    -> Cardano.Lovelace
    ->  ( Cardano.StakeAddress
        , Cardano.Lovelace
        , Cardano.BuildTxWith Cardano.BuildTx
            (Cardano.Witness Cardano.WitCtxStake era)
        )
withdrawalForKey net pubkey wdrlAmt =
    ( stakeAddressForKey net pubkey
    , wdrlAmt
    , Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
    )

whenSupportedInEra
    :: forall era a. (CardanoEra era -> Maybe a)
    -> CardanoEra era
    -> (a -> Property)
    -> Property
whenSupportedInEra test era f =
    case test era of
        Nothing ->
            True
            & label ("feature not supported in " <> show era)
        Just supported ->
            f supported

prop_signTransaction_addsRewardAccountKey
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> Coin
    -- ^ Amount to withdraw
    -> Property
prop_signTransaction_addsRewardAccountKey
    (AnyCardanoEra era) rootXPrv utxo wdrlAmt =
    withMaxSuccess 10 $
    whenSupportedInEra Cardano.withdrawalsSupportedInEra era $
    \(supported :: Cardano.WithdrawalsSupportedInEra era) -> do
        let
            rootK :: (ShelleyKey 'RootK XPrv, Passphrase "encryption")
            rootK = first liftRawKey rootXPrv

            rawRewardK :: (XPrv, Passphrase "encryption")
            rawRewardK =
                ( getRawKey $ deriveRewardAccount (snd rootK) (fst rootK)
                , snd rootK
                )

            rewardAcctPubKey :: XPub
            rewardAcctPubKey = toXPub $ fst rawRewardK

            extraWdrls =
                [ withdrawalForKey SL.Mainnet rewardAcctPubKey
                    (toCardanoLovelace wdrlAmt)
                ]

            addWithdrawals
                :: Cardano.TxBodyContent Cardano.BuildTx era
                -> Cardano.TxBodyContent Cardano.BuildTx era
            addWithdrawals txBodyContent = txBodyContent
                { Cardano.txWithdrawals =
                    case Cardano.txWithdrawals txBodyContent of
                        Cardano.TxWithdrawalsNone ->
                            Cardano.TxWithdrawals supported extraWdrls
                        Cardano.TxWithdrawals _ wdrls ->
                            Cardano.TxWithdrawals supported $
                            wdrls <> extraWdrls
                }

        withBodyContent era addWithdrawals $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction tl (AnyCardanoEra era)
                    (const Nothing) rootK utxo sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits = InAnyCardanoEra era <$>
                    case Cardano.cardanoEraStyle era of
                        LegacyByronEra -> error
                            "Withdrawal witnesses are not supported in the \
                            \Byron era."
                        ShelleyBasedEra _ ->
                            [mkShelleyWitness txBody rawRewardK]

            expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeysSeqWithPass =<< genPassphrase (0, 16)

genRootKeysSeqWithPass
    :: Passphrase "encryption"
    -> Gen (ShelleyKey depth XPrv)
genRootKeysSeqWithPass encryptionPass = do
    s <- SomeMnemonic <$> genMnemonic @15
    g <- Just . SomeMnemonic <$> genMnemonic @12
    return $ Shelley.unsafeGenerateKeyFromSeed (s, g) encryptionPass

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

prop_signTransaction_addsExtraKeyWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Keys
    -> Property
prop_signTransaction_addsExtraKeyWitnesses
    (AnyCardanoEra era) rootK utxo extraKeys =
    withMaxSuccess 10 $
    whenSupportedInEra Cardano.extraKeyWitnessesSupportedInEra era $
    \(supported :: Cardano.TxExtraKeyWitnessesSupportedInEra era) -> do
    let
        keys
            :: (XPrv, Passphrase "encryption")
            -> Cardano.SigningKey Cardano.PaymentExtendedKey
        keys = Cardano.PaymentExtendedSigningKey . fst

        hashes :: [Cardano.Hash Cardano.PaymentKey]
        hashes =
            ( Cardano.verificationKeyHash
            . Cardano.castVerificationKey
            . Cardano.getVerificationKey
            . keys
            ) <$> extraKeys

        addExtraWits
            :: Cardano.TxBodyContent Cardano.BuildTx era
            -> Cardano.TxBodyContent Cardano.BuildTx era
        addExtraWits txBodyContent = txBodyContent
            { Cardano.txExtraKeyWits =
                Cardano.TxExtraKeyWitnesses supported hashes
            }

    withBodyContent era addExtraWits $ \(txBody, wits) -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
            sealedTx' = signTransaction tl
                (AnyCardanoEra era)
                (lookupFnFromKeys extraKeys)
                (first liftRawKey rootK)
                utxo
                sealedTx

            expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
            expectedWits = InAnyCardanoEra era <$>
                case Cardano.cardanoEraStyle era of
                    LegacyByronEra ->
                        -- signTransaction does nothing in Byron era
                        []
                    ShelleyBasedEra _ ->
                        mkShelleyWitness txBody <$> extraKeys

        expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

keyToAddress :: (XPrv, Passphrase "encryption") -> Address
keyToAddress (xprv, _pwd) =
    -- TODO, decrypt?
    paymentAddress @'Mainnet @ShelleyKey @'CredFromKeyK .
    publicKey .
    liftRawKey @ShelleyKey $ xprv

utxoFromKeys
    :: [(XPrv, Passphrase "encryption")]
    -> (UTxO -> Property)
    -> Property
utxoFromKeys keys utxoProp =
    let
        addresses :: [Address]
        addresses = keyToAddress <$> keys

        txOuts :: [TxOut]
        txOuts = (flip foldMap) addresses $ \addr ->
            [TxOut addr mempty]

        isUnique :: [TxIn] -> Bool
        isUnique txIns = nub txIns == txIns
    in
        forAll (vectorOf (length txOuts) genTxIn `suchThat` isUnique) $
            \txIns -> do
                let utxo = UTxO $ Map.fromList $ zip txIns txOuts
                utxoProp utxo

lookupFnFromKeys
    :: [(XPrv, Passphrase "encryption")]
    -> (Address -> Maybe (ShelleyKey 'CredFromKeyK XPrv, Passphrase "encryption"))
lookupFnFromKeys keys addr =
    let
        addrMap
            :: Map Address (ShelleyKey 'CredFromKeyK XPrv, Passphrase "encryption")
        addrMap = Map.fromList
            $ zip (keyToAddress <$> keys) (first liftRawKey <$> keys)
    in
        Map.lookup addr addrMap

withBodyContent
    :: IsCardanoEra era
    => CardanoEra era
    ->  ( Cardano.TxBodyContent Cardano.BuildTx era ->
          Cardano.TxBodyContent Cardano.BuildTx era
        )
    -> ((Cardano.TxBody era, [Cardano.KeyWitness era]) -> Property)
    -> Property
withBodyContent era modTxBody cont =
    forAllShow (genTxBodyContent era) showTransactionBody $
        \txBodyContent -> do
            let
                txBodyContent' = modTxBody txBodyContent
                txBody = unsafeMakeTransactionBody txBodyContent'

            forAll (genWitnesses era txBody) $ \wits -> cont (txBody, wits)

checkSubsetOf :: (Eq a, Show a) => [a] -> [a] -> Property
checkSubsetOf as bs = property
    $ counterexample counterexampleText
    $ all (`Set.member` ys) (ShowOrd <$> as)
  where
    xs = Set.fromList (ShowOrd <$> as)
    ys = Set.fromList (ShowOrd <$> bs)

    counterexampleText = unlines
        [ "the following set:"
        , showSet xs
        , "is not a subset of:"
        , showSet ys
        , "rogue elements:"
        , showSet (xs `Set.difference` ys)
        ]
      where
        showSet = pretty . fmap (show . unShowOrd) . F.toList

prop_signTransaction_addsTxInWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInWitnesses
    (AnyCardanoEra era) rootK extraKeysNE =
    withMaxSuccess 10 $ do

    let extraKeys = NE.toList extraKeysNE

    utxoFromKeys extraKeys $ \utxo -> do
        let
            txIns :: [TxIn]
            txIns = Map.keys $ unUTxO utxo

            addTxIns
                :: Cardano.TxBodyContent Cardano.BuildTx era
                -> Cardano.TxBodyContent Cardano.BuildTx era
            addTxIns txBodyContent = txBodyContent
                { Cardano.txIns =
                    (
                    , Cardano.BuildTxWith
                        (Cardano.KeyWitness Cardano.KeyWitnessForSpending)
                    )
                    . toCardanoTxIn <$> txIns
                }

        withBodyContent era addTxIns $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction tl
                    (AnyCardanoEra era)
                    (lookupFnFromKeys extraKeys)
                    (first liftRawKey rootK)
                    utxo
                    sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits = InAnyCardanoEra era <$>
                    case Cardano.cardanoEraStyle era of
                        LegacyByronEra ->
                            -- signTransaction does nothing in Byron era
                            []
                        ShelleyBasedEra _ ->
                            mkShelleyWitness txBody <$> extraKeys

            expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

prop_signTransaction_addsTxInCollateralWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInCollateralWitnesses
    (AnyCardanoEra era) rootK extraKeysNE =
    withMaxSuccess 10 $
    whenSupportedInEra Cardano.collateralSupportedInEra era $
    \(supported :: Cardano.CollateralSupportedInEra era) -> do

        let extraKeys = NE.toList extraKeysNE

        utxoFromKeys extraKeys $ \utxo -> do
            let
                txIns :: [TxIn]
                txIns = Map.keys $ unUTxO utxo

                addTxCollateralIns
                    :: Cardano.TxBodyContent Cardano.BuildTx era
                    -> Cardano.TxBodyContent Cardano.BuildTx era
                addTxCollateralIns txBodyContent = txBodyContent
                    { Cardano.txInsCollateral =
                        Cardano.TxInsCollateral supported
                            (toCardanoTxIn <$> txIns)
                    }

            withBodyContent era addTxCollateralIns $ \(txBody, wits) -> do
                let
                    tl = testTxLayer

                    sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                    sealedTx' = signTransaction tl
                        (AnyCardanoEra era)
                        (lookupFnFromKeys extraKeys)
                        (first liftRawKey rootK)
                        utxo
                        sealedTx

                    expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                    expectedWits = InAnyCardanoEra era <$>
                        case Cardano.cardanoEraStyle era of
                            LegacyByronEra ->
                                -- signTransaction does nothing in Byron era
                                []
                            ShelleyBasedEra _ ->
                                mkShelleyWitness txBody <$> extraKeys

                expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

prop_signTransaction_neverRemovesWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Extra keys to form basis of address -> key lookup function
    -> Property
prop_signTransaction_neverRemovesWitnesses
    (AnyCardanoEra era) rootK utxo extraKeys =
    withMaxSuccess 10 $
    forAll (genTxInEra era) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction tl
                (AnyCardanoEra era)
                (lookupFnFromKeys extraKeys)
                (first liftRawKey rootK)
                utxo
                sealedTx

            witnessesBefore = getSealedTxWitnesses sealedTx
            witnessesAfter = getSealedTxWitnesses sealedTx'

        checkCoverage
            $ cover 10 (not $ null witnessesBefore) "witnesses non-empty before"
            $ witnessesBefore `checkSubsetOf` witnessesAfter

prop_signTransaction_neverChangesTxBody
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Extra keys to form basis of address -> key lookup function
    -> Property
prop_signTransaction_neverChangesTxBody
    (AnyCardanoEra era) rootK utxo extraKeys =
    withMaxSuccess 10 $
    forAll (genTxInEra era) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction tl
                (AnyCardanoEra era)
                (lookupFnFromKeys extraKeys)
                (first liftRawKey rootK)
                utxo
                sealedTx

            txBodyContent
                :: InAnyCardanoEra Cardano.Tx
                -> InAnyCardanoEra (Cardano.TxBodyContent Cardano.ViewTx)
            txBodyContent
                (InAnyCardanoEra e
                    (Cardano.Tx (Cardano.TxBody bodyContent) _wits)
                ) =
                InAnyCardanoEra e bodyContent

            bodyContentBefore = txBodyContent $ cardanoTx sealedTx
            bodyContentAfter = txBodyContent $ cardanoTx sealedTx'

        bodyContentBefore == bodyContentAfter

prop_signTransaction_preservesScriptIntegrity
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> Property
prop_signTransaction_preservesScriptIntegrity (AnyCardanoEra era) rootK utxo =
    withMaxSuccess 10 $
    whenSupportedInEra Cardano.scriptDataSupportedInEra era $ \_supported ->
    forAll (genTxInEra era) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction tl
                (AnyCardanoEra era)
                (const Nothing)
                (first liftRawKey rootK)
                utxo
                sealedTx

            txIntegrityCardanoApi = txIntegrity . fromCardanoApiTx

            getScriptIntegrityHashInAnyCardanoEra
                :: InAnyCardanoEra Cardano.Tx
                -> Maybe ByteString
            getScriptIntegrityHashInAnyCardanoEra (InAnyCardanoEra _ tx') =
                getHash <$> txIntegrityCardanoApi tx'

            scriptIntegrityHashBefore =
                getScriptIntegrityHashInAnyCardanoEra $ cardanoTx sealedTx
            scriptIntegrityHashAfter =
                getScriptIntegrityHashInAnyCardanoEra $ cardanoTx sealedTx'

        checkCoverage
            $ cover 30 (isJust scriptIntegrityHashBefore)
                "script integrity hash exists"
            $ conjoin
                [ scriptIntegrityHashBefore == scriptIntegrityHashAfter
                    & counterexample
                        ("script integrity hash before: "
                            <> show scriptIntegrityHashBefore
                        )
                    & counterexample
                        ("script integrity hash after: "
                            <> show scriptIntegrityHashAfter
                        )
                ]

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
    eraSpec (AnyCardanoEra BabbageEra)

forAllRecentEras :: (AnyRecentEra -> Spec) -> Spec
forAllRecentEras eraSpec = do
    eraSpec (AnyRecentEra RecentEraAlonzo)
    eraSpec (AnyRecentEra RecentEraBabbage)

allEras :: [(Int, AnyCardanoEra)]
allEras =
    [ (1, AnyCardanoEra ByronEra)
    , (2, AnyCardanoEra ShelleyEra)
    , (3, AnyCardanoEra AllegraEra)
    , (4, AnyCardanoEra MaryEra)
    , (5, AnyCardanoEra AlonzoEra)
    , (6, AnyCardanoEra BabbageEra)
    ]

eraNum :: AnyCardanoEra -> Int
eraNum e = fst $ head $ filter ((== e) . snd) allEras

shelleyEraNum :: AnyRecentEra -> Int
shelleyEraNum (AnyRecentEra era) =
    eraNum . AnyCardanoEra $ cardanoEraFromRecentEra era

instance Arbitrary AnyCardanoEra where
    arbitrary = frequency $ zip [1..] $ map (pure . snd) allEras
    -- Shrink by choosing a *later* era
    shrink e = map snd $ filter ((> eraNum e) . fst) allEras

instance Arbitrary AnyRecentEra where
    arbitrary = elements
        [ AnyRecentEra RecentEraAlonzo
        , AnyRecentEra RecentEraBabbage
        ]

decodeSealedTxSpec :: Spec
decodeSealedTxSpec = describe "SealedTx serialisation/deserialisation" $ do
    it "tx with withdrawal" $ do
        let bytes = unsafeFromHex byteString
        let sealedTx = sealedTxFromBytes bytes
        sealedTx `shouldSatisfy` isRight

    prop "roundtrip for Shelley witnesses" prop_sealedTxRecentEraRoundtrip
  where
    byteString =
        "84a70081825820410a9cd4af08b3abe25c2d3b87af4c23d0bb2fb7577b639d5cfbdf\
        \e13a4a696c0c0d80018182583901059f0c7b9899793d2c9afaeff4fd09bedd9df3b8\
        \cb1b9c301ab8e0f7fb3c13a29d3798f1b77b47f2ddb31c19326b87ed6f71fb9a2713\
        \3ad51b000001001d19d714021a000220ec03198d0f05a1581de1fb3c13a29d3798f1\
        \b77b47f2ddb31c19326b87ed6f71fb9a27133ad51b000000e8d4a510000e80a0f5f6"

-- Note:
--
-- In the tests below, the expected numbers of inputs are highly sensitive
-- to the size distribution of token bundles within generated transaction
-- outputs.
--
-- If these tests fail unexpectedly, it's a good idea to check whether or
-- not the distribution of generated token bundles has changed.
--
estimateMaxInputsSpec :: AnyCardanoEra -> Spec
estimateMaxInputsSpec era@(AnyCardanoEra cEra) =
    if withinEra (AnyCardanoEra AlonzoEra) era
    then do
        describe ("Alonzo and earlier - " <> show cEra) $ do
            estimateMaxInputsTests @ShelleyKey era
                [(1,115),(5,109),(10,104),(20,93),(50,54)]
            estimateMaxInputsTests @ByronKey era
                [(1,73),(5,69),(10,65),(20,57),(50,29)]
            estimateMaxInputsTests @IcarusKey era
                [(1,73),(5,69),(10,65),(20,57),(50,29)]
    -- Babbage era and later has slightly larger transactions, meaning slightly
    -- smaller expected outputs.
    else do
        describe ("Babbage and later - " <> show cEra) $ do
            estimateMaxInputsTests @ShelleyKey era
                [(1,115),(5,109),(10,104),(20,92),(50,53)]
            estimateMaxInputsTests @ByronKey era
                [(1,73),(5,69),(10,65),(20,56),(50,28)]
            estimateMaxInputsTests @IcarusKey era
                [(1,73),(5,69),(10,65),(20,56),(50,28)]

feeCalculationSpec :: AnyCardanoEra -> Spec
feeCalculationSpec era = describe "fee calculations" $ do
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
                minFeeSkeleton $ emptyTxSkeleton
                    { txAssetsToMintOrBurn = Set.fromList assets }
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
                minFeeSkeleton $ emptyTxSkeleton { txMintOrBurnScripts = scripts }
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
                estimateTxSize' $ emptyTxSkeleton
                    { txAssetsToMintOrBurn = Set.fromList mints }
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
            numWitnesses = fromIntegral $ sum $
                estimateMaxWitnessRequiredPerInput <$> scripts
            sizeWitness  =    1 -- small array
                           + 34 -- vkey
                           + 66 -- signature

            -- Total size (in bytes) of the scripts when serialized
            scriptLengths = fromIntegral . getSum $
                F.foldMap (Sum . BS.length . serializeScript ) scripts

            sizeWith =
                estimateTxSize' $ emptyTxSkeleton { txMintOrBurnScripts = scripts }
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
                , txParameters = dummyTxParameters
                    { getMaxExecutionUnits = ExecutionUnits 10_000_000 10_000_000_000
                    }
                }
        txs <- readTestTransactions
        forM_ txs $ \(filepath, tx) -> do
            let rdmrs = replicate (sealedNumberOfRedeemers tx) (error "Redeemer")
            if (null rdmrs) then do
                it ("without redeemers: " <> filepath) $
                    _maxScriptExecutionCost ppWithPrices rdmrs
                        `shouldBe` (Coin 0)
            else do
                it ("with redeemers: " <> filepath) $
                    _maxScriptExecutionCost ppWithPrices rdmrs
                        `shouldSatisfy` (> (Coin 0))

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
                    minFeeSkeleton $ emptyTxSkeleton
                        { txAssetsToMintOrBurn = Set.fromList assets }
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
                    minFeeSkeleton $ emptyTxSkeleton { txMintOrBurnScripts = scripts }
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
                    estimateTxSize' $ emptyTxSkeleton
                        { txAssetsToMintOrBurn = Set.fromList mints }
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
                numWitnesses = fromIntegral $ sum $
                    estimateMaxWitnessRequiredPerInput <$> scripts
                sizeWitness  =    1 -- small array
                               + 34 -- vkey
                               + 66 -- signature

                -- Total size (in bytes) of the scripts when serialized
                scriptLengths = fromIntegral . getSum $
                    F.foldMap (Sum . BS.length . serializeScript ) scripts

                sizeWith =
                    estimateTxSize' $ emptyTxSkeleton { txMintOrBurnScripts = scripts }
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
                { getFeePolicy =
                    LinearFee LinearFunction {intercept = 100_000, slope = 100}
                }
            }

    minFee :: TransactionCtx -> Integer
    minFee ctx = Coin.toInteger $ calcMinimumCost testTxLayer era pp ctx sel
      where sel = emptySkeleton

    minFeeSkeleton :: TxSkeleton -> Integer
    minFeeSkeleton = Coin.toInteger . estimateTxCost era pp

    estimateTxSize' :: TxSkeleton -> Integer
    estimateTxSize' = fromIntegral . unTxSize . estimateTxSize era

    (dummyAcct, dummyPath) =
        (RewardAccount mempty, DerivationIndex 0 :| [])

feeEstimationRegressionSpec :: Spec
feeEstimationRegressionSpec = describe "Regression tests" $ do
    it "#1740 Fee estimation at the boundaries" $ do
        let requiredCost = 166_029
        let runSelection = except $ Left
                $ ErrSelectAssetsSelectionError
                $ SelectionBalanceErrorOf
                $ UnableToConstructChange
                $ UnableToConstructChangeError
                    { requiredCost = Coin.fromWord64 requiredCost
                    , shortfall = Coin 100_000
                    }
        result <- runExceptT (estimateFee runSelection)
        result `shouldBe` Right (FeeEstimation requiredCost requiredCost)

binaryCalculationsSpec :: AnyRecentEra -> Spec
binaryCalculationsSpec (AnyRecentEra era) =
    case era of
        RecentEraAlonzo -> binaryCalculationsSpec' @Cardano.AlonzoEra era
        RecentEraBabbage -> binaryCalculationsSpec' @Cardano.BabbageEra era

-- Up till Mary era we have the following structure of transaction
--   transaction =
-- [ transaction_body
-- , transaction_witness_set
-- , auxiliary_data / null
-- ]
-- So we begin with 3-element array binary prefix, that is encoded as '83'
-- From Alonzo on tx was enriched for isValid field making it
-- 4-element array that is encoded as '84'.
--   transaction =
-- [ transaction_body
-- , transaction_witness_set
-- , bool
-- , auxiliary_data / null
-- ]
-- Alonzo transaction stil has the same binary representation (array)
-- for transaction outputs like in the previous eras.
-- Babbage era changes this representation, and introduces map binary
-- representation for transaction outputs as the concept of them is
-- extended in this era.
binaryCalculationsSpec'
    :: forall era. EraConstraints era
    => RecentEra era -> Spec
binaryCalculationsSpec' era = describe ("calculateBinary - "+||era||+"") $ do
    describe "Byron witnesses - mainnet" $ do
        let net = Cardano.Mainnet
        it "1 input, 2 outputs" $ do
            let pairs = [dummyWit 0]
            let amtInp = 10_000_000
            let amtFee = 129_700
            let amtOut = 2_000_000
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
            let binary = case era of
                    RecentEraBabbage ->
                        "84a400818258200000000000000000000000000000000000000000\
                        \000000000000000000000000000182a20058390101010101010101\
                        \010101010101010101010101010101010101010101010101010101\
                        \01010101010101010101010101010101010101010101011a001e84\
                        \80a200583901020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \0202020202020202011a0078175c021a0001faa403191e46a10281\
                        \845820010000000000000000000000000000000000000000000000\
                        \000000000000000058407154db81463825f150bb3b9b0824caf151\
                        \3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d\
                        \483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000\
                        \000000000000000000000000000000000000000000000000000000\
                        \41a0f5f6"
                    RecentEraAlonzo ->
                        "84a400818258200000000000000000000000000000000000000000\
                        \000000000000000000000000000182825839010101010101010101\
                        \010101010101010101010101010101010101010101010101010101\
                        \0101010101010101010101010101010101010101011a001e848082\
                        \583901020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \02020202021a0078175c021a0001faa403191e46a1028184582001\
                        \000000000000000000000000000000000000000000000000000000\
                        \000000005840d7af60ae33d2af351411c1445c79590526990bfa73\
                        \cbb3732b54ef322daa142e6884023410f8be3c16e9bd52076f2bb3\
                        \6bf38dfe034a9f04658e9f56197ab80f5820000000000000000000\
                        \000000000000000000000000000000000000000000000041a0f5f6"

            calculateBinary net utxo outs chgs pairs `shouldBe` binary

        it "2 inputs, 3 outputs" $ do
            let pairs = [dummyWit 0, dummyWit 1]
            let amtInp = 10_000_000
            let amtFee = 135_200
            let amtOut = 6_000_000
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
            let binary = case era of
                    RecentEraBabbage ->
                        "84a400828258200000000000000000000000000000000000000000\
                        \000000000000000000000000008258200000000000000000000000\
                        \000000000000000000000000000000000000000000010183a20058\
                        \390102020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \02020202011a005b8d80a200583901030303030303030303030303\
                        \030303030303030303030303030303030303030303030303030303\
                        \0303030303030303030303030303030303011a005b8d80a2005839\
                        \010404040404040404040404040404040404040404040404040404\
                        \040404040404040404040404040404040404040404040404040404\
                        \040404011a007801e0021a0002102003191e46a102828458200100\
                        \000000000000000000000000000000000000000000000000000000\
                        \00000058401a8667d2d0af4e24d4d385443002f1e9036063bdb7c6\
                        \2d45447a2e176ded81a11683bd944c6d7db6e5fd886840025f6319\
                        \2a382e526f4150e2b336ee9ed80808582000000000000000000000\
                        \0000000000000000000000000000000000000000000041a0845820\
                        \130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7c\
                        \e38d477f515840320ed7d1513b0f1b61381f7942a07b627b246c85\
                        \a13b2623e4868ea82488c778a7760124f3a17f924c08d425c0717d\
                        \f6cd898eb4ab8439a16e08befdc415120e58200101010101010101\
                        \01010101010101010101010101010101010101010101010141a0f5\
                        \f6"
                    RecentEraAlonzo ->
                        "84a400828258200000000000000000000000000000000000000000\
                        \000000000000000000000000008258200000000000000000000000\
                        \000000000000000000000000000000000000000000010183825839\
                        \010202020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \0202021a005b8d8082583901030303030303030303030303030303\
                        \030303030303030303030303030303030303030303030303030303\
                        \03030303030303030303030303031a005b8d808258390104040404\
                        \040404040404040404040404040404040404040404040404040404\
                        \040404040404040404040404040404040404040404040404041a00\
                        \7801e0021a0002102003191e46a102828458200100000000000000\
                        \0000000000000000000000000000000000000000000000005840e8\
                        \e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b7895\
                        \5728bffa7efa54297c6a5d73337bd6280205b1759c13f79d4c93f2\
                        \9871fc51b78aeba80e582000000000000000000000000000000000\
                        \0000000000000000000000000000000041a0845820130ae82201d7\
                        \072e6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158\
                        \405835ff78c6fc5e4466a179ca659fa85c99b8a3fba083f3f3f42b\
                        \a360d479c64ef169914b52ade49b19a7208fd63a6e67a19c406b48\
                        \26608fdc5307025506c30758200101010101010101010101010101\
                        \01010101010101010101010101010101010141a0f5f6"

            calculateBinary net utxo outs chgs pairs `shouldBe` binary

    describe "Byron witnesses - testnet" $ do
        let net = Cardano.Testnet (Cardano.NetworkMagic 0)
        it "1 input, 2 outputs" $ do
            let pairs = [dummyWit 0]
            let amtInp = 10_000_000
            let amtFee = 129_700
            let amtOut = 2_000_000
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
            let binary = case era of
                    RecentEraBabbage ->
                        "84a400818258200000000000000000000000000000000000000000\
                        \000000000000000000000000000182a20058390101010101010101\
                        \010101010101010101010101010101010101010101010101010101\
                        \01010101010101010101010101010101010101010101011a001e84\
                        \80a200583901020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \0202020202020202011a0078175c021a0001faa403191e46a10281\
                        \845820010000000000000000000000000000000000000000000000\
                        \000000000000000058407154db81463825f150bb3b9b0824caf151\
                        \3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d\
                        \483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000\
                        \000000000000000000000000000000000000000000000000000000\
                        \44a1024100f5f6"
                    RecentEraAlonzo ->
                        "84a400818258200000000000000000000000000000000000000000\
                        \000000000000000000000000000182825839010101010101010101\
                        \010101010101010101010101010101010101010101010101010101\
                        \0101010101010101010101010101010101010101011a001e848082\
                        \583901020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \02020202021a0078175c021a0001faa403191e46a1028184582001\
                        \000000000000000000000000000000000000000000000000000000\
                        \000000005840d7af60ae33d2af351411c1445c79590526990bfa73\
                        \cbb3732b54ef322daa142e6884023410f8be3c16e9bd52076f2bb3\
                        \6bf38dfe034a9f04658e9f56197ab80f5820000000000000000000\
                        \000000000000000000000000000000000000000000000044a10241\
                        \00f5f6"

            calculateBinary net utxo outs chgs pairs `shouldBe` binary

        it "2 inputs, 3 outputs" $ do
            let pairs = [dummyWit 0, dummyWit 1]
            let amtInp = 10_000_000
            let amtFee = 135_200
            let amtOut = 6_000_000
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
            let binary = case era of
                    RecentEraBabbage ->
                        "84a400828258200000000000000000000000000000000000000000\
                        \000000000000000000000000008258200000000000000000000000\
                        \000000000000000000000000000000000000000000010183a20058\
                        \390102020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \02020202011a005b8d80a200583901030303030303030303030303\
                        \030303030303030303030303030303030303030303030303030303\
                        \0303030303030303030303030303030303011a005b8d80a2005839\
                        \010404040404040404040404040404040404040404040404040404\
                        \040404040404040404040404040404040404040404040404040404\
                        \040404011a007801e0021a0002102003191e46a10282845820130a\
                        \e82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d\
                        \477f515840320ed7d1513b0f1b61381f7942a07b627b246c85a13b\
                        \2623e4868ea82488c778a7760124f3a17f924c08d425c0717df6cd\
                        \898eb4ab8439a16e08befdc415120e582001010101010101010101\
                        \0101010101010101010101010101010101010101010144a1024100\
                        \845820010000000000000000000000000000000000000000000000\
                        \000000000000000058401a8667d2d0af4e24d4d385443002f1e903\
                        \6063bdb7c62d45447a2e176ded81a11683bd944c6d7db6e5fd8868\
                        \40025f63192a382e526f4150e2b336ee9ed8080858200000000000\
                        \000000000000000000000000000000000000000000000000000000\
                        \44a1024100f5f6"
                    RecentEraAlonzo ->
                        "84a400828258200000000000000000000000000000000000000000\
                        \000000000000000000000000008258200000000000000000000000\
                        \000000000000000000000000000000000000000000010183825839\
                        \010202020202020202020202020202020202020202020202020202\
                        \020202020202020202020202020202020202020202020202020202\
                        \0202021a005b8d8082583901030303030303030303030303030303\
                        \030303030303030303030303030303030303030303030303030303\
                        \03030303030303030303030303031a005b8d808258390104040404\
                        \040404040404040404040404040404040404040404040404040404\
                        \040404040404040404040404040404040404040404040404041a00\
                        \7801e0021a0002102003191e46a10282845820130ae82201d7072e\
                        \6fbfc0a1884fb54636554d14945b799125cf7ce38d477f51584058\
                        \35ff78c6fc5e4466a179ca659fa85c99b8a3fba083f3f3f42ba360\
                        \d479c64ef169914b52ade49b19a7208fd63a6e67a19c406b482660\
                        \8fdc5307025506c307582001010101010101010101010101010101\
                        \0101010101010101010101010101010144a1024100845820010000\
                        \000000000000000000000000000000000000000000000000000000\
                        \00005840e8e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b\
                        \81be9b78955728bffa7efa54297c6a5d73337bd6280205b1759c13\
                        \f79d4c93f29871fc51b78aeba80e58200000000000000000000000\
                        \00000000000000000000000000000000000000000044a1024100f5\
                        \f6"

            calculateBinary net utxo outs chgs pairs `shouldBe` binary

  where
    slotNo = SlotNo 7_750
    md = Nothing
    calculateBinary net utxo outs chgs pairs =
        hex (Cardano.serialiseToCBOR ledgerTx)
      where
          ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
          mkByronWitness' unsignedTx (_, (TxOut addr _)) =
              mkByronWitness @era unsignedTx net addr
          addrWits = zipWith (mkByronWitness' unsigned) inps pairs
          fee = toCardanoLovelace $ selectionDelta TxOut.coin cs
          Right unsigned =
              mkUnsignedTx (shelleyBasedEraFromRecentEra era)
                (Nothing, slotNo) (Right cs) md mempty [] fee
              TokenMap.empty TokenMap.empty Map.empty Map.empty
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
    spec_forAllEras "cost of empty transaction" prop_txConstraints_txBaseCost
    spec_forAllEras "size of empty transaction" prop_txConstraints_txBaseSize
    spec_forAllEras "cost of non-empty transaction" prop_txConstraints_txCost
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
    => AnyCardanoEra
    -> [(GivenNumOutputs, ExpectedNumInputs)]
    -> SpecWith ()
estimateMaxInputsTests era cases = do
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
                _estimateMaxNumberOfInputs @k era (Quantity 16_384) defaultTransactionCtx outs
                    `shouldBe` nInps

        prop "more outputs ==> less inputs"
            (prop_moreOutputsMeansLessInputs @k era)
        prop "bigger size  ==> more inputs"
            (prop_biggerMaxSizeMeansMoreInputs @k)

--------------------------------------------------------------------------------
-- Roundtrip tests for SealedTx

prop_sealedTxRecentEraRoundtrip
    :: AnyRecentEra
    -> AnyCardanoEra
    -> Pretty DecodeSetup
    -> Property
prop_sealedTxRecentEraRoundtrip
    txEra@(AnyRecentEra era) currentEra (Pretty tc) =
    conjoin
        [ txBytes ==== serialisedTx sealedTxC
        , either
            (\e -> counterexample (show e) False)
            (compareOnCBOR tx)
            sealedTxB
        ]
        .||. encodingFromTheFuture (txEra) currentEra
  where
    tx = makeShelleyTx (shelleyBasedEraFromRecentEra era) tc
    txBytes = Cardano.serialiseToCBOR tx
    sealedTxC = sealedTxFromCardano' tx
    sealedTxB = sealedTxFromBytes' currentEra txBytes

makeShelleyTx
    :: IsShelleyBasedEra era
    => ShelleyBasedEra era
    -> DecodeSetup
    -> Cardano.Tx era
makeShelleyTx era testCase = Cardano.makeSignedTransaction addrWits unsigned
  where
    DecodeSetup utxo outs md slotNo pairs _netwk = testCase
    inps = Map.toList $ unUTxO utxo
    fee = toCardanoLovelace $ selectionDelta TxOut.coin cs
    Right unsigned =
        mkUnsignedTx era (Nothing, slotNo) (Right cs) md mempty [] fee
        TokenMap.empty TokenMap.empty Map.empty Map.empty
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

encodingFromTheFuture :: AnyRecentEra -> AnyCardanoEra -> Bool
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
    => AnyCardanoEra
    -> Quantity "byte" Word16
    -> NonEmptyList TxOut
    -> Property
prop_moreOutputsMeansLessInputs era size (NonEmpty xs)
    = withMaxSuccess 1_000
    $ within 300_000
    $ _estimateMaxNumberOfInputs @k era size defaultTransactionCtx (tail xs)
      >=
      _estimateMaxNumberOfInputs @k era size defaultTransactionCtx xs

-- | Increasing the max size automatically increased the number of inputs
prop_biggerMaxSizeMeansMoreInputs
    :: forall k. TxWitnessTagFor k
    => AnyCardanoEra
    -> Quantity "byte" Word16
    -> [TxOut]
    -> Property
prop_biggerMaxSizeMeansMoreInputs era size outs
    = withMaxSuccess 1_000
    $ within 300_000
    $ getQuantity size < maxBound `div` 2 ==>
        _estimateMaxNumberOfInputs @k era size defaultTransactionCtx outs
        <=
        _estimateMaxNumberOfInputs @k era ((*2) <$> size ) defaultTransactionCtx outs

testTxLayer :: TransactionLayer ShelleyKey 'CredFromKeyK SealedTx
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
    arbitrary = SlotNo <$> choose (1, 1_000)

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
    arbitrary =
        TxOut addr <$> scale (`mod` 4) genTokenBundleSmallRange
      where
        addr = Address $ BS.pack (1:replicate 56 0)
    shrink (TxOut addr bundle) =
        [ TxOut addr bundle'
        | bundle' <- shrinkTokenBundleSmallRange bundle
        ]

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
        let addr = Address $ BS.pack (1:replicate 56 0)
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

instance Arbitrary (Passphrase "user") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "user"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "user"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "user")

instance Arbitrary (Quantity "byte" Word16) where
    arbitrary = Quantity <$> choose (128, 2_048)
    shrink (Quantity size)
        | size <= 1 = []
        | otherwise = Quantity <$> shrink size

dummyAddress :: Word8 -> Address
dummyAddress b =
    Address $ BS.pack $ 1 : replicate 56 b

coinToBundle :: Word64 -> TokenBundle
coinToBundle = TokenBundle.fromCoin . Coin.fromWord64

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
    , getMaxExecutionUnits =
        error "dummyTxParameters: getMaxExecutionUnits"
    }

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters = ProtocolParameters
    { decentralizationLevel =
        error "dummyProtocolParameters: decentralizationLevel"
    , txParameters =
        error "dummyProtocolParameters: txParameters"
    , desiredNumberOfStakePools =
        error "dummyProtocolParameters: desiredNumberOfStakePools"
    , minimumUTxO =
        error "dummyProtocolParameters: minimumUTxO"
    , stakeKeyDeposit =
        error "dummyProtocolParameters: stakeKeyDeposit"
    , eras =
        error "dummyProtocolParameters: eras"
    , maximumCollateralInputCount =
        error "dummyProtocolParameters: maximumCollateralInputCount"
    , minimumCollateralPercentage =
        error "dummyProtocolParameters: minimumCollateralPercentage"
    , executionUnitPrices =
        error "dummyProtocolParameters: executionUnitPrices"
    , currentNodeProtocolParameters =
        error "dummyProtocolParameters: currentNodeProtocolParameters"
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
mockFeePolicy = LinearFee $ LinearFunction
    { intercept = 155_381
    , slope = 44
    }

mockProtocolParameters :: ProtocolParameters
mockProtocolParameters = dummyProtocolParameters
    { executionUnitPrices = Just $ ExecutionUnitPrices
        (721 % 10_000_000)
        (577 %     10_000)
    , txParameters = TxParameters
        { getFeePolicy = mockFeePolicy
        , getTxMaxSize = Quantity 16_384
        , getTokenBundleMaxSize = TokenBundleMaxSize $ TxSize 4_000
        , getMaxExecutionUnits = ExecutionUnits 10_000_000_000 14_000_000
        }
    , minimumUTxO = minimumUTxOForShelleyBasedEra Cardano.ShelleyBasedEraAlonzo
        def
            { Alonzo._coinsPerUTxOWord = testParameter_coinsPerUTxOWord_Alonzo
            }
    , maximumCollateralInputCount = 3
    , minimumCollateralPercentage = 150
    }

mockTxConstraints :: AnyCardanoEra -> TxConstraints
mockTxConstraints era =
    txConstraints era mockProtocolParameters TxWitnessShelleyUTxO

data MockSelection = MockSelection
    { txInputCount :: Int
    , txOutputs :: [TxOut]
    , txRewardWithdrawal :: Coin
    }
    deriving (Eq, Show)

genMockSelection :: Gen MockSelection
genMockSelection = do
    txInputCount <-
        oneof [ pure 0, choose (1, 1_000) ]
    txOutputCount <-
        oneof [ pure 0, choose (1, 1_000) ]
    txOutputs <- replicateM txOutputCount genOut
    txRewardWithdrawal <-
        Coin <$> oneof [ pure 0, chooseNatural (1, 1_000_000) ]
    pure MockSelection
        { txInputCount
        , txOutputs
        , txRewardWithdrawal
        }
  where
    genOut = TxOut (dummyAddress dummyByte) <$> genTokenBundleSmallRange
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
prop_txConstraints_txBaseCost :: AnyCardanoEra -> Property
prop_txConstraints_txBaseCost era =
    txBaseCost (mockTxConstraints era)
        === estimateTxCost era mockProtocolParameters emptyTxSkeleton

-- Tests that using 'txBaseSize' to estimate the size of an empty selection
-- produces a result that is consistent with the result of using
-- 'estimateTxSize'.
--
prop_txConstraints_txBaseSize :: AnyCardanoEra -> Property
prop_txConstraints_txBaseSize era =
    txBaseSize (mockTxConstraints era)
        === estimateTxSize era emptyTxSkeleton

-- Tests that using 'txConstraints' to estimate the cost of a non-empty
-- selection produces a result that is consistent with the result of using
-- 'estimateTxCost'.
--
prop_txConstraints_txCost :: AnyCardanoEra -> MockSelection -> Property
prop_txConstraints_txCost era mock =
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
        [ txBaseCost (mockTxConstraints era)
        , txInputCount `mtimesDefault` txInputCost (mockTxConstraints era)
        , F.foldMap (txOutputCost (mockTxConstraints era) . tokens) txOutputs
        , txRewardWithdrawalCost (mockTxConstraints era) txRewardWithdrawal
        ]
    lowerBound = estimateTxCost era mockProtocolParameters emptyTxSkeleton
        {txInputCount, txOutputs, txRewardWithdrawal}
    -- We allow a small amount of overestimation due to the slight variation in
    -- the marginal cost of an input:
    upperBound = lowerBound <> txInputCount `mtimesDefault` Coin (4*44)

-- Tests that using 'txConstraints' to estimate the size of a non-empty
-- selection produces a result that is consistent with the result of using
-- 'estimateTxSize'.
--
prop_txConstraints_txSize :: AnyCardanoEra -> MockSelection -> Property
prop_txConstraints_txSize era mock =
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
        [ txBaseSize (mockTxConstraints era)
        , txInputCount `mtimesDefault` txInputSize (mockTxConstraints era)
        , F.foldMap (txOutputSize (mockTxConstraints era) . tokens) txOutputs
        , txRewardWithdrawalSize (mockTxConstraints era) txRewardWithdrawal
        ]
    lowerBound = estimateTxSize era emptyTxSkeleton
        {txInputCount, txOutputs, txRewardWithdrawal}
    -- We allow a small amount of overestimation due to the slight variation in
    -- the marginal size of an input:
    upperBound = lowerBound <> txInputCount `mtimesDefault` TxSize 4

newtype Large a = Large { unLarge :: a }
    deriving (Eq, Show)

instance Arbitrary (Large TokenBundle) where
    arbitrary = fmap Large . genTxOutTokenBundle =<< choose (1, 128)

-- Tests that if a bundle is oversized (when serialized), then a comparison
-- between 'txOutputSize' and 'txOutputMaximumSize' should also indicate that
-- the bundle is oversized.
--
prop_txConstraints_txOutputMaximumSize
    :: AnyCardanoEra
    -> Blind (Large TokenBundle)
    -> Property
prop_txConstraints_txOutputMaximumSize era (Blind (Large bundle)) =
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
    simulatedSize = txOutputSize (mockTxConstraints era) bundle
    simulatedSizeMax :: TxSize
    simulatedSizeMax = txOutputMaximumSize (mockTxConstraints era)

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

instance Arbitrary StdGenSeed  where
  arbitrary = StdGenSeed . fromIntegral @Int <$> arbitrary

balanceTransactionSpec :: Spec
balanceTransactionSpec = describe "balanceTransaction" $ do
    -- TODO: Create a test to show that datums are passed through...

    it "doesn't balance transactions with existing 'totalCollateral'"
        $ property prop_balanceTransactionExistingTotalCollateral

    it "doesn't balance transactions with existing 'returnCollateral'"
        $ property prop_balanceTransactionExistingReturnCollateral

    it "produces valid transactions or fails"
        $ property prop_balanceTransactionValid

    balanceTransactionGoldenSpec

    describe "posAndNegFromCardanoValue" $
        it "roundtrips with toCardanoValue" $
            property prop_posAndNegFromCardanoValueRoundtrip

    it "increases zero-ada outputs to minimum" $ do
        pendingWith "Needs correct mock PParams for Babbage"
        let era = WriteTx.RecentEraBabbage
        let out = TxOut dummyAddr (TokenBundle.fromCoin (Coin 0))
        let out' = TxOut dummyAddr (TokenBundle.fromCoin (Coin 874_930))
        let Cardano.ShelleyTx _ tx = either (error . show) id
                $ balanceTx
                $ paymentPartialTx [ out ]
        let outs = WriteTx.outputs era $ WriteTx.txBody era tx

        let pp = def
                { Babbage._coinsPerUTxOByte =
                    testParameter_coinsPerUTxOByte_Babbage
                }
        WriteTx.isBelowMinimumCoinForTxOut era pp (head outs)
            `shouldBe` False

        head outs `shouldBe` (toBabbageTxOut out' Nothing)
    describe "effect of txMaxSize on coin selection" $ do
        -- Wallet with only small utxos, and enough of them to fill a tx in the
        -- tests below.
        let wallet = mkTestWallet dummyRootK $ UTxO $ Map.fromList $
                [ ( TxIn (Hash $ B8.replicate 32 '1') ix
                  , TxOut dummyAddr (TokenBundle.fromCoin $ Coin 1_000_000)
                  )
                | ix <- [0 .. 500]
                ]

        let balance = balanceTransaction' wallet testStdGenSeed
        let totalOutput tx =
                let (wtx, _, _, _, _, _) =
                        decodeTx testTxLayer maxBound
                        (ShelleyWalletCtx dummyPolicyK)
                        (sealedTxFromCardano' tx)
                in
                    F.foldMap (view (#tokens . #coin)) (view #outputs wtx)
                    <> fromMaybe (Coin 0) (view #fee wtx)

        it "tries to select 2x the payment amount" $ do
            let tx = balance $ paymentPartialTx
                    [ TxOut dummyAddr
                        (TokenBundle.fromCoin (Coin 50_000_000))
                    ]
            totalOutput <$> tx `shouldBe` Right (Coin 100_000_000)

        it "falls back to 1x if out of space" $ do
            let tx = balance $ paymentPartialTx
                    [ TxOut dummyAddr
                        (TokenBundle.fromCoin (Coin 100_000_000))
                    ]
            totalOutput <$> tx `shouldBe` Right (Coin 102_000_000)

        it "otherwise fails with ErrBalanceTxMaxSizeLimitExceeded" $ do
            let tx = balance $ paymentPartialTx
                    [ TxOut dummyAddr
                        (TokenBundle.fromCoin (Coin 200_000_000))
                    ]
            tx `shouldBe` Left ErrBalanceTxMaxSizeLimitExceeded

    describe "when passed unresolved inputs" $ do
        it "fails with ErrBalanceTxUnresolvedTxIn" $ do
            let txin = TxIn (Hash $ B8.replicate 32 '3') 10

            -- 1 output, 1 input without utxo entry
            let partialTx :: PartialTx Cardano.BabbageEra
                partialTx = over #tx (addExtraTxIns [txin]) $
                    paymentPartialTx
                        [ TxOut dummyAddr
                            (TokenBundle.fromCoin (Coin 1_000_000))
                        ]
            balanceTx partialTx
                `shouldBe`
                Left (ErrBalanceTxUnresolvedInputs (txin :| []))

        describe "with redeemers" $
            it "fails with ErrBalanceTxUnresolvedInputs" $ do
                let withNoUTxO :: PartialTx era -> PartialTx era
                    withNoUTxO ptx = ptx { inputs = mempty }

                balanceTx (withNoUTxO pingPong_2)
                    `shouldBe` Left
                        (ErrBalanceTxUnresolvedInputs $ NE.fromList
                            [ TxIn (Hash "11111111111111111111111111111111") 0
                            ]
                        )

    describe "when validity interval is too far in the future" $ do
        let withValidityBeyondHorizon = withValidityInterval
                $ ValidityInterval SNothing (SJust beyondHorizon)
        describe "with some Plutus redeemers" $ do
            it "fails with TimeTranslationPastHorizon" $ do
                case balanceTx (withValidityBeyondHorizon pingPong_2) of
                    Left
                        (ErrBalanceTxAssignRedeemers
                            (ErrAssignRedeemersTranslationError
                                (TimeTranslationPastHorizon
                                    _pastHoriozon))) -> return ()
                    other -> expectationFailure $
                        "Expected pastHorizon failure; got " <> show other

        describe "with no redeemers" $ do
            it "succeeds at balancing" $ do
                case balanceTx (withValidityBeyondHorizon pingPong_1) of
                    Right _tx -> return ()
                    other -> expectationFailure $
                        "Expected (Right tx); got " <> show other

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
            case balanceTx (withNoRedeemers pingPong_2) of
                Right _tx -> pure ()
                other -> expectationFailure $
                    "Expected (Right tx); got " <> show other

    describe "when a redeemer points to an input that doesn't exist" $ do
        it "fails with ErrAssignRedeemersTargetNotFound" $ do

            let tid = Hash $ B8.replicate 32 '1'

            -- With ix 1 instead of 0, making it point to an input which
            -- doesn't exist in the tx.
            let faultyRedeemer =
                    RedeemerSpending (unsafeFromHex "D87A80") (TxIn tid 1)

            let withFaultyRedeemer =
                    over #redeemers $ mapFirst $ const faultyRedeemer

            balanceTx (withFaultyRedeemer pingPong_2)
                `shouldBe`
                Left (ErrBalanceTxAssignRedeemers
                        (ErrAssignRedeemersTargetNotFound faultyRedeemer))
  where
    mapFirst f (x:xs) = f x : xs
    mapFirst _ [] = error "mapFirst: empty list"

    horizon = SlotNo 20
    beyondHorizon = SlotNo 21

    balanceTx
        :: WriteTx.IsRecentEra era
        => PartialTx era
        -> Either ErrBalanceTx (Cardano.Tx era)
    balanceTx tx = flip evalRand (stdGenFromSeed testStdGenSeed) $ runExceptT $
        balanceTransaction @_ @(Rand StdGen)
            (nullTracer @(Rand StdGen))
            testTxLayer
            (delegationAddress @'Mainnet)
            mockProtocolParametersForBalancing
            (dummyTimeInterpreterWithHorizon horizon)
            (u, wal, pending)
            tx
      where
        Wallet' u wal pending
            = mkTestWallet dummyRootK (utxo [Coin 5_000_000])

        utxo coins = UTxO $ Map.fromList $ zip ins outs
          where
            ins = map (TxIn dummyHash) [0..]
            outs = map (TxOut dummyAddr . TokenBundle.fromCoin) coins
            dummyHash = Hash $ B8.replicate 32 '0'

    dummyRootK = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
      where
        mw = SomeMnemonic $ either (error . show) id
            (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")

    dummyAddr = Address $ unsafeFromHex
        "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

distributeSurplusSpec :: Spec
distributeSurplusSpec = do
    describe "sizeOfCoin" $ do
        let coinToWord64Clamped = fromMaybe maxBound . Coin.toWord64Maybe
        let cborSizeOfCoin =
                TxSize
                . fromIntegral
                . BS.length
                . CBOR.toStrictByteString
                . CBOR.encodeWord64 . coinToWord64Clamped

        let isBoundary c =
                sizeOfCoin c /= sizeOfCoin (c `Coin.difference` Coin 1)
                || sizeOfCoin c /= sizeOfCoin (c `Coin.add` Coin 1)

        it "matches the size of the Word64 CBOR encoding" $
            property $ checkCoverage $
                forAll genEncodingBoundaryLovelace $ \l -> do
                    let c = fromCardanoLovelace l
                    let expected = cborSizeOfCoin c

                    -- Use a low coverage requirement of 0.01% just to
                    -- ensure we see /some/ amount of every size.
                    let coverSize s = cover 0.01 (s == expected) (show s)
                    sizeOfCoin c === expected
                        & coverSize (TxSize 1)
                        & coverSize (TxSize 2)
                        & coverSize (TxSize 3)
                        & coverSize (TxSize 5)
                        & coverSize (TxSize 9)
                        & cover 0.5 (isBoundary c) "boundary case"

        describe "boundary case goldens" $ do
            it "1 byte to 2 byte boundary" $ do
                sizeOfCoin (Coin 23) `shouldBe` TxSize 1
                sizeOfCoin (Coin 24) `shouldBe` TxSize 2
            it "2 byte to 3 byte boundary" $ do
                sizeOfCoin (Coin $ 2 `power` 8 - 1) `shouldBe` TxSize 2
                sizeOfCoin (Coin $ 2 `power` 8    ) `shouldBe` TxSize 3
            it "3 byte to 5 byte boundary" $ do
                sizeOfCoin (Coin $ 2 `power` 16 - 1) `shouldBe` TxSize 3
                sizeOfCoin (Coin $ 2 `power` 16    ) `shouldBe` TxSize 5
            it "5 byte to 9 byte boundary" $ do
                sizeOfCoin (Coin $ 2 `power` 32 - 1) `shouldBe` TxSize 5
                sizeOfCoin (Coin $ 2 `power` 32    ) `shouldBe` TxSize 9

    describe "costOfIncreasingCoin" $ do
        it "costs 176 lovelace to increase 4294.967295 ada (2^32 - 1 lovelace) \
           \by 1 lovelace on mainnet" $ do
            let feePolicy = LinearFee $ LinearFunction
                    { intercept = 150_000, slope = 44 }
            let expectedCostIncrease = Coin 176
            costOfIncreasingCoin feePolicy (Coin $ 2 `power` 32 - 1) (Coin 1)
                `shouldBe` expectedCostIncrease

        it "produces results in the range [0, 8 * feePerByte]" $
            property $ \c increase -> do
                let feePolicy = LinearFee $ LinearFunction 1 0
                let res = costOfIncreasingCoin feePolicy c increase
                counterexample (show res <> "out of bounds") $
                    res >= Coin 0 && res <= Coin 8

    describe "distributeSurplus" $ do

      it "prop_distributeSurplus_onSuccess_conservesSurplus" $
          prop_distributeSurplus_onSuccess_conservesSurplus
              & property
      it "prop_distributeSurplus_onSuccess_coversCostIncrease" $
          prop_distributeSurplus_onSuccess_coversCostIncrease
              & property
      it "prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues" $
          prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
              & property
      it "prop_distributeSurplus_onSuccess_doesNotReduceFeeValue" $
          prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
              & property
      it "prop_distributeSurplus_onSuccess_preservesChangeLength" $
          prop_distributeSurplus_onSuccess_preservesChangeLength
              & property
      it "prop_distributeSurplus_onSuccess_preservesChangeAddresses" $
          prop_distributeSurplus_onSuccess_preservesChangeAddresses
              & property
      it "prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets" $
          prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
              & property
      it "prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue" $
          prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
              & property
      it "prop_distributeSurplus_onSuccess_increasesValuesByDelta" $
          prop_distributeSurplus_onSuccess_increasesValuesByDelta
              & property

    describe "distributeSurplusDelta" $ do

        let simplestFeePolicy = LinearFee $ LinearFunction
                { intercept = 0, slope = 1 }
        let mainnetFeePolicy = LinearFee $ LinearFunction
                { intercept = 150_000, slope = 44 }

        -- NOTE: The test values below make use of 255 being encoded as 2 bytes,
        -- and 256 as 3 bytes.

        describe "when increasing change increases fee" $
            it "will increase fee (99 lovelace for change, 1 for fee)" $
                distributeSurplusDelta
                    simplestFeePolicy
                    (Coin 100)
                    (TxFeeAndChange (Coin 200) [Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 1) [Coin 99])

        describe "when increasing fee increases fee" $
            it "will increase fee (98 lovelace for change, 2 for fee)" $ do
                distributeSurplusDelta
                    simplestFeePolicy
                    (Coin 100)
                    (TxFeeAndChange (Coin 255) [Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 2) [Coin 98])

        describe "when increasing the change costs more in fees than the \
                 \increase itself" $ do
            it "will try burning the surplus as fees" $ do
                distributeSurplusDelta
                    mainnetFeePolicy
                    (Coin 10)
                    (TxFeeAndChange (Coin 200) [Coin 255])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 10) [Coin 0])

            it "will fail if neither the fee can be increased" $ do
                distributeSurplusDelta
                    mainnetFeePolicy
                    (Coin 10)
                    (TxFeeAndChange (Coin 255) [Coin 255])
                    `shouldBe`
                    Left (ErrMoreSurplusNeeded $ Coin 34)

        describe "when no change output is present" $ do
            it "will burn surplus as excess fees" $
                property $ \surplus fee0 -> do
                    distributeSurplusDelta
                        simplestFeePolicy
                        surplus
                        (TxFeeAndChange fee0 [])
                        `shouldBe`
                        Right (TxFeeAndChange surplus [])

        it "prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus" $
            prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
                & withMaxSuccess 10_000
                & property

-- Verify that 'distributeSurplusDelta':
--
--    - covers the increase to the fee requirement incurred as a result of
--      increasing the fee value and change values.
--
--    - conserves the surplus:
--        - feeDelta + sum changeDeltas == surplus
--
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    :: FeePolicy -> Coin -> Coin -> [Coin] -> Property
prop_distributeSurplusDelta_coversCostIncreaseAndConservesSurplus
    feePolicy surplus fee0 change0 =
    checkCoverage $
    cover 2  (isLeft  mres) "Failure" $
    cover 50 (isRight mres) "Success" $
    report mres "Result" $
    counterexample (show mres) $ case mres of
        Left (ErrMoreSurplusNeeded shortfall) ->
            conjoin
                [ property $ surplus < maxCoinCostIncrease
                , property $ shortfall > Coin 0
                , costOfIncreasingCoin feePolicy fee0 surplus
                    === surplus <> shortfall
                ]
        Right (TxFeeAndChange feeDelta changeDeltas) -> do
            let feeRequirementIncrease = mconcat
                    [ costOfIncreasingCoin feePolicy fee0 feeDelta
                    , F.fold $ zipWith (costOfIncreasingCoin feePolicy)
                        change0
                        changeDeltas
                    ]
            conjoin
                [ property $ feeDelta >= feeRequirementIncrease
                    & counterexample ("fee requirement increased by "
                        <> show feeRequirementIncrease
                        <> " but the fee delta was just "
                        <> show feeDelta
                        )
                , F.fold changeDeltas <> feeDelta
                    === surplus
                ]
  where
    mres = distributeSurplusDelta
        feePolicy surplus (TxFeeAndChange fee0 change0)
    maxCoinCostIncrease = maximumCostOfIncreasingCoin feePolicy

--------------------------------------------------------------------------------
-- Properties for 'distributeSurplus'
--------------------------------------------------------------------------------

instance Arbitrary FeePolicy where
    arbitrary = frequency
        [ (1, feePolicyMainnet)
        , (7, feePolicyGeneral)
        ]
      where
        feePolicyMainnet :: Gen FeePolicy
        feePolicyMainnet = pure $ LinearFee $ LinearFunction
            {intercept = 150_000, slope = 44}

        feePolicyGeneral :: Gen FeePolicy
        feePolicyGeneral = do
            intercept <- frequency
                [ (1, pure 0.0)
                , (3, getPositive <$> arbitrary)
                ]
            slope <- frequency
                [ (1, pure 0.0)
                , (3, getPositive <$> arbitrary)
                ]
            pure $ LinearFee LinearFunction {intercept, slope}

    shrink (LinearFee LinearFunction {intercept, slope}) =
        LinearFee . uncurry LinearFunction
            <$> shrink (intercept, slope)

newtype TxBalanceSurplus a = TxBalanceSurplus {unTxBalanceSurplus :: a}
    deriving (Eq, Show)

instance Arbitrary (TxBalanceSurplus Coin) where
    -- We want to test cases where the surplus is zero. So it's important that
    -- we do not restrict ourselves to positive coins here.
    arbitrary = TxBalanceSurplus <$> frequency
        [ (8, genCoin)
        , (4, genCoin & scale (* (2 `power`  4)))
        , (2, genCoin & scale (* (2 `power`  8)))
        , (1, genCoin & scale (* (2 `power` 16)))
        ]
    shrink = shrinkMapBy TxBalanceSurplus unTxBalanceSurplus shrinkCoin

instance Arbitrary (TxFeeAndChange [TxOut]) where
    arbitrary = do
        fee <- genCoin
        change <- frequency
            [ (1, pure [])
            , (1, (: []) <$> TxOutGen.genTxOut)
            , (6, listOf TxOutGen.genTxOut)
            ]
        pure $ TxFeeAndChange fee change
    shrink (TxFeeAndChange fee change) =
        uncurry TxFeeAndChange <$> liftShrink2
            (shrinkCoin)
            (shrinkList TxOutGen.shrinkTxOut)
            (fee, change)

-- A helper function to generate properties for 'distributeSurplus' on
-- success.
--
prop_distributeSurplus_onSuccess
    :: Testable prop
    => (FeePolicy
        -> Coin
        -> TxFeeAndChange [TxOut]
        -> TxFeeAndChange [TxOut]
        -> prop)
    -> FeePolicy
    -> TxBalanceSurplus Coin
    -> TxFeeAndChange [TxOut]
    -> Property
prop_distributeSurplus_onSuccess propertyToTest policy txSurplus fc =
    checkCoverage $
    cover 50
        (isRight mResult)
        "isRight mResult" $
    cover 10
        (length changeOriginal == 0)
        "length changeOriginal == 0" $
    cover 10
        (length changeOriginal == 1)
        "length changeOriginal == 1" $
    cover 50
        (length changeOriginal >= 2)
        "length changeOriginal >= 2" $
    cover 2
        (feeOriginal == Coin 0)
        "feeOriginal == Coin 0" $
    cover 2
        (feeOriginal == Coin 1)
        "feeOriginal == Coin 1" $
    cover 50
        (feeOriginal >= Coin 2)
        "feeOriginal >= Coin 2" $
    cover 1
        (surplus == Coin 0)
        "surplus == Coin 0" $
    cover 1
        (surplus == Coin 1)
        "surplus == Coin 1" $
    cover 50
        (surplus >= Coin 2)
        "surplus >= Coin 2" $
    either
        (const $ property True)
        (property . propertyToTest policy surplus fc)
        mResult
  where
    TxBalanceSurplus surplus = txSurplus
    TxFeeAndChange feeOriginal changeOriginal = fc

    mResult :: Either ErrMoreSurplusNeeded (TxFeeAndChange [TxOut])
    mResult = _distributeSurplus policy surplus fc

-- Verifies that the 'distributeSurplus' function conserves the surplus: the
-- total increase in the fee and change ada quantities should be exactly equal
-- to the given surplus.
--
prop_distributeSurplus_onSuccess_conservesSurplus
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_conservesSurplus =
    prop_distributeSurplus_onSuccess $ \_policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
        surplus === Coin.difference
            (feeModified <> F.foldMap TxOut.coin changeModified)
            (feeOriginal <> F.foldMap TxOut.coin changeOriginal)

-- The 'distributeSurplus' function should cover the cost of any increases in
-- 'Coin' values.
--
-- If the total cost of encoding ada quantities has increased by 𝛿c, then the
-- fee value should have increased by at least 𝛿c.
--
prop_distributeSurplus_onSuccess_coversCostIncrease
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_coversCostIncrease =
    prop_distributeSurplus_onSuccess $ \policy _surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) -> do
        let coinsOriginal = feeOriginal : (TxOut.coin <$> changeOriginal)
        let coinsModified = feeModified : (TxOut.coin <$> changeModified)
        let coinDeltas = zipWith Coin.difference coinsModified coinsOriginal
        let costIncrease = F.foldMap
                (uncurry $ costOfIncreasingCoin policy)
                (coinsOriginal `zip` coinDeltas)
        Coin.difference feeModified feeOriginal >= costIncrease
            & report feeModified "feeModified"
            & report feeOriginal "feeOriginal"
            & report costIncrease "costIncrease"

-- Since the 'distributeSurplus' function is not aware of the minimum ada
-- quantity or how to calculate it, it should never allow change ada values to
-- decrease.
--
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            all (uncurry (<=)) $ zip
                (TxOut.coin <$> changeOriginal)
                (TxOut.coin <$> changeModified)

-- The 'distributeSurplus' function should never return a 'fee' value that is
-- less than the original value.
--
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_doesNotReduceFeeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange feeOriginal _changeOriginal)
        (TxFeeAndChange feeModified _changeModified) ->
            feeOriginal <= feeModified

-- The 'distributeSurplus' function should always return exactly the same
-- number of change outputs that it was given. It should never create or
-- destroy change outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeLength
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_preservesChangeLength =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            length changeOriginal === length changeModified

-- The 'distributeSurplus' function should never adjust addresses of change
-- outputs.
--
prop_distributeSurplus_onSuccess_preservesChangeAddresses
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_preservesChangeAddresses =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view #address <$> changeOriginal) ===
            (view #address <$> changeModified)

-- The 'distributeSurplus' function should never adjust the values of non-ada
-- assets.
--
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (view (#tokens . #tokens) <$> changeOriginal) ===
            (view (#tokens . #tokens) <$> changeModified)

-- The 'distributeSurplus' function should only adjust the very first change
-- value.  All other change values should be left untouched.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- In principle, 'distributeSurplus' could allow itself to adjust any of the
-- change values in order to find a (marginally) more optimal solution.
-- However, for reasons of simplicity, we only adjust the first change value.
--
-- Here we verify that the implementation indeed only adjusts the first change
-- value, as expected.
--
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_onlyAdjustsFirstChangeValue =
    prop_distributeSurplus_onSuccess $ \_policy _surplus
        (TxFeeAndChange _feeOriginal changeOriginal)
        (TxFeeAndChange _feeModified changeModified) ->
            (drop 1 changeOriginal) ===
            (drop 1 changeModified)

-- The 'distributeSurplus' function should increase values by the exact amounts
-- indicated in 'distributeSurplusDelta'.
--
-- This is actually an implementation detail of 'distributeSurplus'.
--
-- However, it's useful to verify that this is true by subtracting the delta
-- values from the result of 'distributeSurplus', which should produce the
-- original fee and change values.
--
prop_distributeSurplus_onSuccess_increasesValuesByDelta
    :: FeePolicy -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_increasesValuesByDelta =
    prop_distributeSurplus_onSuccess $ \policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
            let Right (TxFeeAndChange feeDelta changeDeltas) =
                    distributeSurplusDelta policy surplus $ TxFeeAndChange
                        (feeOriginal)
                        (TxOut.coin <$> changeOriginal)
            in
            (TxFeeAndChange
                (feeModified `Coin.difference` feeDelta)
                (zipWith TxOut.subtractCoin changeDeltas changeModified)
            )
            ===
            TxFeeAndChange feeOriginal changeOriginal

--------------------------------------------------------------------------------

-- https://mail.haskell.org/pipermail/haskell-cafe/2016-August/124742.html
mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)

data Wallet' = Wallet'
    (UTxOIndex WalletUTxO)
    (Wallet (SeqState 'Mainnet ShelleyKey))
    (Set Tx)

instance Show Wallet' where
    show (Wallet' u w pending) = fmt $ mconcat
        [ nameF "Wallet" (pretty w)
        , nameF "UTxOIndex" (""+||u||+"")
        , nameF "pending" (""+||pending||+"")
        ]


mkTestWallet :: ShelleyKey 'RootK XPrv -> UTxO -> Wallet'
mkTestWallet rootK utxo = Wallet'
    (UTxOIndex.fromMap $ CS.toInternalUTxOMap utxo)
    (unsafeInitWallet utxo (header block0) s)
    mempty
  where
    s = mkSeqStateFromRootXPrv (rootK, mempty) purposeCIP1852 defaultAddressPoolGap

instance Arbitrary Wallet' where
    arbitrary = do
        utxo <- scale (* 3) genUTxO
        mw <- SomeMnemonic <$> genMnemonic @12
        pure $ mkTestWallet (rootK mw) utxo
      where
        genUTxO =
            UTxO . Map.fromList <$> listOf genEntry
          where
            genEntry = (,) <$> genIn <*> genOut
              where
                genIn = Compatibility.fromCardanoTxIn <$> Cardano.genTxIn
                genOut = Compatibility.fromCardanoTxOut <$> genTxOut AlonzoEra

        rootK :: SomeMnemonic -> ShelleyKey 'RootK XPrv
        rootK mw = generateKeyFromSeed (mw, Nothing) mempty
    shrink w = [setUTxO u' w
               | u' <- shrinkUTxO' (getUTxO w)
               ]

      where
        setUTxO :: UTxO -> Wallet' -> Wallet'
        setUTxO u (Wallet' _ wal pending) =
            Wallet'
                (UTxOIndex.fromMap $ CS.toInternalUTxOMap u)
                (wal {utxo = u})
                pending

        getUTxO (Wallet' u _ _) =
            CS.toExternalUTxOMap $ UTxOIndex.toMap u

        shrinkUTxO' u
            | UTxO.size u > 1 && simplifyUTxO u /= u
                = [simplifyUTxO u]
            | otherwise
                = []
          where
            -- NOTE: We could merge all TokenBundles into one, or use
            -- 'shrinkUTxO', however this primitive shrinking to a 10 ada UTxO
            -- seems better.
            simplifyUTxO :: UTxO -> UTxO
            simplifyUTxO = UTxO . uncurry Map.singleton
                . foldl1 (\(i, TxOut addr _val1) (_, TxOut _ _val2)
                    -> (i, TxOut addr (TokenBundle.fromCoin (Coin 10_000_000))))
                . UTxO.toList


newtype ShowBuildable a = ShowBuildable a
    deriving newtype Arbitrary

instance Buildable a => Show (ShowBuildable a) where
    show (ShowBuildable x) = pretty x

instance Arbitrary (Hash "Datum") where
    arbitrary = pure $ Hash $ BS.pack $ replicate 28 0

instance IsCardanoEra era => Arbitrary (Cardano.AddressInEra era) where
      arbitrary = genAddressInEra Cardano.cardanoEra

instance IsCardanoEra era => Arbitrary (Cardano.TxOutDatum ctx era) where
      arbitrary = genTxOutDatum Cardano.cardanoEra

instance IsCardanoEra era => Arbitrary (Cardano.TxOut ctx era) where
      arbitrary = genTxOut Cardano.cardanoEra
      shrink (Cardano.TxOut addr val dat refScript) = tail
          [ Cardano.TxOut addr' val' dat' refScript'
          | addr' <- prependOriginal shrink addr
          , val' <- prependOriginal shrink val
          , dat' <- prependOriginal shrink dat
          , refScript' <- prependOriginal (const []) refScript
          ]

-- NOTE: We should constrain by @IsRecentEra era@ instead, where @RecentEra@ is
-- the two latest eras.
instance IsCardanoEra era => Arbitrary (Cardano.TxOutValue era) where
      arbitrary = case Cardano.cardanoEra @era of
         Cardano.AlonzoEra ->
             Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                 <$> genValueForTxOut
         Cardano.BabbageEra ->
             Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                 <$>  genValueForTxOut
         e -> error $ mconcat
             [ "Arbitrary (TxOutValue "
             , show e
             , ") not implemented)"
             ]

      shrink (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra val) =
          map
              (Cardano.TxOutValue Cardano.MultiAssetInAlonzoEra
                  . toCardanoValue)
              (shrink $ Compatibility.fromCardanoValue val)

      shrink (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra val) =
          map
              (Cardano.TxOutValue Cardano.MultiAssetInBabbageEra
                  . toCardanoValue)
              (shrink $ fromCardanoValue val)
      shrink _ =
        error "Arbitrary (TxOutValue era) is not implemented for old eras"

instance Arbitrary (PartialTx Cardano.AlonzoEra) where
    arbitrary = do
        let era = AlonzoEra
        tx <- genTxForBalancing era
        let (Cardano.Tx (Cardano.TxBody content) _) = tx
        let inputs = Cardano.txIns content
        inputUTxO <- fmap (Cardano.UTxO . Map.fromList) . forM inputs $ \i -> do
            -- NOTE: genTxOut does not generate quantities larger than
            -- `maxBound :: Word64`, however users could supply these.
            -- We should ideally test what happens, and make it clear what code,
            -- if any, should validate.
            o <- genTxOut Cardano.AlonzoEra
            return (fst i, o)
        let redeemers = []
        return $ PartialTx
            tx
            inputUTxO
            redeemers
    shrink (PartialTx tx inputUTxO redeemers) =
        [ PartialTx tx inputUTxO' redeemers
        | inputUTxO' <- shrinkInputResolution inputUTxO
        ] ++
        [ restrictResolution $ PartialTx
            tx'
            inputUTxO
            redeemers
        | tx' <- shrinkTxAlonzo tx
        ]

instance Arbitrary (PartialTx Cardano.BabbageEra) where
    arbitrary = do
        let era = BabbageEra
        tx <- genTxForBalancing era
        let (Cardano.Tx (Cardano.TxBody content) _) = tx
        let inputs = Cardano.txIns content
        inputUTxO <- fmap (Cardano.UTxO . Map.fromList) . forM inputs $ \i -> do
            -- NOTE: genTxOut does not generate quantities larger than
            -- `maxBound :: Word64`, however users could supply these.
            -- We should ideally test what happens, and make it clear what code,
            -- if any, should validate.
            o <- genTxOut Cardano.BabbageEra
            return (fst i, o)
        let redeemers = []
        return $ PartialTx tx inputUTxO redeemers
    shrink (PartialTx tx inputUTxO redeemers) =
        [ PartialTx tx inputUTxO' redeemers
        | inputUTxO' <- shrinkInputResolution inputUTxO
        ] <>
        [ restrictResolution $ PartialTx tx' inputUTxO redeemers
        | tx' <- shrinkTxBabbage tx
        ]


shrinkInputResolution
    :: forall era.
        ( Cardano.IsCardanoEra era
        , Arbitrary (Cardano.TxOut Cardano.CtxUTxO era)
        )
    => Cardano.UTxO era
    -> [Cardano.UTxO era]
shrinkInputResolution =
    shrinkMapBy utxoFromList utxoToList shrinkUTxOEntries
   where
     utxoToList (Cardano.UTxO u) = Map.toList u
     utxoFromList = Cardano.UTxO . Map.fromList

     -- NOTE: We only want to shrink the outputs, keeping the inputs and length
     -- of the list the same.
     shrinkUTxOEntries :: Arbitrary o => [(i, o)] -> [[(i, o)]]
     shrinkUTxOEntries ((i,o) : rest) = mconcat
         -- First shrink the first element
         [ map (\o' -> (i, o') : rest ) (shrink o)
         -- Recurse to shrink subsequent elements on their own
         , map ((i,o):) (shrinkUTxOEntries rest)
         ]
     shrinkUTxOEntries [] = []

instance Semigroup (Cardano.UTxO era) where
    Cardano.UTxO a <> Cardano.UTxO b = Cardano.UTxO (a <> b)

instance Monoid (Cardano.UTxO era) where
    mempty = Cardano.UTxO mempty

shrinkTxAlonzo ::
    Cardano.Tx Cardano.AlonzoEra -> [Cardano.Tx Cardano.AlonzoEra]
shrinkTxAlonzo (Cardano.Tx bod wits) =
    [ Cardano.Tx bod' wits | bod' <- shrinkTxBodyAlonzo bod ]

shrinkTxBabbage ::
    Cardano.Tx Cardano.BabbageEra -> [Cardano.Tx Cardano.BabbageEra]
shrinkTxBabbage (Cardano.Tx bod wits) =
    [ Cardano.Tx bod' wits | bod' <- shrinkTxBodyBabbage bod ]

-- | Restricts the inputs list of the 'PartialTx' to the inputs of the
-- underlying CBOR transaction. This allows us to "fix" the 'PartialTx' after
-- shrinking the CBOR.
--
-- NOTE: Perhaps ideally 'PartialTx' would handle this automatically.
restrictResolution :: PartialTx era -> PartialTx era
restrictResolution (PartialTx tx (Cardano.UTxO u) redeemers) =
    let
        u' = u `Map.restrictKeys` (inputsInTx tx)
    in
        PartialTx tx (Cardano.UTxO u') redeemers
  where
    inputsInTx (Cardano.Tx (Cardano.TxBody bod) _) =
             Set.fromList $ map fst $ Cardano.txIns bod

shrinkTxBodyAlonzo
    :: Cardano.TxBody Cardano.AlonzoEra
    -> [Cardano.TxBody Cardano.AlonzoEra]
shrinkTxBodyAlonzo (Cardano.ShelleyTxBody e bod scripts scriptData aux val) =
    tail
        [ Cardano.ShelleyTxBody e bod' scripts' scriptData' aux' val'
        | bod' <- prependOriginal shrinkLedgerTxBody bod
        , aux' <- aux : filter (/= aux) [Nothing]
        , scriptData' <- prependOriginal shrinkScriptData scriptData
        , scripts' <- prependOriginal (shrinkList (const [])) scripts
        , val' <- case Cardano.txScriptValiditySupportedInShelleyBasedEra e of
            Nothing -> [val]
            Just txsvsie -> val : filter (/= val)
                [ Cardano.TxScriptValidity txsvsie Cardano.ScriptValid ]
        ]
  where
    shrinkLedgerTxBody
        :: Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.AlonzoEra)
        -> [Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.AlonzoEra)]
    shrinkLedgerTxBody body = tail
        [ body
            { Alonzo.txwdrls = wdrls' }
            { Alonzo.outputs = outs' }
            { Alonzo.inputs = ins' }
            { Alonzo.txcerts = certs' }
            { Alonzo.mint = mint' }
            { Alonzo.reqSignerHashes = rsh' }
            { Alonzo.txUpdates = updates' }
            { Alonzo.txfee = txfee' }
        | updates' <-
            prependOriginal shrinkStrictMaybe (Alonzo.txUpdates body)
        , wdrls' <-
            prependOriginal shrinkWdrl (Alonzo.txwdrls body)
        , outs' <-
            prependOriginal (shrinkSeq (const [])) (Alonzo.outputs body)
        , ins' <-
            prependOriginal (shrinkSet (const [])) (Alonzo.inputs body)
        , certs' <-
            prependOriginal (shrinkSeq (const [])) (Alonzo.txcerts body)
        , mint' <-
            prependOriginal shrinkValue (Alonzo.mint body)
        , rsh' <-
            prependOriginal (shrinkSet (const [])) (Alonzo.reqSignerHashes body)
        , txfee' <-
            prependOriginal shrinkFee (Alonzo.txfee body)
        ]

shrinkTxBodyBabbage
    :: Cardano.TxBody Cardano.BabbageEra -> [Cardano.TxBody Cardano.BabbageEra]
shrinkTxBodyBabbage (Cardano.ShelleyTxBody e bod scripts scriptData aux val) =
    tail
        [ Cardano.ShelleyTxBody e bod' scripts' scriptData' aux' val'
        | bod' <- prependOriginal shrinkLedgerTxBody bod
        , aux' <- aux : filter (/= aux) [Nothing]
        , scriptData' <- prependOriginal shrinkScriptData scriptData
        , scripts' <- prependOriginal (shrinkList (const [])) scripts
        , val' <- case Cardano.txScriptValiditySupportedInShelleyBasedEra e of
            Nothing -> [val]
            Just txsvsie -> val : filter (/= val)
                [ Cardano.TxScriptValidity txsvsie Cardano.ScriptValid ]
        ]
  where
    shrinkLedgerTxBody
        :: Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.BabbageEra)
        -> [Ledger.TxBody (Cardano.ShelleyLedgerEra Cardano.BabbageEra)]
    shrinkLedgerTxBody body = tail
        [ body
            { Babbage.txwdrls = wdrls'
            , Babbage.outputs = outs'
            , Babbage.inputs = ins'
            , Babbage.txcerts = certs'
            , Babbage.mint = mint'
            , Babbage.reqSignerHashes = rsh'
            , Babbage.txUpdates = updates'
            , Babbage.txfee = txfee'
            }
        | updates' <-
            prependOriginal shrinkStrictMaybe (Babbage.txUpdates body)
        , wdrls' <-
            prependOriginal shrinkWdrl (Babbage.txwdrls body)
        , outs' <-
            prependOriginal (shrinkSeq (const [])) (Babbage.outputs body)
        , ins' <-
            prependOriginal (shrinkSet (const [])) (Babbage.inputs body)
        , certs' <-
            prependOriginal (shrinkSeq (const [])) (Babbage.txcerts body)
        , mint' <-
            prependOriginal shrinkValue (Babbage.mint body)
        , rsh' <-
            prependOriginal
                (shrinkSet (const [])) (Babbage.reqSignerHashes body)
        , txfee' <-
            prependOriginal shrinkFee (Babbage.txfee body)
        ]

shrinkScriptData
    :: Era (Cardano.ShelleyLedgerEra era)
    => Cardano.TxBodyScriptData era
    -> [Cardano.TxBodyScriptData era]
shrinkScriptData Cardano.TxBodyNoScriptData = []
shrinkScriptData (Cardano.TxBodyScriptData era
    (Alonzo.TxDats dats) (Alonzo.Redeemers redeemers)) = tail
        [ Cardano.TxBodyScriptData era
            (Alonzo.TxDats dats')
            (Alonzo.Redeemers redeemers')
        | dats' <- dats :
            (Map.fromList <$> shrinkList (const []) (Map.toList dats))
        , redeemers' <- redeemers :
            (Map.fromList <$> shrinkList (const []) (Map.toList redeemers))
        ]

-- | For writing shrinkers in the style of https://stackoverflow.com/a/14006575
prependOriginal :: (t -> [t]) -> t -> [t]
prependOriginal shrinker x = x : shrinker x

shrinkValue :: (Eq a, Monoid a) => a -> [a]
shrinkValue v = filter (/= v) [mempty]

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet shrinkElem = map Set.fromList . shrinkList shrinkElem . F.toList

shrinkSeq :: Foldable t => (a -> [a]) -> t a -> [StrictSeq.StrictSeq a]
shrinkSeq shrinkElem =
    map StrictSeq.fromList . shrinkList shrinkElem . F.toList

shrinkFee :: Ledger.Coin -> [Ledger.Coin]
shrinkFee (Ledger.Coin 0) = []
shrinkFee _ = [Ledger.Coin 0]

shrinkWdrl :: Wdrl era -> [Wdrl era]
shrinkWdrl (Wdrl m) = map (Wdrl . Map.fromList) $
    shrinkList shrinkWdrl' (Map.toList m)
    where
    shrinkWdrl' (acc, Ledger.Coin c) =
        [ (acc, Ledger.Coin c')
        | c' <- filter (>= 1) $ shrink c
        ]

shrinkStrictMaybe :: StrictMaybe a -> [StrictMaybe a]
shrinkStrictMaybe = \case
    SNothing -> []
    SJust _ -> [SNothing]

balanceTransaction'
    :: WriteTx.IsRecentEra era
    => Wallet'
    -> StdGenSeed
    -> PartialTx era
    -> Either ErrBalanceTx (Cardano.Tx era)
balanceTransaction' (Wallet' utxo wal pending) seed tx  =
    flip evalRand (stdGenFromSeed seed) $ runExceptT $
        balanceTransaction @_ @(Rand StdGen)
            (nullTracer @(Rand StdGen))
            testTxLayer
            (delegationAddress @'Mainnet)
            mockProtocolParametersForBalancing
            dummyTimeInterpreter
            (utxo, wal, pending)
            tx

prop_posAndNegFromCardanoValueRoundtrip :: Property
prop_posAndNegFromCardanoValueRoundtrip = forAll genSignedValue $ \v ->
    let
        (pos, neg) = posAndNegFromCardanoValue v
    in
        toCardanoValue pos <> (Cardano.negateValue (toCardanoValue neg)) === v

data BalanceTxGolden =
    BalanceTxGoldenSuccess
        Coin -- ^ Wallet balance
        Cardano.Lovelace -- ^ Fee
        Cardano.Lovelace -- ^ Minimum fee
    | BalanceTxGoldenFailure Coin String
    deriving (Eq, Show)

-- CSV with the columns: wallet_balance,(fee,minfee | error)
instance Buildable BalanceTxGolden where
    build (BalanceTxGoldenFailure c err) = mconcat
        [ build c
        , ","
        , build (T.pack err)
        ]
    build (BalanceTxGoldenSuccess c fee minfee) = mconcat
        [ build c
        , ","
        , lovelaceF fee
        , ","
        , lovelaceF minfee
        ]
      where
        lovelaceF (Cardano.Lovelace l)
            | l < 0     = "-" <> pretty (Coin.unsafeFromIntegral (-l))
            | otherwise = pretty (Coin.unsafeFromIntegral l)

testStdGenSeed :: StdGenSeed
testStdGenSeed = StdGenSeed 0

balanceTransactionGoldenSpec
    :: Spec
balanceTransactionGoldenSpec = describe "balance goldens" $ do
    describe "results when varying wallet balance (1 UTxO)" $ do
        test "pingPong_1" pingPong_1
        test "pingPong_2" pingPong_2
        test "delegate" delegate
        test "1ada-payment" payment
  where
    test :: String -> PartialTx Cardano.BabbageEra -> Spec
    test name partialTx = it name $ do
        goldenText name
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
                , goldenFile = dir </> name' </> "golden"
                , actualFile = Just (dir </> name' </> "actual")
                , failFirstTime = False
                }
          where
            dir = $(getTestData) </> "balanceTx"

        mkGolden :: PartialTx Cardano.BabbageEra -> Coin -> BalanceTxGolden
        mkGolden ptx c =
            let
                res = balanceTransaction'
                    (mkTestWallet rootK (utxo [c]))
                    testStdGenSeed
                    ptx
            in
                case res of
                    Right tx
                        -> BalanceTxGoldenSuccess c (txFee tx) (txMinFee tx)
                    Left e
                        -> BalanceTxGoldenFailure c (show e)

        utxo coins = UTxO $ Map.fromList $ zip ins outs
          where
            ins = map (TxIn dummyHash) [0..]
            outs = map (TxOut addr . TokenBundle.fromCoin) coins
            dummyHash = Hash $ B8.replicate 32 '0'

        mw = SomeMnemonic $ either (error . show) id
            (entropyToMnemonic @12 <$> mkEntropy "0000000000000000")
        rootK = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
        addr = Address $ unsafeFromHex
            "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    payment :: PartialTx Cardano.BabbageEra
    payment = paymentPartialTx
        [ TxOut addr (TokenBundle.fromCoin (Coin 1_000_000))
        ]
      where
        addr = Address $ unsafeFromHex
            "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    delegate :: PartialTx Cardano.BabbageEra
    delegate = PartialTx (Cardano.Tx body []) mempty []
      where
        body = Cardano.ShelleyTxBody
            Cardano.ShelleyBasedEraBabbage
            ledgerBody
            []
            Cardano.TxBodyNoScriptData
            Nothing
            Cardano.TxScriptValidityNone

        certs = mkDelegationCertificates delegationAction xpub
          where
            poolId = PoolId "\236(\243=\203\230\214@\n\RS^3\155\208d|\
                            \\ts\202l\f\249\194\187\230\131\141\198"
            xpub = getRawKey $ publicKey rootK
            mw = SomeMnemonic $ either (error . show) id
                (entropyToMnemonic @12 <$> mkEntropy "0000000000000001")
            rootK = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
            delegationAction = JoinRegisteringKey poolId
        ledgerBody = Babbage.TxBody
          { Babbage.inputs = mempty
          , Babbage.collateral = mempty
          , Babbage.outputs = mempty
          , Babbage.txcerts
            = StrictSeq.fromList $ map Cardano.toShelleyCertificate certs
          , Babbage.txwdrls = Wdrl mempty
          , Babbage.txfee = mempty
          , Babbage.txvldt = ValidityInterval SNothing SNothing
          , Babbage.txUpdates = SNothing
          , Babbage.reqSignerHashes = mempty
          , Babbage.mint = mempty
          , Babbage.scriptIntegrityHash = SNothing
          , Babbage.adHash = SNothing
          , Babbage.txnetworkid = SNothing
          , Babbage.referenceInputs = mempty
          , Babbage.collateralReturn = SNothing
          , Babbage.totalCollateral = SNothing
          }

    txFee :: Cardano.Tx Cardano.BabbageEra -> Cardano.Lovelace
    txFee (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

    txMinFee :: Cardano.Tx Cardano.BabbageEra -> Cardano.Lovelace
    txMinFee = toCardanoLovelace
        . evaluateMinimumFee testTxLayer (snd mockProtocolParametersForBalancing)

-- NOTE: 'balanceTransaction' relies on estimating the number of witnesses that
-- will be needed. The correctness of this estimation is not tested here.
--
-- TODO: Ensure scripts are well tested
--   - Ensure we have coverage for normal plutus contracts
--
-- TODO: Generate data for other eras than Alonzo
prop_balanceTransactionValid
    :: Wallet'
    -> ShowBuildable (PartialTx Cardano.AlonzoEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionValid wallet (ShowBuildable partialTx) seed
    = withMaxSuccess 1_000 $ do
        let combinedUTxO = mconcat
                [ view #inputs partialTx
                , Compatibility.toCardanoUTxO Cardano.ShelleyBasedEraAlonzo walletUTxO
                ]
        let originalBalance = txBalance (view #tx partialTx) combinedUTxO
        let res = balanceTransaction'
                wallet
                seed
                partialTx
        case res of
            Right tx -> counterexample ("\nResult: " <> show (Pretty tx)) $ do
                label "success"
                    $ classify (originalBalance == mempty)
                        "already balanced"
                    $ classify (txFee tx > Cardano.Lovelace 1_000_000)
                        "fee above 1 ada"
                    $ classify (hasZeroAdaOutputs $ view #tx partialTx)
                        "partial tx had zero ada outputs"
                    $ classify (hasCollateral tx)
                        "balanced tx has collateral"
                    $ conjoin
                        [ txBalance tx combinedUTxO === mempty
                        , prop_validSize tx
                        , prop_minfeeIsCovered tx
                        , let
                              minUTxOValue = Cardano.Lovelace 999_978
                              upperBoundCostOfOutput = Cardano.Lovelace 1_000
                          in
                              -- Coin selection should only pay more fees than
                              -- required when it can't afford to create a
                              -- change output with the minimumUTxOValue.
                              prop_expectFeeExcessSmallerThan
                                  (minUTxOValue <> upperBoundCostOfOutput)
                                  tx

                        -- FIXME [ADP-2419] Re-enable when we have stricter
                        -- validation. Will otherwise fail with:
                        --
                        -- @
                        --     --match balanceTransaction --seed 139473932`
                        -- @
                        --
                        -- , prop_outputsSatisfyMinAdaRequirement tx
                        ]
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf (BalanceInsufficient err)))) -> do
                let missing = balanceMissing err
                case (view #coin missing == Coin 0, view #tokens missing == mempty) of
                    (False, False) -> label "missing coin and tokens" $ property True
                    (False, True) -> label "missing coin" $ property True
                    (True, False) -> label "missing tokens" $ property True
                    (True, True) -> property False
            Left (ErrBalanceTxUpdateError (ErrExistingKeyWitnesses _)) ->
                label "existing key wits" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionOutputErrorOf
                (SelectionOutputError _index
                (SelectionOutputCoinInsufficient _))))) ->
                label "output below minCoinValue" $ property True
            Left (ErrBalanceTxExistingCollateral) ->
                label "existing collateral" True
            Left (ErrBalanceTxExistingTotalCollateral) ->
                label "existing total collateral" True
            Left (ErrBalanceTxExistingReturnCollateral) ->
                label "existing collateral return outputs" True
            Left ErrBalanceTxMaxSizeLimitExceeded ->
                label "maxTxSize limit exceeded" $ property True
            Left ErrBalanceTxConflictingNetworks ->
                label "conflicting networks" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf EmptyUTxO))) ->
                label "empty UTxO" $ property True
            Left (ErrBalanceTxInternalError
                 (ErrUnderestimatedFee delta candidateTx)) ->
                let counterexampleText = mconcat
                        [ "underestimated fee by "
                        , pretty delta
                        , "\n candidate tx: "
                        , pretty candidateTx
                        ]
                in
                    counterexample counterexampleText $ property False
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf
                (SelectionLimitReached _)))) ->
                    let msg = "selection limit reached should never be returned"
                    in counterexample msg $ property False
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf (UnableToConstructChange _)))) ->
                label "unable to construct change" $ property True
            Left ErrBalanceTxInputResolutionConflicts{} ->
                label "input resolution conflicts" $ property True
            Left err -> label "other error" $
                counterexample ("balanceTransaction failed: " <> show err) False
  where
    prop_expectFeeExcessSmallerThan
        :: Cardano.Lovelace -> Cardano.Tx Cardano.AlonzoEra -> Property
    prop_expectFeeExcessSmallerThan lim tx = do
        let fee = txFee tx
        let minfee = txMinFee tx
        let unLovelace (Cardano.Lovelace x) = x
        let delta = Cardano.Lovelace $ (unLovelace fee) - (unLovelace minfee)
        let msg = unwords
                [ "The fee", showInParens fee
                , "was", show delta
                , "larger than the minimum fee", showInParens minfee <> ","
                , "which is a larger difference than", show lim
                ]
        counterexample msg $ property $ delta < lim
      where
        showInParens x = "(" <> show x <> ")"

    prop_minfeeIsCovered :: Cardano.Tx Cardano.AlonzoEra -> Property
    prop_minfeeIsCovered tx = do
        let fee = txFee tx
        let minfee = txMinFee tx
        let unLovelace (Cardano.Lovelace x) = x
        let delta = Cardano.Lovelace $ (unLovelace minfee) - (unLovelace fee)
        let msg = unwords
                [ "The minimum fee was"
                , show minfee
                , "but the actual fee,"
                , show fee
                , "was lower by"
                , show delta
                ]
        counterexample msg $ property $ fee >= minfee

    prop_validSize :: Cardano.Tx Cardano.AlonzoEra -> Property
    prop_validSize tx = do
        let (TxSize size) =
                estimateSignedTxSize
                    testTxLayer
                    (snd mockProtocolParametersForBalancing)
                    tx
        let limit = fromIntegral $ getQuantity $
                view (#txParameters . #getTxMaxSize) mockProtocolParameters
        let msg = unwords
                [ "The tx size "
                , show size
                , "must be lower than the maximum size "
                , show limit
                , ", tx:"
                , show tx
                ]
        counterexample msg $ property (size <= limit)

    _prop_outputsSatisfyMinAdaRequirement
        :: Cardano.Tx Cardano.AlonzoEra
        -> Property
    _prop_outputsSatisfyMinAdaRequirement (Cardano.ShelleyTx _ tx) = do
        let outputs = WriteTx.outputs era $ WriteTx.txBody era tx
        conjoin $ map valid outputs
      where
        era = WriteTx.RecentEraAlonzo

        valid :: WriteTx.TxOut WriteTx.StandardAlonzo -> Property
        valid out = counterexample msg $ property $
            not $ WriteTx.isBelowMinimumCoinForTxOut era pp out
          where
            pp = ledgerPParams
            msg = unwords
                [ "ada quantity is"
                , "below minimum requirement"
                , "\nin\n"
                , show out
                , "\n"
                , "Suggested ada quantity (may overestimate requirement):"
                , show $ WriteTx.computeMinimumCoinForTxOut
                    WriteTx.RecentEraAlonzo
                    ledgerPParams
                    out
                ]

    walletUTxO :: UTxO
    walletUTxO =
        let Wallet' _ w _ = wallet
        in view #utxo w

    hasZeroAdaOutputs :: Cardano.Tx Cardano.AlonzoEra -> Bool
    hasZeroAdaOutputs (Cardano.Tx (Cardano.ShelleyTxBody _ body _ _ _ _) _) =
        any hasZeroAda (Alonzo.outputs body)
      where
        hasZeroAda (Alonzo.TxOut _ val _) = Value.coin val == Ledger.Coin 0

    hasCollateral :: Cardano.Tx era -> Bool
    hasCollateral (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txInsCollateral content of
            Cardano.TxInsCollateralNone -> False
            Cardano.TxInsCollateral _ [] -> False
            Cardano.TxInsCollateral _ (_:_) -> True

    txFee :: Cardano.Tx Cardano.AlonzoEra -> Cardano.Lovelace
    txFee (Cardano.Tx (Cardano.TxBody content) _) =
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

    txMinFee :: Cardano.Tx Cardano.AlonzoEra -> Cardano.Lovelace
    txMinFee = toCardanoLovelace
        . evaluateMinimumFee testTxLayer nodePParams

    txBalance
        :: Cardano.Tx Cardano.AlonzoEra
        -> Cardano.UTxO Cardano.AlonzoEra
        -> Cardano.Value
    txBalance (Cardano.Tx bod _) u =
        valueFromCardanoTxOutValue
        $ Cardano.evaluateTransactionBalance nodePParams mempty u bod

    valueFromCardanoTxOutValue
        :: forall era. Cardano.TxOutValue era -> Cardano.Value
    valueFromCardanoTxOutValue = \case
        TxOutAdaOnly _ coin -> Cardano.lovelaceToValue coin
        TxOutValue _ val -> val

    (_, nodePParams) = mockProtocolParametersForBalancing
    ledgerPParams = Cardano.toLedgerPParams
        Cardano.ShelleyBasedEraAlonzo nodePParams

prop_balanceTransactionExistingTotalCollateral
    :: Wallet'
    -> ShowBuildable (PartialTx Cardano.BabbageEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionExistingTotalCollateral
    wallet (ShowBuildable partialTx@PartialTx{tx}) seed = withMaxSuccess 10 $
        hasTotalCollateral tx
            && not (hasInsCollateral tx)
            && not (hasReturnCollateral tx) ==>
        case balanceTransaction' wallet seed partialTx of
            Left err -> ErrBalanceTxExistingTotalCollateral === err
            e -> counterexample (show e) False

prop_balanceTransactionExistingReturnCollateral
    :: Wallet'
    -> ShowBuildable (PartialTx Cardano.BabbageEra)
    -> StdGenSeed
    -> Property
prop_balanceTransactionExistingReturnCollateral
    wallet (ShowBuildable partialTx@PartialTx{tx}) seed = withMaxSuccess 10 $
        hasReturnCollateral tx
            && not (hasInsCollateral tx)
            && not (hasTotalCollateral tx) ==>
        case balanceTransaction' wallet seed partialTx of
            Left err -> ErrBalanceTxExistingReturnCollateral === err
            e -> counterexample (show e) False

hasInsCollateral :: Cardano.Tx era -> Bool
hasInsCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txInsCollateral content of
        Cardano.TxInsCollateralNone -> False
        Cardano.TxInsCollateral _ [] -> False
        Cardano.TxInsCollateral _ _ -> True

hasTotalCollateral :: Cardano.Tx era -> Bool
hasTotalCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txTotalCollateral content of
        Cardano.TxTotalCollateralNone -> False
        Cardano.TxTotalCollateral _ _ -> True

hasReturnCollateral :: Cardano.Tx era -> Bool
hasReturnCollateral (Cardano.Tx (Cardano.TxBody content) _) =
    case Cardano.txReturnCollateral content of
        Cardano.TxReturnCollateralNone -> False
        Cardano.TxReturnCollateral _ _ -> True

-- | Consistent pair of ProtocolParameters of both wallet and cardano-api types.
--
-- We try to use similar parameters to mainnet where it matters (in particular
-- fees, execution unit prices, and the cost model.)
--
-- NOTE: We don't have a 'Cardano.ProtocolParameters -> ProtocolParameters'
-- function, so we need to manually ensure the hard-coded values are consistent.
mockProtocolParametersForBalancing
    :: (ProtocolParameters, Cardano.ProtocolParameters)
mockProtocolParametersForBalancing = (mockProtocolParameters, nodePParams)
  where
    nodePParams = Cardano.ProtocolParameters
        { Cardano.protocolParamTxFeeFixed = 155_381
        , Cardano.protocolParamTxFeePerByte = 44
        , Cardano.protocolParamMaxTxSize = 16_384
        , Cardano.protocolParamMinUTxOValue = Nothing
        , Cardano.protocolParamMaxTxExUnits =
            Just $ Cardano.ExecutionUnits 10_000_000_000 14_000_000
        , Cardano.protocolParamMaxValueSize = Just 4_000
        , Cardano.protocolParamProtocolVersion = (6, 0)
        , Cardano.protocolParamDecentralization = Just 0
        , Cardano.protocolParamExtraPraosEntropy = Nothing
        , Cardano.protocolParamMaxBlockHeaderSize = 100_000 -- Dummy value
        , Cardano.protocolParamMaxBlockBodySize = 100_000
        , Cardano.protocolParamStakeAddressDeposit = Cardano.Lovelace 2_000_000
        , Cardano.protocolParamStakePoolDeposit = Cardano.Lovelace 500_000_000
        , Cardano.protocolParamMinPoolCost = Cardano.Lovelace 32_000_000
        , Cardano.protocolParamPoolRetireMaxEpoch = Cardano.EpochNo 2
        , Cardano.protocolParamStakePoolTargetNum = 100
        , Cardano.protocolParamPoolPledgeInfluence = 0
        , Cardano.protocolParamMonetaryExpansion = 0
        , Cardano.protocolParamTreasuryCut  = 0
        , Cardano.protocolParamUTxOCostPerWord =
            Just $ fromIntegral $ SL.unCoin testParameter_coinsPerUTxOWord_Alonzo
        , Cardano.protocolParamUTxOCostPerByte =
            Just $ fromIntegral $ SL.unCoin testParameter_coinsPerUTxOByte_Babbage
        , Cardano.protocolParamCostModels =
            Map.singleton
                (Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV1)
                costModel
        , Cardano.protocolParamPrices =
            Just $ Cardano.ExecutionUnitPrices
                (721 % 10_000_000)
                (577 % 10_000)
        , Cardano.protocolParamMaxBlockExUnits =
            Just $ Cardano.ExecutionUnits 10_000_000_000 14_000_000
        , Cardano.protocolParamCollateralPercent = Just 1
        , Cardano.protocolParamMaxCollateralInputs = Just 3
        }
    costModel = Cardano.CostModel
        . fromMaybe (error "Plutus.defaultCostModelParams")
        $ Plutus.defaultCostModelParams


block0 :: Block
block0 = Block
    { header = BlockHeader
        { slotNo = SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = mockHash $ SlotNo 0
        , parentHeaderHash = Nothing
        }
    , transactions = []
    , delegations = []
    }

updateSealedTxSpec :: Spec
updateSealedTxSpec = do
    describe "updateSealedTx" $ do
        describe "no existing key witnesses" $ do
            txs <- readTestTransactions
            forM_ txs $ \(filepath, sealedTx) -> do
                let anyShelleyEraTx
                        = fromJust $ asAnyShelleyBasedEra $ cardanoTx sealedTx
                it ("without TxUpdate: " <> filepath) $ do
                    withShelleyBasedTx anyShelleyEraTx $ \tx ->
                        case updateSealedTx tx noTxUpdate of
                            Left e ->
                                expectationFailure $
                                "expected update to succeed but failed: "
                                    <> show e
                            Right tx' -> do
                                if tx /= tx' && show tx == show tx'
                                -- The transaction encoding has changed.
                                -- Unfortunately transactions are compared using
                                -- their memoized bytes, but shown without their
                                -- memoized bytes. This leads to the very
                                -- confusing situation where the show result of
                                -- two transactions is identical, but the (==)
                                -- result of two transactions shows a
                                -- discrepancy.
                                --
                                -- In this case we expect failure and write out
                                -- the new memoized bytes to a file so the
                                -- developer can update the binary test data.
                                then do
                                  let
                                     newEncoding = convertToBase Base16 $ Cardano.serialiseToCBOR tx'
                                     rejectFilePath = $(getTestData) </> "plutus" </> filepath <> ".rej"
                                  BS.writeFile rejectFilePath newEncoding
                                  expectationFailure $ "Transaction encoding has changed, making comparison impossible, see .rej file: " <> rejectFilePath
                                else
                                  Cardano.serialiseToCBOR tx
                                      `shouldBe` Cardano.serialiseToCBOR tx'

                prop ("with TxUpdate: " <> filepath) $
                    prop_updateSealedTx anyShelleyEraTx

        describe "existing key witnesses" $ do
            it "returns `Left err` with noTxUpdate" $ do
                -- Could be argued that it should instead return `Right tx`.
                let anyShelleyTx = shelleyBasedTxFromBytes
                        $ unsafeFromHex txWithInputsOutputsAndWits
                withShelleyBasedTx anyShelleyTx $ \tx ->
                        updateSealedTx tx noTxUpdate
                            `shouldBe` Left (ErrExistingKeyWitnesses 2)

            it "returns `Left err` when extra body content is non-empty" $ do
                pendingWith "todo: add test data"

unsafeSealedTxFromHex :: ByteString -> IO SealedTx
unsafeSealedTxFromHex =
    either (fail . show) pure
        . sealedTxFromBytes
        . unsafeFromHex
        . BS.dropWhileEnd isNewlineChar
  where
    isNewlineChar c = c `elem` [10,13]

prop_updateSealedTx
    :: Cardano.InAnyShelleyBasedEra Cardano.Tx
    -> [(TxIn, TxOut)]
    -> [TxIn]
    -> [TxOut]
    -> Coin
    -> Property
prop_updateSealedTx
    (Cardano.InAnyShelleyBasedEra era tx)
    extraIns extraCol extraOuts newFee =
    do
        let extra = TxUpdate extraIns extraCol extraOuts (UseNewTxFee newFee)
        let tx' = either (error . show) id
                $ updateSealedTx tx extra
        conjoin
            [ inputs tx' === inputs tx <> Set.fromList (fst <$> extraIns)
            , outputs tx' === outputs tx <> Set.fromList extraOuts
            , sealedFee tx' === Just newFee
            , collateralIns tx' ===
                if isAlonzoOrLater era
                then collateralIns tx <> Set.fromList extraCol
                else mempty
            ]
  where
    -- No 'Ord' on 'AnyCardanoEra'
    isAlonzoOrLater Cardano.ShelleyBasedEraAlonzo  = True
    isAlonzoOrLater Cardano.ShelleyBasedEraBabbage = True
    isAlonzoOrLater _                              = False

    inputs = sealedInputs . sealedTxFromCardano'
    outputs = sealedOutputs . sealedTxFromCardano'
    collateralIns = sealedCollateralInputs . sealedTxFromCardano'

estimateSignedTxSizeSpec :: Spec
estimateSignedTxSizeSpec =
    describe "estimateSignedTxSize" $ do
        it "equals the binary size of signed txs" $ property $ do
            forAllGoldens signedTxGoldens $ \hexTx -> do
                let bs = unsafeFromHex hexTx
                let tx = shelleyBasedTxFromBytes bs
                -- 'mockProtocolParametersForBalancing' is not valid for
                -- 'ShelleyEra'.
                let pparams = (snd mockProtocolParametersForBalancing)
                        { Cardano.protocolParamMinUTxOValue = Just 1_000_000
                        }
                withShelleyBasedTx tx
                    (estimateSignedTxSize testTxLayer pparams)
                    `shouldBe`
                    TxSize (fromIntegral $ BS.length bs)
  where
    forAllGoldens goldens f = forM_ goldens $ \x ->
        Hspec.counterexample (show x) $ f x

fst6 :: (a, b, c, d, e, f) -> a
fst6 (a,_,_,_,_,_) = a

sealedInputs :: SealedTx -> Set TxIn
sealedInputs =
    Set.fromList
    . map fst
    . view #resolvedInputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

sealedCollateralInputs
    :: SealedTx -> Set TxIn
sealedCollateralInputs =
    Set.fromList
    . map fst
    . view #resolvedCollateralInputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

sealedOutputs
    :: SealedTx -> Set TxOut
sealedOutputs =
    Set.fromList
    . view #outputs
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)

sealedNumberOfRedeemers :: SealedTx -> Int
sealedNumberOfRedeemers sealedTx =
    case cardanoTx sealedTx of
        InAnyCardanoEra ByronEra _   -> 0
        InAnyCardanoEra ShelleyEra _ -> 0
        InAnyCardanoEra AllegraEra _ -> 0
        InAnyCardanoEra MaryEra _    -> 0
        InAnyCardanoEra AlonzoEra (Cardano.Tx body _) ->
            let dats =
                    case body of
                        Cardano.ShelleyTxBody _ _ _ d _ _ -> d
            in case dats of
                    Cardano.TxBodyNoScriptData ->
                        0
                    Cardano.TxBodyScriptData _ _ (Alonzo.Redeemers rdmrs) ->
                        Map.size rdmrs
        InAnyCardanoEra BabbageEra (Cardano.Tx body _) ->
            let dats =
                    case body of
                        Cardano.ShelleyTxBody _ _ _ d _ _ -> d
            in case dats of
                    Cardano.TxBodyNoScriptData ->
                        0
                    Cardano.TxBodyScriptData _ _ (Alonzo.Redeemers rdmrs) ->
                        Map.size rdmrs

sealedFee
    :: forall era. Cardano.IsCardanoEra era => Cardano.Tx era -> Maybe Coin
sealedFee =
    view #fee
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)
    . sealedTxFromCardano'

paymentPartialTx :: [TxOut] -> PartialTx Cardano.BabbageEra
paymentPartialTx txouts = PartialTx (Cardano.Tx body []) mempty []
  where
    body = Cardano.ShelleyTxBody
        Cardano.ShelleyBasedEraBabbage
        babbageBody
        []
        Cardano.TxBodyNoScriptData
        Nothing
        Cardano.TxScriptValidityNone

    -- NOTE: We should write this as @emptyTxBody { outputs = ... }@.
    -- The next time we bump the ledger , this may already be availible to us
    -- with 'mkBabbageTxBody' and 'initialTxBodyRaw'.
    -- https://github.com/input-output-hk/cardano-ledger/blob/17649bc09e4c47923f7ad103d337cc1b8e6d3078/eras/babbage/impl/src/Cardano/Ledger/Babbage/TxBody.hs#L940
    babbageBody = Babbage.TxBody
        { Babbage.inputs = mempty
        , Babbage.collateral = mempty
        , Babbage.outputs = StrictSeq.fromList $
            map (Ledger.mkSized . (`toBabbageTxOut` Nothing)) txouts
        , Babbage.txcerts = mempty
        , Babbage.txwdrls = Wdrl mempty
        , Babbage.txfee = mempty
        , Babbage.txvldt = ValidityInterval SNothing SNothing
        , Babbage.txUpdates = SNothing
        , Babbage.reqSignerHashes = mempty
        , Babbage.mint = mempty
        , Babbage.scriptIntegrityHash = SNothing
        , Babbage.adHash = SNothing
        , Babbage.txnetworkid = SNothing
        , Babbage.referenceInputs = mempty
        , Babbage.collateralReturn = SNothing
        , Babbage.totalCollateral = SNothing
        }

pingPong_1 :: PartialTx Cardano.BabbageEra
pingPong_1 = PartialTx tx mempty []
  where
    tx = deserializeBabbageTx $ unsafeFromHex
        "84a500800d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd4\
        \5bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04ba\
        \bed17320d8d1b9ff9ad086e86f44ec02000e80a10481d87980f5f6"

pingPong_2 :: PartialTx Cardano.BabbageEra
pingPong_2 = PartialTx
    { tx = deserializeBabbageTx $ mconcat
        [ unsafeFromHex "84a50081825820"
        , tid
        , unsafeFromHex "000d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e848058208392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e02000e80a20381591b72591b6f01000033233332222333322223322332232323332223233322232333333332222222232333222323333222232323322323332223233322232323322332232323333322222332233223322332233223322223223223232533530333330083333573466e1d40192004204f23333573466e1d401d2002205123333573466e1d40212000205323504b35304c3357389201035054310004d49926499263333573466e1d40112004205323333573466e1d40152002205523333573466e1d40192000205723504b35304c3357389201035054310004d49926499263333573466e1cd55cea8012400046601664646464646464646464646666ae68cdc39aab9d500a480008cccccccccc064cd409c8c8c8cccd5cd19b8735573aa004900011980f981d1aba15002302c357426ae8940088d4164d4c168cd5ce2481035054310005b49926135573ca00226ea8004d5d0a80519a8138141aba150093335502e75ca05a6ae854020ccd540b9d728169aba1500733502704335742a00c66a04e66aa0a8098eb4d5d0a8029919191999ab9a3370e6aae754009200023350213232323333573466e1cd55cea80124000466a05266a084eb4d5d0a80118239aba135744a00446a0ba6a60bc66ae712401035054310005f49926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502733504275a6ae854008c11cd5d09aba2500223505d35305e3357389201035054310005f49926135573ca00226ea8004d5d09aba2500223505935305a3357389201035054310005b49926135573ca00226ea8004d5d0a80219a813bae35742a00666a04e66aa0a8eb88004d5d0a801181c9aba135744a00446a0aa6a60ac66ae71241035054310005749926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea00290031180f181d9aba135573ca00646666ae68cdc3a801240084603a608a6ae84d55cf280211999ab9a3370ea00690011180e98181aba135573ca00a46666ae68cdc3a80224000460406eb8d5d09aab9e50062350503530513357389201035054310005249926499264984d55cea80089baa001357426ae8940088d4124d4c128cd5ce249035054310004b49926104a1350483530493357389201035054350004a4984d55cf280089baa001135573a6ea80044d55ce9baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa078446666aae7c004940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06c6a606e66ae71241035054310003849926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235032353033335738921035054310003449926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540d888c8cccd55cf80112804919a80419aa81898031aab9d5002300535573ca00460086ae8800c0b84d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c490103505431000244992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce249035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55cea80089baa001212230020032122300100320011122232323333573466e1cd55cea80124000466aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004448848cc00400c008448004488800c488800848880048004488800c488800848880048004448c8c00400488cc00cc008008004c8c8cc88cc88c8ccc888c8c8c8c8c8ccc888ccc888ccc888c8cccc8888c8cc88c8cccc8888c8cc88c8cc88c8ccc888c8c8cc88c8c8cc88c8c8c8cccc8888c8c8c8c8c8cc88c8cc88cc88ccccccccccccc8888888888888c8c8c8c8c8cccccccc88888888cc88cc88cc88cc88c8ccccc88888c8cc88cc88cc88c8cc88cc88cc88c8cc88c8c8c8cccc8888cccc8888c8888d4d540400108888c8c8c94cd4c24004ccc0140280240205400454cd4c24004cd5ce249025331000910115001109101153353508101003215335309001333573466e1cccc109400cd4c07800488004c0580212002092010910115002153353090013357389201025332000910115002109101150011533535080013300533501b00833303e03f5001323355306012001235355096010012233550990100233553063120012353550990100122335509c0100233704900080080080099a809801180a003003909a9aa84a8080091911a9a80f00091299a984a0098050010a99a984a00999aa9837090009a835283491a9aa84d8080091199aa9838890009a836a83611a9aa84f0080091199ab9a3370e900000084e0084d808008008a8020a99a984a0099ab9c49102533300095011500410950113535501e00522253353097013333355027253335301400113374a90001bb14984cdd2a40046ec52613374a90021bb149800c008c8cd400541d141d4488cc008cd40ac01cccc124128018cd4078034c07c04400403c4264044cd5ce249025335000980113535501a0012225335309301333335502325301d00100300200100b109501133573892010253340009401133573892010253360008f0113530220052235302d002222222222253353508b013303000a00b2135303a0012235303e0012220021350a10135309d0133573892010253300009e01498cccd5403488d4d404c008894ccd4c02400c54ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f054ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f04d41f4cd542400554034cd4058019419894ccd4c008004421c04421c044220048882280541e0488800c488800848880048004488800c48880084888004800444ccd5401d416541654164494cd4d41b8004848cd4168cd5421404d4c03000888004cd4168cd54214040052002505b505b12505a235355081013530100012235301b00222222222225335350793301e00a00b213530280012235302c00122235303100322335308701002230930116253353508201004213355098010020011309301161308a01162200211222212333300100500400300211200120011122212333001004003002112001122123300100300212001221233001003002200111222225335307533355304f120013504b504a235300b002223301500200300415335307533355304f120013504b504a235300b002223530160022222222222353501500d22533530840133355305e120013505450562353025001223304b00200400c10860113357389201024c30000850100315335307533355304f120013504b504a235300b002223530160022222222222353501300d22533530840133355305e12001350545056235302700122253353507a00121533530890133305108501003006153353507b330623019007009213308501001002108a01108a011089015335350763301b00c00d2135302500122353029001222333553055120012235302e00222235303300822353035005225335309301333308401004003002001133506f0090081008506701113508c01353088013357389201024c6600089014984218044cd5ce2481024c3100085010021077150741507415074122123300100300212001122123300100300212001221233001003002200122533335300300121505f21505f21505f2133355304612001504a235300d001225335306f3303300200413506300315062003212222300400521222230030052122223002005212222300100520013200135506c22233333333333353019001235300500322222222225335307153353506333355304b12001504f253353072333573466e3c0300041d01cc4d41980045419400c841d041c841cc4cd5ce249024c340007222353006004222222222253353506453353506433355304c1200150502353550790012253353075333573466e3c00803c1dc1d84d41a400c541a000884d419cd4d541e40048800454194854cd4c1ccccd5cd19baf00100c0750741075150701506f235300500322222222225335307133355304b120013504150432333573466ebc0300041d01cccd54c108480048d4d541e00048800400841cc4cd5ce249024c320007222225335306a333573466e1cd4c0200188888888888ccc09801c0380300041b01ac41b04cd5ce2481024c390006b22235300700522222222225335307333355304d1200135043504523530160012225335350690012153353078333040074003010153353506a35301601422222222223305b01b0022153353079333573466e3c0040081ec1e84d4c07401488cccc1b0008004c1d005541b841e841e441e441e002441d44cd5ce249024c6200074225335306833303002f0013335530331200150175045353006004222222222233355303d120012235301600222235301b00322335307100225335307a333573466e3c0500041f01ec4cd415801401c401c801d413c02441a84cd5ce2481024c610006925335306733302f02e001353005003222222222233355304b12001501f235301400122200200910691335738921024c36000682533530673335530411200135037503923300500400100110691335738921024c640006825335306733302f02e001353005003222222222233355304b12001501f23530120012235301600122200200a106913357389201024c35000682353005003222222222253353506333355304b12001504f235301200122533530743303800200e1350680031506700a213530120012235301600122253353506900121507610791506f22353006004222222222253353506433355304c120015050235301300122533530753303900200f1350690031506800a2107513357389201024c380007323530050032222222222353503100b22353503500222353503500822353503900222533530793333333222222253335306d33350640070060031533530800100215335308001005133350610070010041081011333506100700100410810113335061007001004333333335064075225335307b333573466e1c0080041f41f041ac54cd4c1ecccd5cd19b8900200107d07c1069106a22333573466e200080041f41f010088ccd5cd19b8900200107c07d22333573466e200080041f01f4894cd4c1ecccd5cd19b8900200107d07c10011002225335307b333573466e240080041f41f04008400401801401c00800400c41ec4cd5ce249024c330007a222222222212333333333300100b00a009008007006005004003002200122123300100300220012221233300100400300220012212330010030022001212222222300700822122222223300600900821222222230050081222222200412222222003221222222233002009008221222222233001009008200113350325001502f13001002222335530241200123535505a00122335505d002335530271200123535505d001223355060002333535502500123300a4800000488cc02c0080048cc02800520000013301c00200122337000040024446464600200a640026aa0b64466a6a05e0029000111a9aa82e00111299a982c199ab9a3371e0040120b40b22600e0022600c006640026aa0b44466a6a05c0029000111a9aa82d80111299a982b999ab9a3371e00400e0b20b020022600c00642444444444444601801a4424444444444446601601c01a42444444444444601401a44442444444444444666601202001e01c01a444244444444444466601001e01c01a4424444444444446600e01c01a42444444444444600c01a42444444444444600a01a42444444444444600801a42444444444444600601a4424444444444446600401c01a42444444444444600201a400224424660020060042400224424660020060042400244a66a607c666ae68cdc79a9801801110011a98018009100102001f8999ab9a3370e6a6006004440026a60060024400208007e207e442466002006004400244666ae68cdc480100081e81e111199aa980a890009a808a80811a9aa82100091199aa980c090009a80a280991a9aa82280091199a9aa8068009198052400000244660160040024660140029000000998020010009119aa98050900091a9aa8200009119aa821801199a9aa804000919aa98070900091a9aa8220009119aa8238011aa80780080091199aaa80401c801000919aa98070900091a9aa8220009119aa8238011aa806800800999aaa80181a001000888911199aa980209000a80a99aa98050900091a9aa8200009119aa8218011aa805800999aa980209000911a9aa82080111299a981e999aa980b890009a806a80791a9aa82200091198050010028030801899a80c802001a80b00099aa98050900091a9aa820000911919aa8220019800802990009aa82291299a9a80c80089aa8058019109a9aa82300111299a982119806001004099aa80800380089803001801190009aa81f1108911299a9a80a800880111099802801199aa980389000802802000889091118018020891091119801002802089091118008020890008919a80891199a9a803001910010010009a9a80200091000990009aa81c110891299a9a8070008a80811099a808980200119aa980309000802000899a80111299a981800108190800817891091980080180109000899a80191299a9816801080088170168919a80591199a9a802001910010010009a9a8010009100089109198008018010900091299a9a80d999aa980189000a80391a9aa81800091299a9816199ab9a3375e00200a05c05a26a0400062a03e002426a03c6a6aa060002440042a038640026aa05e4422444a66a6a00c00226a6a01400644002442666a6a01800a440046008004666aa600e2400200a00800222440042442446600200800624002266a00444a66a6a02c004420062002a02a24424660020060042400224446a6a008004446a6a00c00644a666a6026666a01400e0080042a66a604c00620022050204e2050244246600200600424002244464646464a666a6a01000c42a666a6a01200c42a666a6a0140104260082c260062c2a666a6a01400e4260082c260062c202a20262a666a6a01200e4260082c260062c2a666a6a01200c4260082c260062c20282a666a6a01000a42024202620222a666a6a01000a42a666a6a01200e42600a2c260082c2a666a6a01200c42600a2c260082c202820242a666a6a01000c42600a2c260082c2a666a6a01000a42600a2c260082c20264a666a6a01000a42a666a6a01200e42a666a6a01400e42666a01e014004002260222c260222c260202c20262a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c202420224a666a6a00e00842a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c20242a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c202220204a666a6a00c00642a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c20222a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c2020201e4a666a6a00a00442a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c20202a666a6a00a00642a666a6a00c00642666a01600c0040022601a2c2601a2c260182c201e201c2424446006008224440042244400224002246a6a0040024444444400e244444444246666666600201201000e00c00a008006004240024c244400624440042444002400244446466a601800a466a601a0084a66a602c666ae68cdc780100080c00b8a801880b900b919a9806802100b9299a980b199ab9a3371e00400203002e2a006202e2a66a6a00a00642a66a6a00c0044266a6014004466a6016004466a601e004466a60200044660280040024034466a6020004403446602800400244403444466a601a0084034444a66a6036666ae68cdc380300180e80e0a99a980d999ab9a3370e00a00403a03826602e00800220382038202a2a66a6a00a0024202a202a2424460040062244002240024244600400644424466600200a00800640024244600400642446002006400244666ae68cdc780100080480411199ab9a3370e00400201000e266ae712401024c630000413357389201024c370000313357389201024c64000021220021220012001235006353002335738921024c6700003498480048004448848cc00400c008448004498448c8c00400488cc00cc0080080050482d87a80d87980f5f6"
        ]
    , inputs = WriteTx.toCardanoUTxO $ WriteTx.utxoFromTxOutsInLatestEra
        [ ( WriteTx.unsafeMkTxIn tid 0
          , WriteTx.TxOutInRecentEra
                (WriteTx.unsafeAddressFromBytes $ unsafeFromHex
                    "714d72cf569a339a18a7d93023139\
                    \83f56e0d96cd45bdcb1d6512dca6a")
                (toLedgerTokenBundle $ TokenBundle.fromCoin $ Coin 2_000_000)
                (WriteTx.DatumHash
                    $ fromJust
                    $ WriteTx.datumHashFromBytes
                    $ unsafeFromHex
                        "923918e403bf43c34b4ef6b48eb2ee04\
                        \babed17320d8d1b9ff9ad086e86f44ec")
                Nothing
          )
        ]
    , redeemers =
        [ RedeemerSpending (unsafeFromHex "D87A80") (TxIn (Hash tid) 0)
        ]
    }
  where
    tid = B8.replicate 32 '1'

deserializeBabbageTx :: ByteString -> Cardano.Tx Cardano.BabbageEra
deserializeBabbageTx = either (error . show) id
    . Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsBabbageEra)

-- Ideally merge with 'updateSealedTx'
addExtraTxIns
    :: [TxIn]
    -> Cardano.Tx Cardano.BabbageEra
    -> Cardano.Tx Cardano.BabbageEra
addExtraTxIns extraIns = modifyBabbageTxBody $ \body ->
    body { Babbage.inputs = Babbage.inputs body <> toLedgerInputs extraIns }
  where
    toLedgerInputs =
        Set.map (Cardano.toShelleyTxIn . toCardanoTxIn)
        . Set.fromList

withValidityInterval
    :: ValidityInterval
    -> PartialTx Cardano.BabbageEra
    -> PartialTx Cardano.BabbageEra
withValidityInterval vi ptx = ptx
    { tx = flip modifyBabbageTxBody (tx ptx) $ \ledgerBody ->
        ledgerBody
            { Babbage.txvldt = vi
            }
    }

-- Ideally merge with 'updateSealedTx'
modifyBabbageTxBody
    :: (Babbage.TxBody StandardBabbage -> Babbage.TxBody StandardBabbage)
    -> Cardano.Tx Cardano.BabbageEra -> Cardano.Tx Cardano.BabbageEra
modifyBabbageTxBody
    f
    (Cardano.Tx
        (Cardano.ShelleyTxBody
            era
            body
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits)
    = Cardano.Tx
        (Cardano.ShelleyTxBody
            era
            (f body)
            scripts
            scriptData
            auxData
            scriptValidity)
        keyWits

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

signedTxGoldens :: [ByteString]
signedTxGoldens =
    [ "84a6008182582062d3756241f3f19483e2f710e00e83c80e84329bff08753df3a6\
      \28beea3454ec18370d800182825839010c36ef7fff0869d7e75cd70f0f369bb770\
      \d66efd50625c2c1a5e84f3cd2a80021c790f232cddd9216631f285a0d745361d40\
      \2305a61abc071a000f422a82583901d6c89cba59e000ab67171115d99a2f845c38\
      \b7616838d168af37288fcd2a80021c790f232cddd9216631f285a0d745361d4023\
      \05a61abc071b000000174865a61e021a0001ffb803198fae0e81581c98947dd5fc\
      \0ec3fa16043bdbf5577fa383bb89deab60d9d9f80a214ca10082825820debdc920\
      \10207d6b51bfd2bab3d03610742d71cbf3867a7c8a7fce360c134a0e5840919835\
      \b47a543b72fae3a64cf75145cf0aa44e31cc0e089c9b2fea93d0acae9e2d69c28d\
      \4808904be4129c7a16ff3563843a8851a56701eb45947b1329bd540b8258204cff\
      \849a17fcbd9e40425e2c2ef96544333c91306e5f869e9d66fc0db91ffa0c584043\
      \c3dd8e9596ba3698633e7d6fcc20c4b0081211a1351ec192296abbb40411692fc7\
      \5504d7a50f02f2439313dc5f16aa982aab8cea6e32e0c6e64a1b82609306f5f6"

    , "84a6008182582062d3756241f3f19483e2f710e00e83c80e84329bff08753df3a6\
      \28beea3454ec18370d800182825839010c36ef7fff0869d7e75cd70f0f369bb770\
      \d66efd50625c2c1a5e84f3cd2a80021c790f232cddd9216631f285a0d745361d40\
      \2305a61abc071a000f422a82583901d6c89cba59e000ab67171115d99a2f845c38\
      \b7616838d168af37288fcd2a80021c790f232cddd9216631f285a0d745361d4023\
      \05a61abc071b000000174865a61e021a0001ffb803198fae0e81581c98947dd5fc\
      \0ec3fa16043bdbf5577fa383bb89deab60d9d9f80a214ca10082825820debdc920\
      \10207d6b51bfd2bab3d03610742d71cbf3867a7c8a7fce360c134a0e5840919835\
      \b47a543b72fae3a64cf75145cf0aa44e31cc0e089c9b2fea93d0acae9e2d69c28d\
      \4808904be4129c7a16ff3563843a8851a56701eb45947b1329bd540b8258204cff\
      \849a17fcbd9e40425e2c2ef96544333c91306e5f869e9d66fc0db91ffa0c584043\
      \c3dd8e9596ba3698633e7d6fcc20c4b0081211a1351ec192296abbb40411692fc7\
      \5504d7a50f02f2439313dc5f16aa982aab8cea6e32e0c6e64a1b82609306f5f6"

    , txWithInputsOutputsAndWits
    ]
readTestTransactions :: SpecM a [(FilePath, SealedTx)]
readTestTransactions = runIO $ do
    let dir = $(getTestData) </> "plutus"
    paths <- listDirectory dir
    files <- flip foldMap paths $ \f ->
        -- Ignore reject files
        if ".rej" `isSuffixOf` takeExtension f
        then pure []
        else do
            contents <- BS.readFile (dir </> f)
            pure [(f, contents)]
    traverse (\(f,bs) -> (f,) <$> unsafeSealedTxFromHex bs) files

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity)
    $ mkSingleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummySlottingParameters :: SlottingParameters
dummySlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21_600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2_160
    }

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters = GenesisParameters
    { getGenesisBlockHash = genesisHash
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
    }

genesisHash :: Hash "Genesis"
genesisHash = Hash (B8.replicate 32 '0')

-- | A dummy 'TimeInterpreter' for testing 'PastHorizonException's.
dummyTimeInterpreterWithHorizon
    :: SlotNo
    -> TimeInterpreter (Either PastHorizonException)
dummyTimeInterpreterWithHorizon horizon =
    let
        slotLength = 1
        t0 = HF.initBound
        t1 = HF.Bound
                (RelativeTime $ fromIntegral $ slotLength * (unSlotNo horizon))
                horizon
                (Cardano.EpochNo 1)

        era1Params = HF.defaultEraParams (SecurityParam 2) (mkSlotLength 1)
        summary = HF.summaryWithExactly $ exactlyOne
            (HF.EraSummary t0 (HF.EraEnd t1) era1Params)
        in
            hoistTimeInterpreter (runIdentity . runExceptT) $ mkTimeInterpreter
                nullTracer
                (StartTime $ posixSecondsToUTCTime 0)
                (pure $ HF.mkInterpreter summary)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | A convenient wrapper type that allows values of any type with a 'Show'
--   instance to be ordered.
--
newtype ShowOrd a = ShowOrd { unShowOrd :: a }
    deriving (Eq, Show)

instance (Eq a, Show a) => Ord (ShowOrd a) where
    compare = comparing show

shelleyBasedTxFromBytes :: ByteString -> Cardano.InAnyShelleyBasedEra Cardano.Tx
shelleyBasedTxFromBytes bytes =
    let
        anyEraTx
            = cardanoTx
            $ either (error . show) id
            $ sealedTxFromBytes bytes
    in
        case asAnyShelleyBasedEra anyEraTx of
            Just shelleyTx -> shelleyTx
            Nothing -> error "shelleyBasedTxFromBytes: ByronTx not supported"

cardanoTx :: SealedTx -> InAnyCardanoEra Cardano.Tx
cardanoTx = cardanoTxIdeallyNoLaterThan maxBound

dummyPolicyK :: KeyHash
dummyPolicyK = KeyHash Policy (BS.replicate 32 0)
