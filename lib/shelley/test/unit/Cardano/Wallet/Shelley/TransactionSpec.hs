{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.TransactionSpec
    ( spec
    , balanceTransactionSpec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, toXPub, xprvFromBytes, xprvToBytes, xpubPublicKey )
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
    , TxOutValue (TxOutAdaOnly, TxOutValue)
    , cardanoEraStyle
    )
import Cardano.Api.Gen
    ( genTx
    , genTxBodyContent
    , genTxForBalancing
    , genTxInEra
    , genTxOut
    , genWitnesses
    )
import Cardano.Api.Shelley
    ( selectLovelace )
import Cardano.BM.Data.Tracer
    ( nullTracer )
import Cardano.BM.Tracer
    ( Tracer )
import Cardano.Ledger.Shelley.API
    ( StrictMaybe (SJust, SNothing), Wdrl (..) )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic) )
import Cardano.Wallet
    ( BalanceTxNotSupportedReason (..)
    , ErrBalanceTx (..)
    , ErrSelectAssets (..)
    , ErrUpdateSealedTx (..)
    , FeeEstimation (..)
    , PartialTx (PartialTx, sealedTx)
    , WalletWorkerLog
    , balanceTransaction
    , estimateFee
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
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , deriveRewardAccount
    , getRawKey
    , hex
    , liftRawKey
    , paymentAddress
    , preparePassphrase
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
import Cardano.Wallet.Primitive.CoinSelection
    ( SelectionError (..), SelectionOf (..), selectionDelta )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionError (EmptyUTxO, SelectionLimitReached)
    , UnableToConstructChangeError (..)
    , emptySkeleton
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet (..), unsafeInitWallet )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, hoistTimeInterpreter, mkSingleEraInterpreter )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (ActiveSlotCoefficient)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (EpochLength)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , MinimumUTxOValue (..)
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
    ( genCoinPositive, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
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
    , TxConstraints (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxOut (..)
    , TxSize (..)
    , cardanoTx
    , getSealedTxWitnesses
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , serialisedTx
    , txMetadataIsNull
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxOutTokenBundle )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Shelley.Compatibility
    ( AnyShelleyBasedEra (..)
    , computeTokenBundleSerializedLengthBytes
    , fromCardanoTxIn
    , fromCardanoTxOut
    , getScriptIntegrityHash
    , getShelleyBasedEra
    , shelleyToCardanoEra
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoTxOut
    )
import Cardano.Wallet.Shelley.Transaction
    ( TxSkeleton (..)
    , TxUpdate (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor
    , estimateTxCost
    , estimateTxSize
    , mkShelleyWitness
    , mkTxSkeleton
    , mkUnsignedTx
    , newTransactionLayer
    , noTxUpdate
    , txConstraints
    , updateSealedTx
    , _decodeSealedTx
    , _estimateMaxNumberOfInputs
    , _maxScriptExecutionCost
    )
import Cardano.Wallet.Transaction
    ( TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeUpdate (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_, replicateM )
import Control.Monad.Random
    ( MonadRandom (..), Random (randomR, randomRs), random, randoms )
import Control.Monad.Trans.Except
    ( except, runExceptT )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Function
    ( on, (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List
    ( nub )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromJust, isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Semigroup
    ( Sum (Sum), getSum, mtimesDefault )
import Data.Set
    ( Set )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( Buildable (..), fmt, nameF, pretty, (+||), (||+) )
import GHC.Generics
    ( Generic )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import System.Directory
    ( listDirectory )
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
    , runIO
    , shouldBe
    , shouldSatisfy
    , xdescribe
    , xit
    )
import Test.Hspec.Core.Spec
    ( SpecM )
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
    , forAll
    , forAllShow
    , frequency
    , label
    , listOf
    , oneof
    , property
    , scale
    , shrinkList
    , suchThat
    , suchThatMap
    , vector
    , vectorOf
    , withMaxSuccess
    , within
    , (.&&.)
    , (.||.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( chooseNatural )
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
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
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

spec :: Spec
spec = do
    decodeSealedTxSpec
    estimateMaxInputsSpec
    feeCalculationSpec
    feeEstimationRegressionSpec
    forAllEras binaryCalculationsSpec
    transactionConstraintsSpec
    updateSealedTxSpec
    balanceTransactionSpec
    describe "Sign transaction" $ do
        forAllEras (\era -> it ("signTransaction adds reward account witness when necessary (" <> show era <> ")") $
            property (prop_signTransaction_addsRewardAccountKey era))
        forAllEras (\era -> it ("signTransaction adds extra key witness when necessary (" <> show era <> ")") $
            property (prop_signTransaction_addsExtraKeyWitnesses era))
        forAllEras (\era -> it ("signTransaction adds tx in witnesses when necessary (" <> show era <> ")") $
            property (prop_signTransaction_addsTxInWitnesses era))
        forAllEras (\era -> it ("signTransaction adds collateral witnesses when necessary (" <> show era <> ")") $
            property (prop_signTransaction_addsTxInCollateralWitnesses era))
        forAllEras (\era -> it ("signTransaction never removes witnesses (" <> show era <> ")") $
            property (prop_signTransaction_neverRemovesWitnesses era))
        forAllEras (\era -> it ("signTransaction never changes tx body (" <> show era <> ")") $
            property (prop_signTransaction_neverChangesTxBody era))
        forAllEras (\era -> it ("signTransaction preserves script integrity (" <> show era <> ")") $
            property (prop_signTransaction_preservesScriptIntegrity era))

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
        net
        (SL.KeyHashObj (SL.KeyHash $ hash pubkey)
             :: SL.Credential 'SL.Staking Crypto.StandardCrypto)
    where
        hash :: XPub -> Crypto.Hash Crypto.Blake2b_224 a
        hash = fromJust . Crypto.hashFromBytes . blake2b224 . xpubPublicKey

withdrawalForKey
    :: SL.Network
    -> XPub
    -> Cardano.Lovelace
    -> (Cardano.StakeAddress, Cardano.Lovelace, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxStake era))
withdrawalForKey net pubkey wdrlAmt =
    ( stakeAddressForKey net pubkey
    , wdrlAmt
    , Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr
    )

whenSupportedInEra :: forall era a . (CardanoEra era -> Maybe a) -> CardanoEra era -> (a -> Property) -> Property
whenSupportedInEra test era f =
    case test era of
        Nothing ->
            True
            & label ("feature not supported in " <> show era <> ".")
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
prop_signTransaction_addsRewardAccountKey (AnyCardanoEra era) rootXPrv utxo wdrlAmt = do
    whenSupportedInEra Cardano.withdrawalsSupportedInEra era $
        \(supported :: Cardano.WithdrawalsSupportedInEra era) -> do
        let
            rootK :: (ShelleyKey 'RootK XPrv, Passphrase "encryption")
            rootK = first liftRawKey rootXPrv

            rawRewardK :: (XPrv, Passphrase "encryption")
            rawRewardK = (getRawKey $ deriveRewardAccount (snd rootK) (fst rootK), snd rootK)

            rewardAcctPubKey :: XPub
            rewardAcctPubKey = toXPub $ fst rawRewardK

            extraWdrls = [ withdrawalForKey SL.Mainnet rewardAcctPubKey (toCardanoLovelace wdrlAmt) ]

            addWithdrawals
                :: Cardano.TxBodyContent Cardano.BuildTx era
                -> Cardano.TxBodyContent Cardano.BuildTx era
            addWithdrawals txBodyContent = txBodyContent {
                Cardano.txWithdrawals =
                        case Cardano.txWithdrawals txBodyContent of
                            Cardano.TxWithdrawalsNone ->
                                Cardano.TxWithdrawals supported extraWdrls
                            Cardano.TxWithdrawals _ wdrls ->
                                Cardano.TxWithdrawals supported $ wdrls <> extraWdrls
            }

        withBodyContent era addWithdrawals $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction tl (AnyCardanoEra era) (const Nothing) rootK utxo sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits =
                    InAnyCardanoEra era <$>
                        case Cardano.cardanoEraStyle era of
                            LegacyByronEra ->
                                error "Withdrawal witnesses are not supported in the Byron era."
                            ShelleyBasedEra _ ->
                                [mkShelleyWitness txBody rawRewardK]

            checkCoverage $
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
prop_signTransaction_addsExtraKeyWitnesses (AnyCardanoEra era) rootK utxo extraKeys = do
    whenSupportedInEra Cardano.extraKeyWitnessesSupportedInEra era $
        \(supported :: Cardano.TxExtraKeyWitnessesSupportedInEra era) -> do
        let
            keys :: (XPrv, Passphrase "encryption") -> Cardano.SigningKey Cardano.PaymentExtendedKey
            keys = Cardano.PaymentExtendedSigningKey . fst

            hashes :: [Cardano.Hash Cardano.PaymentKey]
            hashes = (Cardano.verificationKeyHash . Cardano.castVerificationKey . Cardano.getVerificationKey . keys) <$> extraKeys

            addExtraWits
                :: Cardano.TxBodyContent Cardano.BuildTx era
                -> Cardano.TxBodyContent Cardano.BuildTx era
            addExtraWits txBodyContent = txBodyContent {
                Cardano.txExtraKeyWits = Cardano.TxExtraKeyWitnesses supported hashes
            }

        withBodyContent era addExtraWits $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction tl (AnyCardanoEra era) (lookupFnFromKeys extraKeys) (first liftRawKey rootK) utxo sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits =
                    InAnyCardanoEra era <$>
                        case Cardano.cardanoEraStyle era of
                            LegacyByronEra ->
                                -- signTransaction does nothing in Byron era
                                []
                            ShelleyBasedEra _ ->
                                mkShelleyWitness txBody <$> extraKeys

            checkCoverage $
                expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

keyToAddress :: (XPrv, Passphrase "encryption") -> Address
keyToAddress (xprv, _pwd) =
    -- TODO, decrypt?
    paymentAddress @'Mainnet . publicKey . liftRawKey @ShelleyKey $ xprv

utxoFromKeys :: [(XPrv, Passphrase "encryption")] -> (UTxO -> Property) -> Property
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
        forAll (vectorOf (length txOuts) genTxIn `suchThat` isUnique) $ \txIns -> do
          let
              utxo = UTxO $ Map.fromList $ zip txIns txOuts
          utxoProp utxo

lookupFnFromKeys
    :: [(XPrv, Passphrase "encryption")]
    -> (Address -> Maybe (ShelleyKey 'AddressK XPrv, Passphrase "encryption"))
lookupFnFromKeys keys addr =
    let
        addrMap
            :: Map Address (ShelleyKey 'AddressK XPrv, Passphrase "encryption")
        addrMap = Map.fromList
            $ zip (keyToAddress <$> keys) (first liftRawKey <$> keys)
    in
       Map.lookup addr addrMap

withBodyContent
    :: IsCardanoEra era
    => CardanoEra era
    -> (Cardano.TxBodyContent Cardano.BuildTx era -> Cardano.TxBodyContent Cardano.BuildTx era)
    -> ((Cardano.TxBody era, [Cardano.KeyWitness era]) -> Property)
    -> Property
withBodyContent era modTxBody cont =
    forAllShow (genTxBodyContent era) showTransactionBody $ \txBodyContent -> do
        let
            txBodyContent' = modTxBody txBodyContent
            txBody = unsafeMakeTransactionBody txBodyContent'

        forAll (genWitnesses era txBody) $ \wits -> cont (txBody, wits)

checkSubsetOf
    :: ( Eq a
       , Show a
       )
    => [a]
    -> [a]
    -> Property
checkSubsetOf elems xs = do
    counterexample ("actual set: " <> show xs) $ conjoin
        [ x `elem` xs & counterexample ("expected elem: " <> show x)
        | x <- elems ]

prop_signTransaction_addsTxInWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInWitnesses (AnyCardanoEra era) rootK extraKeysNE = do
    let
        extraKeys = NE.toList extraKeysNE

    utxoFromKeys extraKeys $ \utxo -> do
        let
            txIns :: [TxIn]
            txIns = Map.keys $ unUTxO utxo

            addTxIns
                :: Cardano.TxBodyContent Cardano.BuildTx era
                -> Cardano.TxBodyContent Cardano.BuildTx era
            addTxIns txBodyContent = txBodyContent {
                Cardano.txIns = (, Cardano.BuildTxWith (Cardano.KeyWitness Cardano.KeyWitnessForSpending)) . toCardanoTxIn <$> txIns
            }

        withBodyContent era addTxIns $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction tl (AnyCardanoEra era) (lookupFnFromKeys extraKeys) (first liftRawKey rootK) utxo sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits =
                    InAnyCardanoEra era <$>
                        case Cardano.cardanoEraStyle era of
                            LegacyByronEra ->
                                -- signTransaction does nothing in Byron era
                                []
                            ShelleyBasedEra _ ->
                                mkShelleyWitness txBody <$> extraKeys

            checkCoverage $
                expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

prop_signTransaction_addsTxInCollateralWitnesses
    :: AnyCardanoEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInCollateralWitnesses (AnyCardanoEra era) rootK extraKeysNE = do
    whenSupportedInEra Cardano.collateralSupportedInEra era $
        \(supported :: Cardano.CollateralSupportedInEra era) -> do
        let
            extraKeys = NE.toList extraKeysNE

        utxoFromKeys extraKeys $ \utxo -> do
            let
                txIns :: [TxIn]
                txIns = Map.keys $ unUTxO utxo

                addTxCollateralIns
                    :: Cardano.TxBodyContent Cardano.BuildTx era
                    -> Cardano.TxBodyContent Cardano.BuildTx era
                addTxCollateralIns txBodyContent = txBodyContent {
                    Cardano.txInsCollateral = Cardano.TxInsCollateral supported (toCardanoTxIn <$> txIns)
                }

            withBodyContent era addTxCollateralIns $ \(txBody, wits) -> do
                let
                    tl = testTxLayer

                    sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                    sealedTx' = signTransaction tl (AnyCardanoEra era) (lookupFnFromKeys extraKeys) (first liftRawKey rootK) utxo sealedTx

                    expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                    expectedWits =
                        InAnyCardanoEra era <$>
                            case Cardano.cardanoEraStyle era of
                                LegacyByronEra ->
                                    -- signTransaction does nothing in Byron era
                                    []
                                ShelleyBasedEra _ ->
                                    mkShelleyWitness txBody <$> extraKeys

                checkCoverage $
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
prop_signTransaction_neverRemovesWitnesses (AnyCardanoEra era) rootK utxo extraKeys =
    forAll (genTxInEra era) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction tl (AnyCardanoEra era) (lookupFnFromKeys extraKeys) (first liftRawKey rootK) utxo sealedTx

            witnessesBefore = getSealedTxWitnesses sealedTx
            witnessesAfter = getSealedTxWitnesses sealedTx'

        checkCoverage
            $ cover 30 (not $ null witnessesBefore) "witnesses non-empty before"
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
prop_signTransaction_neverChangesTxBody (AnyCardanoEra era) rootK utxo extraKeys =
    forAll (genTxInEra era) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction tl (AnyCardanoEra era) (lookupFnFromKeys extraKeys) (first liftRawKey rootK) utxo sealedTx

            txBodyContent :: InAnyCardanoEra Cardano.Tx -> InAnyCardanoEra (Cardano.TxBodyContent Cardano.ViewTx)
            txBodyContent (InAnyCardanoEra e (Cardano.Tx (Cardano.TxBody bodyContent) _wits)) =
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
    whenSupportedInEra Cardano.scriptDataSupportedInEra era $ \_supported -> do
        forAll (genTxInEra era) $ \tx -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' tx
                sealedTx' = signTransaction tl (AnyCardanoEra era) (const Nothing) (first liftRawKey rootK) utxo sealedTx

                getScriptIntegrityHashInAnyCardanoEra :: InAnyCardanoEra Cardano.Tx -> Maybe ByteString
                getScriptIntegrityHashInAnyCardanoEra (InAnyCardanoEra _ transaction) = getScriptIntegrityHash transaction

                scriptIntegrityHashBefore = getScriptIntegrityHashInAnyCardanoEra $ cardanoTx sealedTx
                scriptIntegrityHashAfter = getScriptIntegrityHashInAnyCardanoEra $ cardanoTx sealedTx'

            checkCoverage
                $ cover 30 (isJust scriptIntegrityHashBefore) "script integrity hash exists"
                $ conjoin
                    [ scriptIntegrityHashBefore == scriptIntegrityHashAfter
                      & counterexample ("script integrity hash before: " <> show scriptIntegrityHashBefore)
                      & counterexample ("script integrity hash after: " <> show scriptIntegrityHashAfter)
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
    it "tx with withdrawal" $ do
        let bytes = unsafeFromHex "84a70081825820410a9cd4af08b3abe25c2d3b87af4c23d0bb2fb7577b639d5cfbdfe13a4a696c0c0d80018182583901059f0c7b9899793d2c9afaeff4fd09bedd9df3b8cb1b9c301ab8e0f7fb3c13a29d3798f1b77b47f2ddb31c19326b87ed6f71fb9a27133ad51b000001001d19d714021a000220ec03198d0f05a1581de1fb3c13a29d3798f1b77b47f2ddb31c19326b87ed6f71fb9a27133ad51b000000e8d4a510000e80a0f5f6"
        let sealedTx = sealedTxFromBytes bytes
        sealedTx `shouldSatisfy` isRight

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
        [(1,115),(5,109),(10,104),(20,93),(50,54)]
    estimateMaxInputsTests @ByronKey
        [(1,73),(5,69),(10,65),(20,57),(50,29)]
    estimateMaxInputsTests @IcarusKey
        [(1,73),(5,69),(10,65),(20,57),(50,29)]

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
                , txParameters = dummyTxParameters
                    { getMaxExecutionUnits = ExecutionUnits 10000000 10000000000
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
    minFee ctx = Coin.toInteger $ calcMinimumCost testTxLayer pp ctx sel
      where sel = emptySkeleton

    minFeeSkeleton :: TxSkeleton -> Integer
    minFeeSkeleton = Coin.toInteger . estimateTxCost pp

    estimateTxSize' :: TxSkeleton -> Integer
    estimateTxSize' = fromIntegral . unTxSize . estimateTxSize

    (dummyAcct, dummyPath) =
        (RewardAccount mempty, DerivationIndex 0 :| [])

feeEstimationRegressionSpec :: Spec
feeEstimationRegressionSpec = describe "Regression tests" $ do
    it "#1740 Fee estimation at the boundaries" $ do
        let requiredCost = 166029
        let runSelection = except $ Left
                $ ErrSelectAssetsSelectionError
                $ SelectionBalanceError
                $ Balance.UnableToConstructChange
                $ Balance.UnableToConstructChangeError
                    { requiredCost = Coin.fromWord64 requiredCost
                    , shortfall = Coin 100000
                    }
        result <- runExceptT (estimateFee runSelection)
        result `shouldBe`
            Right (FeeEstimation requiredCost requiredCost)

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
    , minimumUTxOvalue =
        error "dummyProtocolParameters: minimumUTxOvalue"
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
mockFeePolicy = LinearFee (Quantity 155381) (Quantity 44)

mockProtocolParameters :: ProtocolParameters
mockProtocolParameters = dummyProtocolParameters
    { executionUnitPrices = Just (ExecutionUnitPrices 1 1)
    , txParameters = TxParameters
        { getFeePolicy = mockFeePolicy
        , getTxMaxSize = Quantity 16384
        , getTokenBundleMaxSize = TokenBundleMaxSize $ TxSize 4000
        , getMaxExecutionUnits = ExecutionUnits 10000000 10000000000
        }
    , minimumUTxOvalue = MinimumUTxOValue $ Coin 1000000
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
    upperBound = lowerBound <> txInputCount `mtimesDefault` Coin (4*44)

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
    arbitrary = fmap Large . genTxOutTokenBundle =<< choose (1, 128)

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

data Ctx = Ctx (Tracer Gen WalletWorkerLog) (TransactionLayer ShelleyKey SealedTx)
    deriving (Generic)

balanceTransactionSpec :: Spec
balanceTransactionSpec = do
    describe "balanceTransaction" $
        -- TODO: Create a test to show that datums are passed through...

        -- TODO: Fix balancing issues which are presumably due to
        -- variable-length coin encoding boundary cases.
        xit "produces balanced transactions or fails"
            $ property prop_balanceTransactionBalanced

-- https://mail.haskell.org/pipermail/haskell-cafe/2016-August/124742.html
mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)

data Wallet' = Wallet' UTxOIndex (Wallet (SeqState 'Mainnet ShelleyKey)) (Set Tx)

instance Show Wallet' where
    show (Wallet' u w pending) = fmt $ mconcat
        [ nameF "Wallet" (pretty w)
        , nameF "UTxOIndex" (""+||u||+"")
        , nameF "pending" (""+||pending||+"")
        ]

instance Arbitrary Wallet' where
    arbitrary = do
        utxo <- genUTxO
        mw <- SomeMnemonic <$> genMnemonic @12
        let s = mkSeqStateFromRootXPrv (rootK mw) purposeCIP1852 defaultAddressPoolGap

        return $ Wallet'
            (UTxOIndex.fromUTxO utxo)
            (unsafeInitWallet utxo (header block0) s)
            mempty
      where
        genUTxO =
            UTxO . Map.fromList <$> listOf genEntry
          where
            genEntry = (,) <$> genIn <*> genOut
              where
                genIn = fromCardanoTxIn <$> Cardano.genTxIn
                genOut = fromCardanoTxOut <$> genTxOut AlonzoEra

        rootK :: SomeMnemonic -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
        rootK mw =
            let
                pwd = mempty
            in
                (generateKeyFromSeed (mw, Nothing) pwd, pwd)
    shrink w = [setUTxO u' w
               | u' <- shrinkUTxO' (getUTxO w)
               ]

      where
        setUTxO :: UTxO -> Wallet' -> Wallet'
        setUTxO u (Wallet' _ wal pending) =
            Wallet'
                (UTxOIndex.fromUTxO u)
                (wal { utxo = u})
                pending

        getUTxO (Wallet' u _ _) = UTxOIndex.toUTxO u

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

instance Arbitrary PartialTx where
    arbitrary = do
        let era = AlonzoEra
        tx <- genTxForBalancing era
        let (Cardano.Tx (Cardano.TxBody content) _) = tx
        let inputs = Cardano.txIns content
        resolvedInputs <- forM inputs $ \i -> do
            -- NOTE: genTxOut does not generate quantities larger than
            -- `maxBound :: Word64`, however users could supply these.
            -- We should ideally test what happens, and make it clear what code,
            -- if any, should validate.
            o <- fromCardanoTxOut <$> genTxOut Cardano.AlonzoEra
            return (fromCardanoTxIn $ fst i, o, Nothing)
        let redeemers = []
        return $ PartialTx
            (sealedTxFromCardano $ InAnyCardanoEra era tx)
            resolvedInputs
            redeemers
    shrink (PartialTx tx inputs redeemers) =
        [ PartialTx tx inputs' redeemers
        | inputs' <- shrinkInputs inputs
        ] ++
        [ restrictResolution $ PartialTx
            (sealedTxFromCardano $ InAnyCardanoEra Cardano.AlonzoEra tx')
            inputs
            redeemers
        | tx' <- shrinkTx (alonzoCardanoTx tx)
        ]
      where
        alonzoCardanoTx :: SealedTx -> Cardano.Tx Cardano.AlonzoEra
        alonzoCardanoTx (cardanoTx -> InAnyCardanoEra Cardano.AlonzoEra atx) =
            atx
        alonzoCardanoTx _ = error "alonzoCardanoTx: todo handle other eras"

        shrinkInputs (i:ins) = map (:ins) (shrink i) ++ map (i:) (shrinkInputs ins)
        shrinkInputs [] = []

resolvedInputsUTxO
    :: ShelleyBasedEra era
    -> PartialTx
    -> Cardano.UTxO era
resolvedInputsUTxO era (PartialTx _ resolvedInputs _) =
    Cardano.UTxO $ Map.fromList $ map convertUTxO resolvedInputs
  where
    convertUTxO (i, o, Nothing) =
        (toCardanoTxIn i, toCardanoTxOut era o)
    convertUTxO (_, _, Just _) =
        error "resolvedInputsUTxO: todo: handle datum hash"

toCardanoUTxO :: UTxO -> Cardano.UTxO Cardano.AlonzoEra
toCardanoUTxO utxo = Cardano.UTxO $ Map.fromList $ map convertUTxO $ UTxO.toList utxo
  where
    convertUTxO (i, o) = (toCardanoTxIn i, toCardanoTxOut Cardano.ShelleyBasedEraAlonzo o)

instance Semigroup (Cardano.UTxO era) where
    Cardano.UTxO a <> Cardano.UTxO b = Cardano.UTxO (a <> b)

instance Monoid (Cardano.UTxO era) where
    mempty = Cardano.UTxO mempty

shrinkTx
    :: Cardano.Tx Cardano.AlonzoEra -> [Cardano.Tx Cardano.AlonzoEra]
shrinkTx (Cardano.Tx bod wits) =
    [ Cardano.Tx bod' wits
    | bod' <- shrinkTxBody bod
    ]

-- | Restricts the inputs list of the 'PartialTx' to the inputs of the
-- underlying CBOR transaction. This allows us to "fix" the 'PartialTx' after
-- shrinking the CBOR.
--
-- NOTE: Perhaps ideally 'PartialTx' would handle this automatically.
restrictResolution :: PartialTx -> PartialTx
restrictResolution (PartialTx tx inputs redeemers) =
    let
        inputs' = flip filter inputs $  \(i, _, _) ->
            i `Set.member` inputsInTx (cardanoTx tx)
    in
        PartialTx tx inputs' redeemers
  where
    inputsInTx (InAnyCardanoEra _era (Cardano.Tx (Cardano.TxBody bod) _)) =
        Set.fromList $ map (fromCardanoTxIn . fst) $ Cardano.txIns bod

shrinkTxBody :: Cardano.TxBody Cardano.AlonzoEra -> [Cardano.TxBody Cardano.AlonzoEra]
shrinkTxBody (Cardano.ShelleyTxBody e bod scripts scriptData aux val) = tail
    [ Cardano.ShelleyTxBody e bod' scripts' scriptData' aux' val'
    | bod' <- prependOriginal shrinkLedgerTxBody bod
    , aux' <- aux : filter (/= aux) [Nothing]
    , scriptData' <- prependOriginal shrinkScriptData scriptData
    , scripts' <- prependOriginal shrinkScripts scripts
    , val' <- val : filter (/= val)
        [ Cardano.TxScriptValidity Cardano.TxScriptValiditySupportedInAlonzoEra
            Cardano.ScriptValid
        ]
    ]
  where
    -- | For writing shrinkers in the style of https://stackoverflow.com/a/14006575
    prependOriginal shrinker = \x -> x : shrinker x

    shrinkScripts = shrinkList (const [])

    shrinkScriptData Cardano.TxBodyNoScriptData = []
    shrinkScriptData (Cardano.TxBodyScriptData era (Alonzo.TxDats dats) (Alonzo.Redeemers redeemers))
        = tail
          [ Cardano.TxBodyScriptData era (Alonzo.TxDats dats') (Alonzo.Redeemers redeemers')
          | dats' <- dats : (Map.fromList <$> shrinkList (const []) (Map.toList dats))
          , redeemers' <- redeemers : (Map.fromList <$> shrinkList (const []) (Map.toList redeemers))
          ]

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
        | updates' <- prependOriginal shrinkUpdates (Alonzo.txUpdates body)
        , wdrls' <- prependOriginal shrinkWdrl (Alonzo.txwdrls body)
        , outs' <- prependOriginal (shrinkSeq (const [])) (Alonzo.outputs body)
        , ins' <- prependOriginal (shrinkSet (const [])) (Alonzo.inputs body)
        , certs' <- prependOriginal  (shrinkSeq (const [])) (Alonzo.txcerts body)
        , mint' <- prependOriginal shrinkValue (Alonzo.mint body)
        , rsh' <- prependOriginal
            (shrinkSet (const []))
            (Alonzo.reqSignerHashes body)
        ]

    shrinkValue v = filter (/= v) [v0]
      where
        v0 = mempty

    shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
    shrinkSet shrinkElem = map Set.fromList . shrinkList shrinkElem . F.toList

    shrinkSeq shrinkElem = map StrictSeq.fromList . shrinkList shrinkElem . F.toList

    shrinkWdrl :: Wdrl era -> [Wdrl era]
    shrinkWdrl (Wdrl m) = map (Wdrl . Map.fromList) $ shrinkList shrinkWdrl' (Map.toList m)
      where
        shrinkWdrl' (acc, Ledger.Coin c) =
            [(acc, Ledger.Coin c')
            | c' <- filter (>= 1) $ shrink c
            ]

    shrinkUpdates SNothing = []
    shrinkUpdates (SJust _) = [SNothing]

-- TODO: I believe evaluateTransactionFee relies on estimating the number of
-- witnesses required to determine the balance. We should also have a similar
-- test which also signs.
--
-- TODO: Ensure scripts are well tested
--   - Ensure we have coverage for normal plutus contracts
--
-- TODO: Generate data for other eras than Alonzo
prop_balanceTransactionBalanced
    :: Wallet'
    -> ShowBuildable PartialTx
    -> Property
prop_balanceTransactionBalanced (Wallet' utxo wal pending) (ShowBuildable partialTx)
    = withMaxSuccess 200 $ do
        let combinedUTxO = mconcat
                [ resolvedInputsUTxO Cardano.ShelleyBasedEraAlonzo partialTx
                , toCardanoUTxO (view #utxo wal)
                ]
        let originalBalance = txBalance (sealedTx partialTx) combinedUTxO
        forAllShow (runExceptT $ balanceTransaction
                (Ctx nullTracer tl)
                (delegationAddress @'Mainnet)
                pparams
                dummyTimeInterpreter
                (utxo, wal, pending)
                partialTx) (show . Pretty) $ \case
            Right (sealedTx ) -> do
                label "success"
                    $ classify (originalBalance == Cardano.Lovelace 0)
                        "already balanced"
                    $ classify (txFee sealedTx > Cardano.Lovelace 1_000_000)
                        "fee above 1 ada"
                    $ classify (txFee sealedTx > Cardano.Lovelace 1_000_000)
                        "fee above 1 ada"
                    $ classify (hasCollateral sealedTx)
                        "balanced tx has collateral"
                    (txBalance sealedTx combinedUTxO === 0)
                    .&&. (abs (txFee sealedTx) .<= 4_000_000)
                    -- Fee limit chosen at a hunch for the sake of sanity. As
                    -- long as the property uses mainnet PParams, this is
                    -- useful.
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceError (Balance.BalanceInsufficient err)))) -> do
                let missing = Balance.balanceMissing err
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
                (SelectionBalanceError
                (Balance.InsufficientMinCoinValues _)))) ->
                label "outputs below minCoinValue" $ property True
            Left (ErrBalanceTxNotYetSupported Deposits) ->
                label ("not yet supported: deposits") True
            Left (ErrBalanceTxExistingCollateral) ->
                label "existing collateral" True
            Left (ErrBalanceTxNotYetSupported (UnderestimatedFee _)) ->
                label "underestimated fee" $ property True
            Left (ErrBalanceTxNotYetSupported ZeroAdaOutput) ->
                label "not yet supported: zero ada output" $ property True
            Left (ErrBalanceTxNotYetSupported ConflictingNetworks) ->
                label "not yet supported: conflicting networks" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceError EmptyUTxO))) ->
                label "empty UTxO" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceError
                (SelectionLimitReached _)))) ->
                label "selection limit reached" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceError (Balance.UnableToConstructChange _)))) ->
                label "unable to construct change" $ property True
            Left err -> label "other error" $
                counterexample ("balanceTransaction failed: " <> show err) False
  where
    a .<= b = counterexample (show a <> " /<= " <> show b) $ property $ a <= b
    tl = testTxLayer

    hasCollateral :: SealedTx -> Bool
    hasCollateral tx = withAlonzoBod tx $ \(Cardano.TxBody content) ->
        case Cardano.txInsCollateral content of
            Cardano.TxInsCollateralNone -> False
            Cardano.TxInsCollateral _ [] -> False
            Cardano.TxInsCollateral _ (_:_) -> True

    txFee :: SealedTx -> Cardano.Lovelace
    txFee tx = withAlonzoBod tx $ \(Cardano.TxBody content) ->
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

    txBalance :: SealedTx -> Cardano.UTxO Cardano.AlonzoEra -> Cardano.Lovelace
    txBalance tx u = withAlonzoBod tx $ \bod ->
        lovelaceFromCardanoTxOutValue
        $ Cardano.evaluateTransactionBalance nodePParams mempty u bod

    lovelaceFromCardanoTxOutValue
        :: forall era. Cardano.TxOutValue era -> Cardano.Lovelace
    lovelaceFromCardanoTxOutValue (TxOutAdaOnly _ coin) = coin
    lovelaceFromCardanoTxOutValue (TxOutValue _ val) = selectLovelace val

    withAlonzoBod
        :: SealedTx
        -> (Cardano.TxBody Cardano.AlonzoEra -> a)
        -> a
    withAlonzoBod (cardanoTx -> Cardano.InAnyCardanoEra Cardano.AlonzoEra tx) f =
        let Cardano.Tx bod _ = tx
        in f bod
    withAlonzoBod _ _ = error "withBod: other eras are not handled yet"

    -- NOTE: We don't have a 'Cardano.ProtocolParameters -> ProtocolParameters'
    -- function. For the time being, we simply hard-code the nodePParms here.

    pparams = (mockProtocolParameters, nodePParams)
    nodePParams = Cardano.ProtocolParameters
        { Cardano.protocolParamTxFeeFixed = 155381
        , Cardano.protocolParamTxFeePerByte = 44
        , Cardano.protocolParamMaxTxSize = 16384
        , Cardano.protocolParamMinUTxOValue = Nothing
        , Cardano.protocolParamMaxTxExUnits =
            Just $ Cardano.ExecutionUnits 10000000 10000000000
        , Cardano.protocolParamMaxValueSize = Just 4000
        , Cardano.protocolParamProtocolVersion = (6, 0)
        , Cardano.protocolParamDecentralization = 0
        , Cardano.protocolParamExtraPraosEntropy = Nothing
        , Cardano.protocolParamMaxBlockHeaderSize = 100000 -- Dummy value
        , Cardano.protocolParamMaxBlockBodySize = 100000
        , Cardano.protocolParamStakeAddressDeposit = Cardano.Lovelace 2_000_000
        , Cardano.protocolParamStakePoolDeposit = Cardano.Lovelace 500_000_000
        , Cardano.protocolParamMinPoolCost = Cardano.Lovelace 32_000_000
        , Cardano.protocolParamPoolRetireMaxEpoch = Cardano.EpochNo 2
        , Cardano.protocolParamStakePoolTargetNum = 100
        , Cardano.protocolParamPoolPledgeInfluence = 0
        , Cardano.protocolParamMonetaryExpansion = 0
        , Cardano.protocolParamTreasuryCut  = 0
        , Cardano.protocolParamUTxOCostPerWord = Just 34482
        , Cardano.protocolParamCostModels = Map.empty -- TODO
        , Cardano.protocolParamPrices =
            Just $ Cardano.ExecutionUnitPrices 1 1
        , Cardano.protocolParamMaxBlockExUnits =
            Just $ Cardano.ExecutionUnits 10000000 10000000000
        , Cardano.protocolParamCollateralPercent = Just 1
        , Cardano.protocolParamMaxCollateralInputs = Just 3
        }


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
            forM_ txs $ \(filepath, tx) -> do
                it ("without TxUpdate: " <> filepath) $ do
                    case updateSealedTx tx noTxUpdate of
                        Left e ->
                            expectationFailure $ "expected update to succeed but failed: " <> show e
                        Right tx' -> do
                            sealedInputs tx `shouldBe` sealedInputs tx'
                            sealedOutputs tx `shouldBe` sealedOutputs tx'
                            sealedFee tx `shouldBe` sealedFee tx'
                            sealedCollateral tx `shouldBe` sealedCollateral tx'

                prop ("with TxUpdate: " <> filepath) $
                    prop_updateSealedTx tx

        describe "existing key witnesses" $ do
            it "returns `Left err` with noTxUpdate" $ do
                -- Could be argued that it should instead return `Right tx`.
                case sealedTxFromBytes $ unsafeFromHex txWithInputsOutputsAndWits of
                    Left e -> expectationFailure $ show e
                    Right tx -> do
                        updateSealedTx tx noTxUpdate
                            `shouldBe` Left (ErrExistingKeyWitnesses 2)

            it "returns `Left err` when extra body content is non-empty" $ do
                pendingWith "todo: add test data"

unsafeSealedTxFromHex :: ByteString -> IO SealedTx
unsafeSealedTxFromHex =
    either (fail . show) pure . sealedTxFromBytes . unsafeFromHex . BS.dropWhileEnd isNewlineChar
  where
    isNewlineChar c = c `elem` [10,13]

prop_updateSealedTx :: SealedTx -> [(TxIn, TxOut)] -> [TxIn] -> [TxOut] -> Coin -> Property
prop_updateSealedTx tx extraIns extraCol extraOuts newFee = do
    let extra = TxUpdate extraIns extraCol extraOuts (UseNewTxFee newFee)
    let tx' = either (error . show) id
            $ updateSealedTx tx extra
    conjoin
        [ sealedInputs tx' === sealedInputs tx <> Set.fromList (fst <$> extraIns)
        , sealedOutputs tx' === sealedOutputs tx <> Set.fromList extraOuts
        , sealedFee tx' === Just newFee
        , sealedCollateral tx' ===
            if isAlonzo tx
            then sealedCollateral tx <> Set.fromList extraCol
            else mempty
        ]
  where
    isAlonzo (cardanoTx -> InAnyCardanoEra Cardano.AlonzoEra _) = True
    isAlonzo (cardanoTx -> InAnyCardanoEra _ _) = False

fst4 :: (a, b, c, d) -> a
fst4 (a,_,_,_) = a

sealedInputs :: SealedTx -> Set TxIn
sealedInputs =
    Set.fromList . map fst . view #resolvedInputs . fst4 . _decodeSealedTx

sealedCollateral :: SealedTx -> Set TxIn
sealedCollateral =
    Set.fromList . map fst . view #resolvedCollateral . fst4 . _decodeSealedTx

sealedOutputs :: SealedTx -> Set TxOut
sealedOutputs =
    Set.fromList . view #outputs . fst4 . _decodeSealedTx

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

sealedFee :: SealedTx -> Maybe Coin
sealedFee =
    view #fee . fst4 . _decodeSealedTx

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

readTestTransactions :: SpecM a [(FilePath, SealedTx)]
readTestTransactions = runIO $ do
    let dir = $(getTestData) </> "plutus"
    listDirectory dir
        >>= traverse (\f -> (f,) <$> BS.readFile (dir </> f))
        >>= traverse (\(f,bs) -> (f,) <$> unsafeSealedTxFromHex bs)

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity)
    $ mkSingleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummySlottingParameters :: SlottingParameters
dummySlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2160
    }

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters = GenesisParameters
    { getGenesisBlockHash = genesisHash
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
    }

genesisHash :: Hash "Genesis"
genesisHash = Hash (B8.replicate 32 '0')
