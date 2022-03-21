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
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval (ValidityInterval) )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic), entropyToMnemonic, mkEntropy )
import Cardano.Wallet
    ( BalanceTxNotSupportedReason (..)
    , ErrBalanceTx (..)
    , ErrSelectAssets (..)
    , ErrUpdateSealedTx (..)
    , FeeEstimation (..)
    , PartialTx (..)
    , WalletWorkerLog
    , balanceTransaction
    , estimateFee
    , signTransaction
    )
import Cardano.Wallet.Byron.Compatibility
    ( maryTokenBundleMaxSize )
import Cardano.Wallet.CoinSelection
    ( SelectionBalanceError (..)
    , SelectionError (..)
    , SelectionOf (..)
    , UnableToConstructChangeError (..)
    , WalletUTxO (..)
    , balanceMissing
    , emptySkeleton
    , selectionDelta
    )
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
    , PoolId (PoolId)
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
    , sealedTxFromCardanoBody
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
    , toCardanoUTxO
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toAlonzoTxOut )
import Cardano.Wallet.Shelley.Transaction
    ( TxSkeleton (..)
    , TxUpdate (..)
    , TxWitnessTag (..)
    , TxWitnessTagFor
    , estimateTxCost
    , estimateTxSize
    , mkDelegationCertificates
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
    ( DelegationAction (RegisterKeyAndJoin)
    , ErrAssignRedeemers (..)
    , TransactionCtx (..)
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
    ( MonadRandom (..)
    , Rand
    , Random (randomR, randomRs)
    , StdGen
    , evalRand
    , random
    , randoms
    )
import Control.Monad.Random.Extra
    ( StdGenSeed (..), stdGenFromSeed )
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
    ( fromJust, fromMaybe, isJust )
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
import Test.Hspec.Golden
    ( Golden (..) )
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
import qualified Cardano.Wallet.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
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
import qualified Data.Text.IO as T
import qualified Test.Hspec.Extra as Hspec

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
    estimateSignedTxSizeSpec
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
                $ SelectionBalanceErrorOf
                $ UnableToConstructChange
                $ UnableToConstructChangeError
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
          Right unsigned =
              mkUnsignedTx era slotNo cs md mempty [] fee
              TokenMap.empty TokenMap.empty Map.empty
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
    Right unsigned =
        mkUnsignedTx era slotNo cs md mempty [] fee
        TokenMap.empty TokenMap.empty Map.empty
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
    Right unsigned =
        mkUnsignedTx era slotNo cs Nothing mempty [] fee
        TokenMap.empty TokenMap.empty Map.empty
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
    { executionUnitPrices = Just $ ExecutionUnitPrices
        (721 % 10_000_000)
        (577 %     10_000)
    , txParameters = TxParameters
        { getFeePolicy = mockFeePolicy
        , getTxMaxSize = Quantity 16384
        , getTokenBundleMaxSize = TokenBundleMaxSize $ TxSize 4000
        , getMaxExecutionUnits = ExecutionUnits 10_000_000_000 14_000_000
        }
    , minimumUTxOvalue = MinimumUTxOValue $ Coin 1000000
    , maximumCollateralInputCount = 3
    , minimumCollateralPercentage = 150
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

data Ctx m = Ctx (Tracer m WalletWorkerLog) (TransactionLayer ShelleyKey SealedTx)
    deriving Generic

instance Arbitrary StdGenSeed  where
  arbitrary = StdGenSeed . fromIntegral @Int <$> arbitrary

balanceTransactionSpec :: Spec
balanceTransactionSpec = do
    describe "balanceTransaction" $ do
        -- TODO: Create a test to show that datums are passed through...

        -- TODO: Fix balancing issues which are presumably due to
        -- variable-length coin encoding boundary cases.
        xit "produces balanced transactions or fails"
            $ property prop_balanceTransactionBalanced

        balanceTransactionGoldenSpec

        describe "when passed unresolved inputs" $ do
            it "may fail"
                $ property prop_balanceTransactionUnresolvedInputs

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
                genIn = fromCardanoTxIn <$> Cardano.genTxIn
                genOut = fromCardanoTxOut <$> genTxOut AlonzoEra

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

balanceTransaction'
    :: (UTxOIndex WalletUTxO, Wallet (SeqState 'Mainnet ShelleyKey), Set Tx)
    -> StdGenSeed
    -> PartialTx
    -> Either ErrBalanceTx SealedTx
balanceTransaction' wal seed tx  =
    flip evalRand (stdGenFromSeed seed) $ runExceptT $
        balanceTransaction @(Rand StdGen)
            (Ctx @(Rand StdGen) nullTracer testTxLayer)
            (delegationAddress @'Mainnet)
            mockProtocolParametersForBalancing
            dummyTimeInterpreter
            wal
            tx

-- | Tests that 'ErrAssignRedeemersUnresolvedTxIns' can in fact be returned by
-- 'balanceTransaction'.
prop_balanceTransactionUnresolvedInputs
    :: Wallet'
    -> ShowBuildable PartialTx
    -> StdGenSeed
    -> Property
prop_balanceTransactionUnresolvedInputs
    (Wallet' utxo wal pending)
    (ShowBuildable partialTx')
    seed = checkCoverage $ withMaxSuccess 400 $
        forAll (dropResolvedInputs partialTx') $ \(partialTx, dropped) -> do
        not (null dropped) ==> do
            let res = balanceTransaction' (utxo, wal, pending) seed partialTx
            cover 1 (isUnresolvedTxInsErr res) "unknown txins" $
                case res of
                    Right _
                        -> label "success" $ property True
                           -- Balancing can succeed if the dropped inputs happen
                           -- to be apart of the wallet UTxO.
                    Left (ErrBalanceTxAssignRedeemers
                        (ErrAssignRedeemersUnresolvedTxIns _))
                        -> property True
                    Left _
                        -> property True
  where
    isUnresolvedTxInsErr
        (Left (ErrBalanceTxAssignRedeemers
            (ErrAssignRedeemersUnresolvedTxIns _))) = True
    isUnresolvedTxInsErr _ = False

    dropResolvedInputs (PartialTx tx inputs redeemers) = do
        shouldKeep <- vectorOf (length inputs) $ frequency
            [ (8, pure False)
            , (2, pure True)
            ]
        let inputs' = map snd $ filter        fst  $ zip shouldKeep inputs
        let dropped = map snd $ filter (not . fst) $ zip shouldKeep inputs
        pure (PartialTx tx inputs' redeemers, dropped)

data BalanceTxGolden =
    BalanceTxGoldenSuccess
        Coin -- ^ Wallet balance
        Cardano.Lovelace -- ^ Fee
        Cardano.Lovelace -- ^ Minimum fee
    | BalanceTxGoldenFailure Coin String

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
    test :: String -> PartialTx -> Spec
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

        mkGolden :: PartialTx -> Coin -> BalanceTxGolden
        mkGolden ptx c =
            let
                Wallet' utxoIndex wal pending = mkTestWallet rootK (utxo [c])
                res = balanceTransaction'
                    (utxoIndex, wal, pending)
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

    payment :: PartialTx
    payment = PartialTx (sealedTxFromCardanoBody body) [] []
      where
        body = Cardano.ShelleyTxBody
            Cardano.ShelleyBasedEraAlonzo
            alonzoBody
            []
            Cardano.TxBodyNoScriptData
            Nothing
            Cardano.TxScriptValidityNone

        alonzoBody = Alonzo.TxBody
          { Alonzo.inputs = mempty
          , Alonzo.collateral = mempty
          , Alonzo.outputs = StrictSeq.singleton $ toAlonzoTxOut
            (TxOut addr (TokenBundle.fromCoin (Coin 1_000_000)))
            Nothing
          , Alonzo.txcerts = mempty
          , Alonzo.txwdrls = Wdrl mempty
          , Alonzo.txfee = mempty
          , Alonzo.txvldt = ValidityInterval SNothing SNothing
          , Alonzo.txUpdates = SNothing
          , Alonzo.reqSignerHashes = mempty
          , Alonzo.mint = mempty
          , Alonzo.scriptIntegrityHash = SNothing
          , Alonzo.adHash = SNothing
          , Alonzo.txnetworkid = SNothing
          }
        addr = Address $ unsafeFromHex
            "60b1e5e0fb74c86c801f646841e07cdb42df8b82ef3ce4e57cb5412e77"

    delegate :: PartialTx
    delegate = PartialTx (sealedTxFromCardanoBody body) [] []
      where
        body = Cardano.ShelleyTxBody
            Cardano.ShelleyBasedEraAlonzo
            alonzoBody
            []
            Cardano.TxBodyNoScriptData
            Nothing
            Cardano.TxScriptValidityNone

        certs = mkDelegationCertificates (RegisterKeyAndJoin poolId) xpub
          where
            poolId = PoolId "\236(\243=\203\230\214@\n\RS^3\155\208d|\
                            \\ts\202l\f\249\194\187\230\131\141\198"
            xpub = getRawKey $ publicKey rootK
            mw = SomeMnemonic $ either (error . show) id
                (entropyToMnemonic @12 <$> mkEntropy "0000000000000001")
            rootK = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
        alonzoBody = Alonzo.TxBody
          { Alonzo.inputs = mempty
          , Alonzo.collateral = mempty
          , Alonzo.outputs = mempty
          , Alonzo.txcerts
            = StrictSeq.fromList $ map Cardano.toShelleyCertificate certs
          , Alonzo.txwdrls = Wdrl mempty
          , Alonzo.txfee = mempty
          , Alonzo.txvldt = ValidityInterval SNothing SNothing
          , Alonzo.txUpdates = SNothing
          , Alonzo.reqSignerHashes = mempty
          , Alonzo.mint = mempty
          , Alonzo.scriptIntegrityHash = SNothing
          , Alonzo.adHash = SNothing
          , Alonzo.txnetworkid = SNothing
          }

    pingPong_1 :: PartialTx
    pingPong_1 = PartialTx tx [] []
      where
        Right tx = sealedTxFromBytes $ unsafeFromHex
            "84a500800d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd4\
            \5bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04ba\
            \bed17320d8d1b9ff9ad086e86f44ec02000e80a10481d87980f5f6"

    pingPong_2 :: PartialTx
    pingPong_2 = PartialTx
        { sealedTx = either (error . show) id $ sealedTxFromBytes $ mconcat
            [ unsafeFromHex "84a50081825820"
            , bytes tid
            , unsafeFromHex "000d80018183581d714d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e848058208392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e02000e80a20381591b72591b6f01000033233332222333322223322332232323332223233322232333333332222222232333222323333222232323322323332223233322232323322332232323333322222332233223322332233223322223223223232533530333330083333573466e1d40192004204f23333573466e1d401d2002205123333573466e1d40212000205323504b35304c3357389201035054310004d49926499263333573466e1d40112004205323333573466e1d40152002205523333573466e1d40192000205723504b35304c3357389201035054310004d49926499263333573466e1cd55cea8012400046601664646464646464646464646666ae68cdc39aab9d500a480008cccccccccc064cd409c8c8c8cccd5cd19b8735573aa004900011980f981d1aba15002302c357426ae8940088d4164d4c168cd5ce2481035054310005b49926135573ca00226ea8004d5d0a80519a8138141aba150093335502e75ca05a6ae854020ccd540b9d728169aba1500733502704335742a00c66a04e66aa0a8098eb4d5d0a8029919191999ab9a3370e6aae754009200023350213232323333573466e1cd55cea80124000466a05266a084eb4d5d0a80118239aba135744a00446a0ba6a60bc66ae712401035054310005f49926135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae7540092000233502733504275a6ae854008c11cd5d09aba2500223505d35305e3357389201035054310005f49926135573ca00226ea8004d5d09aba2500223505935305a3357389201035054310005b49926135573ca00226ea8004d5d0a80219a813bae35742a00666a04e66aa0a8eb88004d5d0a801181c9aba135744a00446a0aa6a60ac66ae71241035054310005749926135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a8011919191999ab9a3370ea00290031180f181d9aba135573ca00646666ae68cdc3a801240084603a608a6ae84d55cf280211999ab9a3370ea00690011180e98181aba135573ca00a46666ae68cdc3a80224000460406eb8d5d09aab9e50062350503530513357389201035054310005249926499264984d55cea80089baa001357426ae8940088d4124d4c128cd5ce249035054310004b49926104a1350483530493357389201035054350004a4984d55cf280089baa001135573a6ea80044d55ce9baa0012212330010030022001222222222212333333333300100b00a00900800700600500400300220012212330010030022001122123300100300212001122123300100300212001122123300100300212001212222300400521222230030052122223002005212222300100520011232230023758002640026aa078446666aae7c004940388cd4034c010d5d080118019aba200203323232323333573466e1cd55cea801a4000466600e6464646666ae68cdc39aab9d5002480008cc034c0c4d5d0a80119a8098169aba135744a00446a06c6a606e66ae71241035054310003849926135573ca00226ea8004d5d0a801999aa805bae500a35742a00466a01eeb8d5d09aba25002235032353033335738921035054310003449926135744a00226aae7940044dd50009110919980080200180110009109198008018011000899aa800bae75a224464460046eac004c8004d540d888c8cccd55cf80112804919a80419aa81898031aab9d5002300535573ca00460086ae8800c0b84d5d08008891001091091198008020018900089119191999ab9a3370ea002900011a80418029aba135573ca00646666ae68cdc3a801240044a01046a0526a605466ae712401035054310002b499264984d55cea80089baa001121223002003112200112001232323333573466e1cd55cea8012400046600c600e6ae854008dd69aba135744a00446a0466a604866ae71241035054310002549926135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088d407cd4c080cd5ce24810350543100021499261375400224464646666ae68cdc3a800a40084a00e46666ae68cdc3a8012400446a014600c6ae84d55cf280211999ab9a3370ea00690001280511a8111a981199ab9c490103505431000244992649926135573aa00226ea8004484888c00c0104488800844888004480048c8cccd5cd19b8750014800880188cccd5cd19b8750024800080188d4068d4c06ccd5ce249035054310001c499264984d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308d407cd4c080cd5ce2481035054310002149926499264992649926135573aa00826aae79400c4d55cf280109aab9e500113754002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024646464646666ae68cdc3a800a400446660106eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d400920002300a300b357426aae7940188d4040d4c044cd5ce2490350543100012499264984d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500a35300b3357389201035054310000c499264984d55cea80089baa001212230020032122300100320011122232323333573466e1cd55cea80124000466aa016600c6ae854008c014d5d09aba25002235007353008335738921035054310000949926135573ca00226ea8004498480048004448848cc00400c008448004488800c488800848880048004488800c488800848880048004448c8c00400488cc00cc008008004c8c8cc88cc88c8ccc888c8c8c8c8c8ccc888ccc888ccc888c8cccc8888c8cc88c8cccc8888c8cc88c8cc88c8ccc888c8c8cc88c8c8cc88c8c8c8cccc8888c8c8c8c8c8cc88c8cc88cc88ccccccccccccc8888888888888c8c8c8c8c8cccccccc88888888cc88cc88cc88cc88c8ccccc88888c8cc88cc88cc88c8cc88cc88cc88c8cc88c8c8c8cccc8888cccc8888c8888d4d540400108888c8c8c94cd4c24004ccc0140280240205400454cd4c24004cd5ce249025331000910115001109101153353508101003215335309001333573466e1cccc109400cd4c07800488004c0580212002092010910115002153353090013357389201025332000910115002109101150011533535080013300533501b00833303e03f5001323355306012001235355096010012233550990100233553063120012353550990100122335509c0100233704900080080080099a809801180a003003909a9aa84a8080091911a9a80f00091299a984a0098050010a99a984a00999aa9837090009a835283491a9aa84d8080091199aa9838890009a836a83611a9aa84f0080091199ab9a3370e900000084e0084d808008008a8020a99a984a0099ab9c49102533300095011500410950113535501e00522253353097013333355027253335301400113374a90001bb14984cdd2a40046ec52613374a90021bb149800c008c8cd400541d141d4488cc008cd40ac01cccc124128018cd4078034c07c04400403c4264044cd5ce249025335000980113535501a0012225335309301333335502325301d00100300200100b109501133573892010253340009401133573892010253360008f0113530220052235302d002222222222253353508b013303000a00b2135303a0012235303e0012220021350a10135309d0133573892010253300009e01498cccd5403488d4d404c008894ccd4c02400c54ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f054ccd4c01400854ccd4c02400c541f04d41f4cd542400554034cd405801c004541f054ccd4c02400c4d41f4cd542400554034cd4058020004541f0541f0541f04d41f4cd542400554034cd4058019419894ccd4c008004421c04421c044220048882280541e0488800c488800848880048004488800c48880084888004800444ccd5401d416541654164494cd4d41b8004848cd4168cd5421404d4c03000888004cd4168cd54214040052002505b505b12505a235355081013530100012235301b00222222222225335350793301e00a00b213530280012235302c00122235303100322335308701002230930116253353508201004213355098010020011309301161308a01162200211222212333300100500400300211200120011122212333001004003002112001122123300100300212001221233001003002200111222225335307533355304f120013504b504a235300b002223301500200300415335307533355304f120013504b504a235300b002223530160022222222222353501500d22533530840133355305e120013505450562353025001223304b00200400c10860113357389201024c30000850100315335307533355304f120013504b504a235300b002223530160022222222222353501300d22533530840133355305e12001350545056235302700122253353507a00121533530890133305108501003006153353507b330623019007009213308501001002108a01108a011089015335350763301b00c00d2135302500122353029001222333553055120012235302e00222235303300822353035005225335309301333308401004003002001133506f0090081008506701113508c01353088013357389201024c6600089014984218044cd5ce2481024c3100085010021077150741507415074122123300100300212001122123300100300212001221233001003002200122533335300300121505f21505f21505f2133355304612001504a235300d001225335306f3303300200413506300315062003212222300400521222230030052122223002005212222300100520013200135506c22233333333333353019001235300500322222222225335307153353506333355304b12001504f253353072333573466e3c0300041d01cc4d41980045419400c841d041c841cc4cd5ce249024c340007222353006004222222222253353506453353506433355304c1200150502353550790012253353075333573466e3c00803c1dc1d84d41a400c541a000884d419cd4d541e40048800454194854cd4c1ccccd5cd19baf00100c0750741075150701506f235300500322222222225335307133355304b120013504150432333573466ebc0300041d01cccd54c108480048d4d541e00048800400841cc4cd5ce249024c320007222225335306a333573466e1cd4c0200188888888888ccc09801c0380300041b01ac41b04cd5ce2481024c390006b22235300700522222222225335307333355304d1200135043504523530160012225335350690012153353078333040074003010153353506a35301601422222222223305b01b0022153353079333573466e3c0040081ec1e84d4c07401488cccc1b0008004c1d005541b841e841e441e441e002441d44cd5ce249024c6200074225335306833303002f0013335530331200150175045353006004222222222233355303d120012235301600222235301b00322335307100225335307a333573466e3c0500041f01ec4cd415801401c401c801d413c02441a84cd5ce2481024c610006925335306733302f02e001353005003222222222233355304b12001501f235301400122200200910691335738921024c36000682533530673335530411200135037503923300500400100110691335738921024c640006825335306733302f02e001353005003222222222233355304b12001501f23530120012235301600122200200a106913357389201024c35000682353005003222222222253353506333355304b12001504f235301200122533530743303800200e1350680031506700a213530120012235301600122253353506900121507610791506f22353006004222222222253353506433355304c120015050235301300122533530753303900200f1350690031506800a2107513357389201024c380007323530050032222222222353503100b22353503500222353503500822353503900222533530793333333222222253335306d33350640070060031533530800100215335308001005133350610070010041081011333506100700100410810113335061007001004333333335064075225335307b333573466e1c0080041f41f041ac54cd4c1ecccd5cd19b8900200107d07c1069106a22333573466e200080041f41f010088ccd5cd19b8900200107c07d22333573466e200080041f01f4894cd4c1ecccd5cd19b8900200107d07c10011002225335307b333573466e240080041f41f04008400401801401c00800400c41ec4cd5ce249024c330007a222222222212333333333300100b00a009008007006005004003002200122123300100300220012221233300100400300220012212330010030022001212222222300700822122222223300600900821222222230050081222222200412222222003221222222233002009008221222222233001009008200113350325001502f13001002222335530241200123535505a00122335505d002335530271200123535505d001223355060002333535502500123300a4800000488cc02c0080048cc02800520000013301c00200122337000040024446464600200a640026aa0b64466a6a05e0029000111a9aa82e00111299a982c199ab9a3371e0040120b40b22600e0022600c006640026aa0b44466a6a05c0029000111a9aa82d80111299a982b999ab9a3371e00400e0b20b020022600c00642444444444444601801a4424444444444446601601c01a42444444444444601401a44442444444444444666601202001e01c01a444244444444444466601001e01c01a4424444444444446600e01c01a42444444444444600c01a42444444444444600a01a42444444444444600801a42444444444444600601a4424444444444446600401c01a42444444444444600201a400224424660020060042400224424660020060042400244a66a607c666ae68cdc79a9801801110011a98018009100102001f8999ab9a3370e6a6006004440026a60060024400208007e207e442466002006004400244666ae68cdc480100081e81e111199aa980a890009a808a80811a9aa82100091199aa980c090009a80a280991a9aa82280091199a9aa8068009198052400000244660160040024660140029000000998020010009119aa98050900091a9aa8200009119aa821801199a9aa804000919aa98070900091a9aa8220009119aa8238011aa80780080091199aaa80401c801000919aa98070900091a9aa8220009119aa8238011aa806800800999aaa80181a001000888911199aa980209000a80a99aa98050900091a9aa8200009119aa8218011aa805800999aa980209000911a9aa82080111299a981e999aa980b890009a806a80791a9aa82200091198050010028030801899a80c802001a80b00099aa98050900091a9aa820000911919aa8220019800802990009aa82291299a9a80c80089aa8058019109a9aa82300111299a982119806001004099aa80800380089803001801190009aa81f1108911299a9a80a800880111099802801199aa980389000802802000889091118018020891091119801002802089091118008020890008919a80891199a9a803001910010010009a9a80200091000990009aa81c110891299a9a8070008a80811099a808980200119aa980309000802000899a80111299a981800108190800817891091980080180109000899a80191299a9816801080088170168919a80591199a9a802001910010010009a9a8010009100089109198008018010900091299a9a80d999aa980189000a80391a9aa81800091299a9816199ab9a3375e00200a05c05a26a0400062a03e002426a03c6a6aa060002440042a038640026aa05e4422444a66a6a00c00226a6a01400644002442666a6a01800a440046008004666aa600e2400200a00800222440042442446600200800624002266a00444a66a6a02c004420062002a02a24424660020060042400224446a6a008004446a6a00c00644a666a6026666a01400e0080042a66a604c00620022050204e2050244246600200600424002244464646464a666a6a01000c42a666a6a01200c42a666a6a0140104260082c260062c2a666a6a01400e4260082c260062c202a20262a666a6a01200e4260082c260062c2a666a6a01200c4260082c260062c20282a666a6a01000a42024202620222a666a6a01000a42a666a6a01200e42600a2c260082c2a666a6a01200c42600a2c260082c202820242a666a6a01000c42600a2c260082c2a666a6a01000a42600a2c260082c20264a666a6a01000a42a666a6a01200e42a666a6a01400e42666a01e014004002260222c260222c260202c20262a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c202420224a666a6a00e00842a666a6a01000c42a666a6a01200c42666a01c012004002260202c260202c2601e2c20242a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c202220204a666a6a00c00642a666a6a00e00a42a666a6a01000a42666a01a0100040022601e2c2601e2c2601c2c20222a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c2020201e4a666a6a00a00442a666a6a00c00842a666a6a00e00842666a01800e0040022601c2c2601c2c2601a2c20202a666a6a00a00642a666a6a00c00642666a01600c0040022601a2c2601a2c260182c201e201c2424446006008224440042244400224002246a6a0040024444444400e244444444246666666600201201000e00c00a008006004240024c244400624440042444002400244446466a601800a466a601a0084a66a602c666ae68cdc780100080c00b8a801880b900b919a9806802100b9299a980b199ab9a3371e00400203002e2a006202e2a66a6a00a00642a66a6a00c0044266a6014004466a6016004466a601e004466a60200044660280040024034466a6020004403446602800400244403444466a601a0084034444a66a6036666ae68cdc380300180e80e0a99a980d999ab9a3370e00a00403a03826602e00800220382038202a2a66a6a00a0024202a202a2424460040062244002240024244600400644424466600200a00800640024244600400642446002006400244666ae68cdc780100080480411199ab9a3370e00400201000e266ae712401024c630000413357389201024c370000313357389201024c64000021220021220012001235006353002335738921024c6700003498480048004448848cc00400c008448004498448c8c00400488cc00cc0080080050482d87a80d87980f5f6"
            ]
        , inputs =
            [ ( TxIn tid 0
              , TxOut
                    (Address $ unsafeFromHex
                    "714d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a")
                    (TokenBundle.fromCoin $ Coin 2_000_000)
              , Just $ Hash $ unsafeFromHex
                "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
              )
            ]
        , redeemers =
            [ RedeemerSpending (unsafeFromHex "D87A80") (TxIn tid 0)
            ]
        }
      where
        bytes (Hash x) = x
        tid = Hash $ B8.replicate 32 '1'

    txFee :: SealedTx -> Cardano.Lovelace
    txFee tx = withAlonzoBody tx $ \(Cardano.TxBody content) ->
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

    txMinFee :: SealedTx -> Cardano.Lovelace
    txMinFee = toCardanoLovelace
        . fromMaybe (error "evaluateMinimumFee returned nothing!")
        . evaluateMinimumFee testTxLayer (snd mockProtocolParametersForBalancing)

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
    -> StdGenSeed
    -> Property
prop_balanceTransactionBalanced (Wallet' utxo wal pending) (ShowBuildable partialTx) seed
    = withMaxSuccess 200 $ do
        let combinedUTxO = mconcat
                [ resolvedInputsUTxO Cardano.ShelleyBasedEraAlonzo partialTx
                , toCardanoUTxO Cardano.ShelleyBasedEraAlonzo $ view #utxo wal
                ]
        let originalBalance = txBalance (sealedTx partialTx) combinedUTxO
        let res = balanceTransaction'
                (utxo, wal, pending)
                seed
                partialTx
        case res of
            Right sealedTx -> counterexample ("\nResult: " <> pretty sealedTx) $ do
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
                (SelectionBalanceErrorOf
                (InsufficientMinCoinValues _)))) ->
                label "outputs below minCoinValue" $ property True
            Left (ErrBalanceTxNotYetSupported Deposits) ->
                label ("not yet supported: deposits") True
            Left (ErrBalanceTxExistingCollateral) ->
                label "existing collateral" True
            Left (ErrBalanceTxNotYetSupported ZeroAdaOutput) ->
                label "not yet supported: zero ada output" $ property True
            Left (ErrBalanceTxNotYetSupported ConflictingNetworks) ->
                label "not yet supported: conflicting networks" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf EmptyUTxO))) ->
                label "empty UTxO" $ property True
            Left (ErrBalanceTxNotYetSupported
                 (UnderestimatedFee delta candidateTx)) ->
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
                label "selection limit reached" $ property True
            Left
                (ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf (UnableToConstructChange _)))) ->
                label "unable to construct change" $ property True
            Left err -> label "other error" $
                counterexample ("balanceTransaction failed: " <> show err) False
  where
    a .<= b = counterexample (show a <> " /<= " <> show b) $ property $ a <= b

    hasCollateral :: SealedTx -> Bool
    hasCollateral tx = withAlonzoBody tx $ \(Cardano.TxBody content) ->
        case Cardano.txInsCollateral content of
            Cardano.TxInsCollateralNone -> False
            Cardano.TxInsCollateral _ [] -> False
            Cardano.TxInsCollateral _ (_:_) -> True

    txFee :: SealedTx -> Cardano.Lovelace
    txFee tx = withAlonzoBody tx $ \(Cardano.TxBody content) ->
        case Cardano.txFee content of
            Cardano.TxFeeExplicit _ c -> c
            Cardano.TxFeeImplicit _ -> error "implicit fee"

    txBalance :: SealedTx -> Cardano.UTxO Cardano.AlonzoEra -> Cardano.Lovelace
    txBalance tx u = withAlonzoBody tx $ \bod ->
        lovelaceFromCardanoTxOutValue
        $ Cardano.evaluateTransactionBalance nodePParams mempty u bod

    lovelaceFromCardanoTxOutValue
        :: forall era. Cardano.TxOutValue era -> Cardano.Lovelace
    lovelaceFromCardanoTxOutValue (TxOutAdaOnly _ coin) = coin
    lovelaceFromCardanoTxOutValue (TxOutValue _ val) = selectLovelace val


    (_, nodePParams) = mockProtocolParametersForBalancing

withAlonzoBody
    :: SealedTx
    -> (Cardano.TxBody Cardano.AlonzoEra -> a)
    -> a
withAlonzoBody (cardanoTx -> Cardano.InAnyCardanoEra Cardano.AlonzoEra tx) f =
    let Cardano.Tx bod _ = tx
    in f bod
withAlonzoBody _ _ = error "withAlonzoBody: other eras are not handled yet"


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
        { Cardano.protocolParamTxFeeFixed = 155381
        , Cardano.protocolParamTxFeePerByte = 44
        , Cardano.protocolParamMaxTxSize = 16384
        , Cardano.protocolParamMinUTxOValue = Nothing
        , Cardano.protocolParamMaxTxExUnits =
            Just $ Cardano.ExecutionUnits 10000000000 14000000
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
        , Cardano.protocolParamCostModels =
            Map.singleton
                (Cardano.AnyPlutusScriptVersion Cardano.PlutusScriptV1)
                costModel
        , Cardano.protocolParamPrices =
            Just $ Cardano.ExecutionUnitPrices
                (721 % 10_000_000)
                (577 % 10_000)
        , Cardano.protocolParamMaxBlockExUnits =
            Just $ Cardano.ExecutionUnits 10000000000 14000000
        , Cardano.protocolParamCollateralPercent = Just 1
        , Cardano.protocolParamMaxCollateralInputs = Just 3
        }
    costModel =
        Cardano.CostModel $ Map.fromList
            [("addInteger-cpu-arguments-intercept",197209)
            , ("addInteger-cpu-arguments-slope",0)
            , ("addInteger-memory-arguments-intercept",1)
            , ("addInteger-memory-arguments-slope",1)
            , ("appendByteString-cpu-arguments-intercept",396231)
            , ("appendByteString-cpu-arguments-slope",621)
            , ("appendByteString-memory-arguments-intercept",0)
            , ("appendByteString-memory-arguments-slope",1)
            , ("appendString-cpu-arguments-intercept",150000)
            , ("appendString-cpu-arguments-slope",1000)
            , ("appendString-memory-arguments-intercept",0)
            , ("appendString-memory-arguments-slope",1)
            , ("bData-cpu-arguments",150000)
            , ("bData-memory-arguments",32)
            , ("blake2b-cpu-arguments-intercept",2477736)
            , ("blake2b-cpu-arguments-slope",29175)
            , ("blake2b-memory-arguments",4)
            , ("cekApplyCost-exBudgetCPU",29773)
            , ("cekApplyCost-exBudgetMemory",100)
            , ("cekBuiltinCost-exBudgetCPU",29773)
            , ("cekBuiltinCost-exBudgetMemory",100)
            , ("cekConstCost-exBudgetCPU",29773)
            , ("cekConstCost-exBudgetMemory",100)
            , ("cekDelayCost-exBudgetCPU",29773)
            , ("cekDelayCost-exBudgetMemory",100)
            , ("cekForceCost-exBudgetCPU",29773)
            , ("cekForceCost-exBudgetMemory",100)
            , ("cekLamCost-exBudgetCPU",29773)
            , ("cekLamCost-exBudgetMemory",100)
            , ("cekStartupCost-exBudgetCPU",100)
            , ("cekStartupCost-exBudgetMemory",100)
            , ("cekVarCost-exBudgetCPU",29773)
            , ("cekVarCost-exBudgetMemory",100)
            , ("chooseData-cpu-arguments",150000)
            , ("chooseData-memory-arguments",32)
            , ("chooseList-cpu-arguments",150000)
            , ("chooseList-memory-arguments",32)
            , ("chooseUnit-cpu-arguments",150000)
            , ("chooseUnit-memory-arguments",32)
            , ("consByteString-cpu-arguments-intercept",150000)
            , ("consByteString-cpu-arguments-slope",1000)
            , ("consByteString-memory-arguments-intercept",0)
            , ("consByteString-memory-arguments-slope",1)
            , ("constrData-cpu-arguments",150000)
            , ("constrData-memory-arguments",32)
            , ("decodeUtf8-cpu-arguments-intercept",150000)
            , ("decodeUtf8-cpu-arguments-slope",1000)
            , ("decodeUtf8-memory-arguments-intercept",0)
            , ("decodeUtf8-memory-arguments-slope",8)
            , ("divideInteger-cpu-arguments-constant",148000)
            , ("divideInteger-cpu-arguments-model-arguments-intercept",425507)
            , ("divideInteger-cpu-arguments-model-arguments-slope",118)
            , ("divideInteger-memory-arguments-intercept",0)
            , ("divideInteger-memory-arguments-minimum",1)
            , ("divideInteger-memory-arguments-slope",1)
            , ("encodeUtf8-cpu-arguments-intercept",150000)
            , ("encodeUtf8-cpu-arguments-slope",1000)
            , ("encodeUtf8-memory-arguments-intercept",0)
            , ("encodeUtf8-memory-arguments-slope",8)
            , ("equalsByteString-cpu-arguments-constant",150000)
            , ("equalsByteString-cpu-arguments-intercept",112536)
            , ("equalsByteString-cpu-arguments-slope",247)
            , ("equalsByteString-memory-arguments",1)
            , ("equalsData-cpu-arguments-intercept",150000)
            , ("equalsData-cpu-arguments-slope",10000)
            , ("equalsData-memory-arguments",1)
            , ("equalsInteger-cpu-arguments-intercept",136542)
            , ("equalsInteger-cpu-arguments-slope",1326)
            , ("equalsInteger-memory-arguments",1)
            , ("equalsString-cpu-arguments-constant",1000)
            , ("equalsString-cpu-arguments-intercept",150000)
            , ("equalsString-cpu-arguments-slope",1000)
            , ("equalsString-memory-arguments",1)
            , ("fstPair-cpu-arguments",150000)
            , ("fstPair-memory-arguments",32)
            , ("headList-cpu-arguments",150000)
            , ("headList-memory-arguments",32)
            , ("iData-cpu-arguments",150000)
            , ("iData-memory-arguments",32)
            , ("ifThenElse-cpu-arguments",1)
            , ("ifThenElse-memory-arguments",1)
            , ("indexByteString-cpu-arguments",150000)
            , ("indexByteString-memory-arguments",1)
            , ("lengthOfByteString-cpu-arguments",150000)
            , ("lengthOfByteString-memory-arguments",4)
            , ("lessThanByteString-cpu-arguments-intercept",103599)
            , ("lessThanByteString-cpu-arguments-slope",248)
            , ("lessThanByteString-memory-arguments",1)
            , ("lessThanEqualsByteString-cpu-arguments-intercept",103599)
            , ("lessThanEqualsByteString-cpu-arguments-slope",248)
            , ("lessThanEqualsByteString-memory-arguments",1)
            , ("lessThanEqualsInteger-cpu-arguments-intercept",145276)
            , ("lessThanEqualsInteger-cpu-arguments-slope",1366)
            , ("lessThanEqualsInteger-memory-arguments",1)
            , ("lessThanInteger-cpu-arguments-intercept",179690)
            , ("lessThanInteger-cpu-arguments-slope",497)
            , ("lessThanInteger-memory-arguments",1)
            , ("listData-cpu-arguments",150000)
            , ("listData-memory-arguments",32)
            , ("mapData-cpu-arguments",150000)
            , ("mapData-memory-arguments",32)
            , ("mkCons-cpu-arguments",150000)
            , ("mkCons-memory-arguments",32)
            , ("mkNilData-cpu-arguments",150000)
            , ("mkNilData-memory-arguments",32)
            , ("mkNilPairData-cpu-arguments",150000)
            , ("mkNilPairData-memory-arguments",32)
            , ("mkPairData-cpu-arguments",150000)
            , ("mkPairData-memory-arguments",32)
            , ("modInteger-cpu-arguments-constant",148000)
            , ("modInteger-cpu-arguments-model-arguments-intercept",425507)
            , ("modInteger-cpu-arguments-model-arguments-slope",118)
            , ("modInteger-memory-arguments-intercept",0)
            , ("modInteger-memory-arguments-minimum",1)
            , ("modInteger-memory-arguments-slope",1)
            , ("multiplyInteger-cpu-arguments-intercept",61516)
            , ("multiplyInteger-cpu-arguments-slope",11218)
            , ("multiplyInteger-memory-arguments-intercept",0)
            , ("multiplyInteger-memory-arguments-slope",1)
            , ("nullList-cpu-arguments",150000)
            , ("nullList-memory-arguments",32)
            , ("quotientInteger-cpu-arguments-constant",148000)
            , ("quotientInteger-cpu-arguments-model-arguments-intercept",425507)
            , ("quotientInteger-cpu-arguments-model-arguments-slope",118)
            , ("quotientInteger-memory-arguments-intercept",0)
            , ("quotientInteger-memory-arguments-minimum",1)
            , ("quotientInteger-memory-arguments-slope",1)
            , ("remainderInteger-cpu-arguments-constant",148000)
            , ("remainderInteger-cpu-arguments-model-arguments-intercept",425507)
            , ("remainderInteger-cpu-arguments-model-arguments-slope",118)
            , ("remainderInteger-memory-arguments-intercept",0)
            , ("remainderInteger-memory-arguments-minimum",1)
            , ("remainderInteger-memory-arguments-slope",1)
            , ("sha2_256-cpu-arguments-intercept",2477736)
            , ("sha2_256-cpu-arguments-slope",29175)
            , ("sha2_256-memory-arguments",4)
            , ("sha3_256-cpu-arguments-intercept",0)
            , ("sha3_256-cpu-arguments-slope",82363)
            , ("sha3_256-memory-arguments",4)
            , ("sliceByteString-cpu-arguments-intercept",150000)
            , ("sliceByteString-cpu-arguments-slope",5000)
            , ("sliceByteString-memory-arguments-intercept",0)
            , ("sliceByteString-memory-arguments-slope",1)
            , ("sndPair-cpu-arguments",150000)
            , ("sndPair-memory-arguments",32)
            , ("subtractInteger-cpu-arguments-intercept",197209)
            , ("subtractInteger-cpu-arguments-slope",0)
            , ("subtractInteger-memory-arguments-intercept",1)
            , ("subtractInteger-memory-arguments-slope",1)
            , ("tailList-cpu-arguments",150000)
            , ("tailList-memory-arguments",32)
            , ("trace-cpu-arguments",150000)
            , ("trace-memory-arguments",32)
            , ("unBData-cpu-arguments",150000)
            , ("unBData-memory-arguments",32)
            , ("unConstrData-cpu-arguments",150000)
            , ("unConstrData-memory-arguments",32)
            , ("unIData-cpu-arguments",150000)
            , ("unIData-memory-arguments",32)
            , ("unListData-cpu-arguments",150000)
            , ("unListData-memory-arguments",32)
            , ("unMapData-cpu-arguments",150000)
            , ("unMapData-memory-arguments",32)
            , ("verifySignature-cpu-arguments-intercept",3345831)
            , ("verifySignature-cpu-arguments-slope",1)
            , ("verifySignature-memory-arguments",1)
            ]


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

estimateSignedTxSizeSpec :: Spec
estimateSignedTxSizeSpec =
    describe "estimateSignedTxSize" $ do
        it "equals the binary size of signed txs" $ property $ do
            forAllGoldens signedTxGoldens $ \hexTx -> do
                let bs = unsafeFromHex hexTx
                let tx = either (error . show) id $ sealedTxFromBytes bs
                -- 'mockProtocolParametersForBalancing' is not valid for
                -- 'ShelleyEra'.
                let pparams = (snd mockProtocolParametersForBalancing)
                        { Cardano.protocolParamMinUTxOValue = Just 1_000_000
                        }
                estimateSignedTxSize testTxLayer pparams tx
                    `shouldBe`
                    Just (TxSize $ fromIntegral $ BS.length bs)
  where
    forAllGoldens goldens f = forM_ goldens $ \x ->
        Hspec.counterexample (show x) $ f x

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
