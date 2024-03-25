{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use null" -}
{- HLINT ignore "Use camelCase" -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
#endif

module Cardano.Wallet.Shelley.TransactionSpec (spec) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    , XPub
    , toXPub
    , xprvFromBytes
    , xprvToBytes
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (Delegation, Payment)
    , Script (..)
    )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    )
import Cardano.Api.Gen
    ( genTx
    , genTxBodyContent
    , genTxInEra
    , genWitnesses
    )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic)
    )
import Cardano.Wallet
    ( Fee (..)
    , Percentile (..)
    , calculateFeePercentiles
    , signTransaction
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , deriveRewardAccount
    , hex
    , paymentAddress
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey
    , liftRawKey
    , publicKey
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    , genScript
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Integrity
    ( txIntegrity
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toCardanoLovelace
    , toCardanoTxIn
    )
import Cardano.Wallet.Primitive.NetworkId
    ( SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (UnsafeAssetName)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive
    , shrinkCoinPositive
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials
    , RootCredentials (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    , shrinkTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId.Gen
    ( genTokenPolicyId
    , shrinkTokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , cardanoTxIdeallyNoLaterThan
    , getSealedTxWitnesses
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , serialisedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , TxSize (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutTokenBundle
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx
    )
import Cardano.Wallet.Shelley.Transaction
    ( TxWitnessTag (..)
    , mkByronWitness
    , mkShelleyWitness
    , mkUnsignedTx
    , newTransactionLayer
    , txConstraints
    )
import Cardano.Wallet.Transaction
    ( SelectionOf (..)
    , TransactionLayer (..)
    , WitnessCountCtx (..)
    , selectionDelta
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Cardano.Write.Tx
    ( ErrBalanceTx (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    )
import Control.Arrow
    ( first
    )
import Control.Monad
    ( replicateM
    )
import Control.Monad.Random
    ( MonadRandom (..)
    , Random (randomR, randomRs)
    , random
    , randoms
    )
import Control.Monad.Trans.Except
    ( except
    , runExceptT
    )
import Cryptography.Hash.Blake
    ( blake2b224
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either
    ( isRight
    )
import Data.Function
    ( on
    , (&)
    )
import Data.IntCast
    ( intCast
    )
import Data.List
    ( nub
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromJust
    , isJust
    )
import Data.Ord
    ( comparing
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Ratio
    ( (%)
    )
import Data.Semigroup
    ( mtimesDefault
    )
import Data.Type.Equality
    ( testEquality
    , (:~:) (..)
    )
import Data.Word
    ( Word16
    , Word64
    , Word8
    )
import Fmt
    ( Buildable (..)
    , pretty
    , (+||)
    , (||+)
    )
import Internal.Cardano.Write.Tx
    ( AnyRecentEra (..)
    , CardanoApiEra
    , IsRecentEra
    , RecentEra (..)
    , ShelleyLedgerEra
    , cardanoEraFromRecentEra
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( TxSkeleton (..)
    , estimateTxSize
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Network.Block
    ( SlotNo (..)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , Testable
    , arbitraryPrintableChar
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllShow
    , frequency
    , oneof
    , property
    , scale
    , suchThat
    , vector
    , vectorOf
    , withMaxSuccess
    , (.||.)
    , (===)
    )
import Test.QuickCheck.Extra
    ( chooseNatural
    )
import Test.QuickCheck.Gen
    ( Gen (..)
    , listOf1
    )
import Test.QuickCheck.Random
    ( QCGen
    )
import Test.Utils.Pretty
    ( Pretty (..)
    , (====)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Error as Cardano
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Babbage.Core as Babbage
import qualified Cardano.Ledger.Babbage.Core as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Internal.Cardano.Write.Tx as Write
    ( BabbageEra
    , CardanoApiEra
    , IsRecentEra
    , PParams
    , RecentEra (RecentEraBabbage, RecentEraConway)
    , cardanoEraFromRecentEra
    , shelleyBasedEra
    , shelleyBasedEraFromRecentEra
    )

spec :: Spec
spec = describe "TransactionSpec" $ do
    decodeSealedTxSpec
    forAllRecentEras feeEstimationRegressionSpec
    forAllRecentEras binaryCalculationsSpec
    transactionConstraintsSpec
    describe "Sign transaction" $ do
        -- TODO [ADP-2849] The implementation must be restricted to work only in
        -- 'RecentEra's, not just the tests.
        spec_forAllRecentErasPendingConway
            "signTransaction adds reward account witness when necessary"
            prop_signTransaction_addsRewardAccountKey
        spec_forAllRecentErasPendingConway
            "signTransaction adds extra key witnesses when necessary"
            prop_signTransaction_addsExtraKeyWitnesses
        spec_forAllRecentErasPendingConway
            "signTransaction adds tx in witnesses when necessary"
            prop_signTransaction_addsTxInWitnesses
        spec_forAllRecentErasPendingConway
            "signTransaction adds collateral witnesses when necessary"
            prop_signTransaction_addsTxInCollateralWitnesses
        spec_forAllRecentErasPendingConway
            "signTransaction never removes witnesses"
            prop_signTransaction_neverRemovesWitnesses
        spec_forAllRecentErasPendingConway
            "signTransaction never changes tx body"
            prop_signTransaction_neverChangesTxBody
        spec_forAllRecentErasPendingConway
            "signTransaction preserves script integrity"
            prop_signTransaction_preservesScriptIntegrity

spec_forAllRecentErasPendingConway
    :: Testable prop => String -> (AnyRecentEra -> prop) -> Spec
spec_forAllRecentErasPendingConway description p =
    describe description
    $ forAllRecentEras
    $ \(AnyRecentEra recentEra) ->
        case recentEra of
            Write.RecentEraBabbage ->
                it (show recentEra) $ property $ p (AnyRecentEra recentEra)
            Write.RecentEraConway ->
                it (show recentEra) $ pendingWith "TODO: Conway"

instance Arbitrary SealedTx where
    arbitrary = sealedTxFromCardano <$> genTx

showTransactionBody
    :: RecentEra era
    -> Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
    -> String
showTransactionBody recentEra =
    either Cardano.displayError show
        . Cardano.createAndValidateTransactionBody
            (Write.shelleyBasedEraFromRecentEra recentEra)

unsafeMakeTransactionBody
    :: RecentEra era
    -> Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
    -> Cardano.TxBody (Write.CardanoApiEra era)
unsafeMakeTransactionBody recentEra =
    either (error . Cardano.displayError) id
        . Cardano.createAndValidateTransactionBody
            (Write.shelleyBasedEraFromRecentEra recentEra)

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

mkCredentials
    :: (XPrv, Passphrase "encryption")
    -> ClearCredentials ShelleyKey
mkCredentials (pk, hpwd) = RootCredentials (liftRawKey ShelleyKeyS pk) hpwd

prop_signTransaction_addsRewardAccountKey
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> Coin
    -- ^ Amount to withdraw
    -> Property
prop_signTransaction_addsRewardAccountKey
        (AnyRecentEra (recentEra :: RecentEra era)) rootXPrv utxo wdrlAmt =
    withMaxSuccess 10 $ do
        let
            shelleyEra :: Cardano.ShelleyBasedEra (Write.CardanoApiEra era)
            shelleyEra = Write.shelleyBasedEraFromRecentEra recentEra

            era = Write.cardanoEraFromRecentEra recentEra

            creds@(RootCredentials pk hpwd) = mkCredentials rootXPrv

            rawRewardK :: (XPrv, Passphrase "encryption")
            rawRewardK =
                ( getRawKey ShelleyKeyS
                    $ deriveRewardAccount hpwd pk minBound
                , hpwd
                )

            rewardAcctPubKey :: XPub
            rewardAcctPubKey = toXPub $ fst rawRewardK

            extraWdrls =
                [ withdrawalForKey SL.Mainnet rewardAcctPubKey
                    (toCardanoLovelace wdrlAmt)
                ]

            addWithdrawals
                :: Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
                -> Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
            addWithdrawals txBodyContent = txBodyContent
                { Cardano.txWithdrawals =
                    case Cardano.txWithdrawals txBodyContent of
                        Cardano.TxWithdrawalsNone ->
                            Cardano.TxWithdrawals shelleyEra extraWdrls
                        Cardano.TxWithdrawals _ wdrls ->
                            Cardano.TxWithdrawals shelleyEra $
                            wdrls <> extraWdrls
                }

        withBodyContent recentEra addWithdrawals $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction ShelleyKeyS
                    tl (AnyCardanoEra era) AnyWitnessCountCtx
                    (const Nothing) Nothing creds utxo Nothing sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits = withCardanoApiConstraints era $
                    InAnyCardanoEra era <$>
                        [ mkShelleyWitness txBody rawRewardK
                        ]

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
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Keys
    -> Property
prop_signTransaction_addsExtraKeyWitnesses
        (AnyRecentEra (recentEra :: RecentEra era)) rootK utxo extraKeys =
    withMaxSuccess 10 $ do
    let
        alonzoOnwards :: Cardano.AlonzoEraOnwards (Write.CardanoApiEra era)
        alonzoOnwards = case recentEra of
            Write.RecentEraBabbage -> Cardano.AlonzoEraOnwardsBabbage
            Write.RecentEraConway -> Cardano.AlonzoEraOnwardsConway

        era = Write.cardanoEraFromRecentEra recentEra

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
            :: Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
            -> Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
        addExtraWits txBodyContent = txBodyContent
            { Cardano.txExtraKeyWits =
                Cardano.TxExtraKeyWitnesses alonzoOnwards hashes
            }

    withBodyContent recentEra addExtraWits $ \(txBody, wits) -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra era)
                AnyWitnessCountCtx
                (lookupFnFromKeys extraKeys)
                Nothing
                (mkCredentials rootK)
                utxo
                Nothing
                sealedTx

            expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
            expectedWits = withCardanoApiConstraints era $
                InAnyCardanoEra era
                    . mkShelleyWitness txBody <$> extraKeys

        expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

keyToAddress :: (XPrv, Passphrase "encryption") -> Address
keyToAddress (xprv, _pwd) =
    -- TODO, decrypt?
    paymentAddress @ShelleyKey @'CredFromKeyK SMainnet
        . publicKey ShelleyKeyS
        . liftRawKey ShelleyKeyS
        $ xprv

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
    ->  ( Address ->
            Maybe (ShelleyKey 'CredFromKeyK XPrv, Passphrase "encryption")
        )
lookupFnFromKeys keys addr =
    let
        addrMap
            :: Map Address
                (ShelleyKey 'CredFromKeyK XPrv, Passphrase "encryption")
        addrMap = Map.fromList
            $ zip (keyToAddress <$> keys)
                (first (liftRawKey ShelleyKeyS) <$> keys)
    in
        Map.lookup addr addrMap

withBodyContent
    :: era ~ Write.CardanoApiEra era'
    => RecentEra era'
    ->  ( Cardano.TxBodyContent Cardano.BuildTx era ->
          Cardano.TxBodyContent Cardano.BuildTx era
        )
    -> ((Cardano.TxBody era, [Cardano.KeyWitness era]) -> Property)
    -> Property
withBodyContent recentEra modTxBody cont =
    forAllShow (genTxBodyContent era) (showTransactionBody recentEra) $
        \txBodyContent -> do
            let
                txBodyContent' = modTxBody txBodyContent
                txBody = unsafeMakeTransactionBody recentEra txBodyContent'

            forAll (genWitnesses era txBody) $ \wits -> cont (txBody, wits)
  where
    era = Write.cardanoEraFromRecentEra recentEra

checkSubsetOf :: (Eq a, Show a) => [a] -> [a] -> Property
checkSubsetOf as bs = property
    $ counterexample counterexampleText
    $ all ((`Set.member` ys) . ShowOrd) as
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
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInWitnesses
    (AnyRecentEra recentEra) rootK extraKeysNE =
    withMaxSuccess 10 $ do

    let extraKeys = NE.toList extraKeysNE

    utxoFromKeys extraKeys $ \utxo -> do
        let
            era = Write.cardanoEraFromRecentEra recentEra

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

        withBodyContent recentEra addTxIns $ \(txBody, wits) -> do
            let
                tl = testTxLayer

                sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                sealedTx' = signTransaction ShelleyKeyS tl
                    (AnyCardanoEra $ Write.cardanoEraFromRecentEra recentEra)
                    AnyWitnessCountCtx
                    (lookupFnFromKeys extraKeys)
                    Nothing
                    (mkCredentials rootK)
                    utxo
                    Nothing
                    sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits = withCardanoApiConstraints era $
                    InAnyCardanoEra era
                        . mkShelleyWitness txBody <$> extraKeys

            expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

prop_signTransaction_addsTxInCollateralWitnesses
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> NonEmpty (XPrv, Passphrase "encryption")
    -- ^ Keys
    -> Property
prop_signTransaction_addsTxInCollateralWitnesses
        (AnyRecentEra (recentEra :: RecentEra era)) rootK extraKeysNE =
    withMaxSuccess 10 $ do
        let
            alonzoOnwards :: Cardano.AlonzoEraOnwards (Write.CardanoApiEra era)
            alonzoOnwards = case recentEra of
                Write.RecentEraBabbage -> Cardano.AlonzoEraOnwardsBabbage
                Write.RecentEraConway -> Cardano.AlonzoEraOnwardsConway

            era = Write.cardanoEraFromRecentEra recentEra

            extraKeys = NE.toList extraKeysNE

        utxoFromKeys extraKeys $ \utxo -> do
            let
                txIns :: [TxIn]
                txIns = Map.keys $ unUTxO utxo

                addTxCollateralIns
                    :: Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
                    -> Cardano.TxBodyContent Cardano.BuildTx (Write.CardanoApiEra era)
                addTxCollateralIns txBodyContent = txBodyContent
                    { Cardano.txInsCollateral =
                        Cardano.TxInsCollateral alonzoOnwards
                            (toCardanoTxIn <$> txIns)
                    }

            withBodyContent recentEra addTxCollateralIns $ \(txBody, wits) -> do
                let
                    tl = testTxLayer

                    sealedTx = sealedTxFromCardano' $ Cardano.Tx txBody wits
                    sealedTx' = signTransaction ShelleyKeyS tl
                        (AnyCardanoEra era)
                        AnyWitnessCountCtx
                        (lookupFnFromKeys extraKeys)
                        Nothing
                        (mkCredentials rootK)
                        utxo
                        Nothing
                        sealedTx

                    expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                    expectedWits = withCardanoApiConstraints era $
                        InAnyCardanoEra era
                            . mkShelleyWitness txBody <$> extraKeys

                expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

prop_signTransaction_neverRemovesWitnesses
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Extra keys to form basis of address -> key lookup function
    -> Property
prop_signTransaction_neverRemovesWitnesses
    (AnyRecentEra recentEra) rootK utxo extraKeys =
    withMaxSuccess 10 $
    forAll (genTxInEra $ Write.cardanoEraFromRecentEra recentEra) $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra $ Write.cardanoEraFromRecentEra recentEra)
                AnyWitnessCountCtx
                (lookupFnFromKeys extraKeys)
                Nothing
                (mkCredentials rootK)
                utxo
                Nothing
                sealedTx

            witnessesBefore = getSealedTxWitnesses sealedTx
            witnessesAfter = getSealedTxWitnesses sealedTx'

        checkCoverage
            $ cover 10 (not $ null witnessesBefore) "witnesses non-empty before"
            $ witnessesBefore `checkSubsetOf` witnessesAfter

prop_signTransaction_neverChangesTxBody
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> [(XPrv, Passphrase "encryption")]
    -- ^ Extra keys to form basis of address -> key lookup function
    -> Property
prop_signTransaction_neverChangesTxBody
    (AnyRecentEra recentEra) rootK utxo extraKeys =
    withMaxSuccess 10
    $ forAll (genTxInEra $ Write.cardanoEraFromRecentEra recentEra)
    $ \tx -> do
        let
            era = Write.cardanoEraFromRecentEra recentEra
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra era)
                AnyWitnessCountCtx
                (lookupFnFromKeys extraKeys)
                Nothing
                (mkCredentials rootK)
                utxo
                Nothing
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

            equal
                :: InAnyCardanoEra (Cardano.TxBodyContent Cardano.ViewTx)
                -> InAnyCardanoEra (Cardano.TxBodyContent Cardano.ViewTx)
                -> Bool
            equal (InAnyCardanoEra era1 x1) (InAnyCardanoEra era2 x2) =
                case era1 `testEquality` era2 of
                    Nothing -> False
                    Just Refl ->
                        unsafeWithShelleyBasedEra era1
                        $ x1 == x2

        bodyContentBefore `equal` bodyContentAfter

prop_signTransaction_preservesScriptIntegrity
    :: AnyRecentEra
    -- ^ Era
    -> (XPrv, Passphrase "encryption")
    -- ^ Root key of wallet
    -> UTxO
    -- ^ UTxO of wallet
    -> Property
prop_signTransaction_preservesScriptIntegrity
    (AnyRecentEra recentEra) rootK utxo =
    withMaxSuccess 10
    $ forAll (genTxInEra $ Write.cardanoEraFromRecentEra recentEra)
    $ \tx -> do
        let
            tl = testTxLayer

            sealedTx = sealedTxFromCardano' tx
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra $ Write.cardanoEraFromRecentEra recentEra)
                AnyWitnessCountCtx
                (const Nothing)
                Nothing
                (mkCredentials rootK)
                utxo
                Nothing
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

forAllRecentEras :: (AnyRecentEra -> Spec) -> Spec
forAllRecentEras eraSpec = do
    eraSpec (AnyRecentEra RecentEraBabbage)
    eraSpec (AnyRecentEra RecentEraConway)

allEras :: [(Int, AnyCardanoEra)]
allEras =
    [ (1, AnyCardanoEra ByronEra)
    , (2, AnyCardanoEra ShelleyEra)
    , (3, AnyCardanoEra AllegraEra)
    , (4, AnyCardanoEra MaryEra)
    , (5, AnyCardanoEra AlonzoEra)
    , (6, AnyCardanoEra BabbageEra)
    , (7, AnyCardanoEra ConwayEra)
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

decodeSealedTxSpec :: Spec
decodeSealedTxSpec = describe "SealedTx serialisation/deserialisation" $ do
    it "tx with withdrawal" $ do
        let bytes = unsafeFromHex byteString
        let sealedTx = sealedTxFromBytes bytes
        sealedTx `shouldSatisfy` isRight

    prop "roundtrip for Shelley witnesses" prop_sealedTxRecentEraRoundtrip
  where
    byteString = mconcat
        [ "84a70081825820410a9cd4af08b3abe25c2d3b87af4c23d0bb2fb7577b639d5cfbdf"
        , "e13a4a696c0c0d80018182583901059f0c7b9899793d2c9afaeff4fd09bedd9df3b8"
        , "cb1b9c301ab8e0f7fb3c13a29d3798f1b77b47f2ddb31c19326b87ed6f71fb9a2713"
        , "3ad51b000001001d19d714021a000220ec03198d0f05a1581de1fb3c13a29d3798f1"
        , "b77b47f2ddb31c19326b87ed6f71fb9a27133ad51b000000e8d4a510000e80a0f5f6"
        ]

feeEstimationRegressionSpec :: AnyRecentEra -> Spec
feeEstimationRegressionSpec (AnyRecentEra (_era :: RecentEra era)) =
    describe "Regression tests" $ do
        it "#1740 Fee estimation at the boundaries" $ do
            let requiredCostLovelace :: Natural
                requiredCostLovelace = 166_029
            let estimateFee = except $ Left
                    $ ErrBalanceTxUnableToCreateChange
                    $ ErrBalanceTxUnableToCreateChangeError
                        { requiredCost =
                            Ledger.Coin $ intCast requiredCostLovelace
                        , shortfall =
                            Ledger.Coin 100_000
                        }
            result <- runExceptT (calculateFeePercentiles @_ @era estimateFee)
            result `shouldBe` Right
                ( Percentile $ Fee $ Coin requiredCostLovelace
                , Percentile $ Fee $ Coin requiredCostLovelace
                )

binaryCalculationsSpec :: AnyRecentEra -> Spec
binaryCalculationsSpec (AnyRecentEra era) =
    case era of
        RecentEraConway -> binaryCalculationsSpec' era
        RecentEraBabbage -> binaryCalculationsSpec' era

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
    :: forall era. Write.IsRecentEra era
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
                    RecentEraConway -> "84a400d901028182582000000000000000000000000000000000000000000000000000000000000000000001828258390101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101011a001e84808258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a0078175c021a0001faa403191e46a102d90102818458200100000000000000000000000000000000000000000000000000000000000000584005dacf0a9cbb4b5429ca2a31187c71d07c51b1151042076c536308c105069be7d099ed1e0b4be87303c03e8ae02586fb568ad8556cb108c00b8e63bc2adbc6065820000000000000000000000000000000000000000000000000000000000000000041a0f5f6"
                    RecentEraBabbage -> "84a4008182582000000000000000000000000000000000000000000000000000000000000000000001828258390101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101011a001e84808258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a0078175c021a0001faa403191e46a1028184582001000000000000000000000000000000000000000000000000000000000000005840d7af60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80f5820000000000000000000000000000000000000000000000000000000000000000041a0f5f6"
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
                    RecentEraConway -> "84a400d901028282582000000000000000000000000000000000000000000000000000000000000000000082582000000000000000000000000000000000000000000000000000000000000000000101838258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a005b8d808258390103030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031a005b8d808258390104040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404041a007801e0021a0002102003191e46a102d9010282845820010000000000000000000000000000000000000000000000000000000000000058405257f6056570317ebd7a878f032932bdf7a0e9e43f27296a12e22dd5c92f1f08b27c95b182c68ce3b1ed6942f2a4cc63aac7a89530c6086aaa3913685f75060f5820000000000000000000000000000000000000000000000000000000000000000041a0845820130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d477f515840b5b781d37d9a31dcbb000d92a461c3bc260e069c24d26d3996c6cadb03ae3ab518e8c7c19bd119fdcf112d4be101d20c7524065722ee0a25d97f31374fe9ef055820010101010101010101010101010101010101010101010101010101010101010141a0f5f6"
                    RecentEraBabbage -> "84a4008282582000000000000000000000000000000000000000000000000000000000000000000082582000000000000000000000000000000000000000000000000000000000000000000101838258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a005b8d808258390103030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031a005b8d808258390104040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404041a007801e0021a0002102003191e46a1028284582001000000000000000000000000000000000000000000000000000000000000005840e8e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b78955728bffa7efa54297c6a5d73337bd6280205b1759c13f79d4c93f29871fc51b78aeba80e5820000000000000000000000000000000000000000000000000000000000000000041a0845820130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158405835ff78c6fc5e4466a179ca659fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b52ade49b19a7208fd63a6e67a19c406b4826608fdc5307025506c3075820010101010101010101010101010101010101010101010101010101010101010141a0f5f6"
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
                    RecentEraConway -> "84a400d901028182582000000000000000000000000000000000000000000000000000000000000000000001828258390101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101011a001e84808258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a0078175c021a0001faa403191e46a102d90102818458200100000000000000000000000000000000000000000000000000000000000000584005dacf0a9cbb4b5429ca2a31187c71d07c51b1151042076c536308c105069be7d099ed1e0b4be87303c03e8ae02586fb568ad8556cb108c00b8e63bc2adbc6065820000000000000000000000000000000000000000000000000000000000000000044a1024100f5f6"
                    RecentEraBabbage -> "84a4008182582000000000000000000000000000000000000000000000000000000000000000000001828258390101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101011a001e84808258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a0078175c021a0001faa403191e46a1028184582001000000000000000000000000000000000000000000000000000000000000005840d7af60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80f5820000000000000000000000000000000000000000000000000000000000000000044a1024100f5f6"
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
                    RecentEraConway -> "84a400d901028282582000000000000000000000000000000000000000000000000000000000000000000082582000000000000000000000000000000000000000000000000000000000000000000101838258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a005b8d808258390103030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031a005b8d808258390104040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404041a007801e0021a0002102003191e46a102d9010282845820130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d477f515840b5b781d37d9a31dcbb000d92a461c3bc260e069c24d26d3996c6cadb03ae3ab518e8c7c19bd119fdcf112d4be101d20c7524065722ee0a25d97f31374fe9ef055820010101010101010101010101010101010101010101010101010101010101010144a1024100845820010000000000000000000000000000000000000000000000000000000000000058405257f6056570317ebd7a878f032932bdf7a0e9e43f27296a12e22dd5c92f1f08b27c95b182c68ce3b1ed6942f2a4cc63aac7a89530c6086aaa3913685f75060f5820000000000000000000000000000000000000000000000000000000000000000044a1024100f5f6"
                    RecentEraBabbage -> "84a4008282582000000000000000000000000000000000000000000000000000000000000000000082582000000000000000000000000000000000000000000000000000000000000000000101838258390102020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202021a005b8d808258390103030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031a005b8d808258390104040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404041a007801e0021a0002102003191e46a10282845820130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158405835ff78c6fc5e4466a179ca659fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b52ade49b19a7208fd63a6e67a19c406b4826608fdc5307025506c3075820010101010101010101010101010101010101010101010101010101010101010144a102410084582001000000000000000000000000000000000000000000000000000000000000005840e8e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b78955728bffa7efa54297c6a5d73337bd6280205b1759c13f79d4c93f29871fc51b78aeba80e5820000000000000000000000000000000000000000000000000000000000000000044a1024100f5f6"

            calculateBinary net utxo outs chgs pairs `shouldBe` binary

  where
    slotNo = SlotNo 7_750
    md = Nothing
    calculateBinary net utxo outs chgs pairs =
        hex (Cardano.serialiseToCBOR ledgerTx)
      where
          ledgerTx :: Cardano.Tx (CardanoApiEra era)
          ledgerTx = Cardano.makeSignedTransaction addrWits unsigned
          mkByronWitness' unsignedTx (_, (TxOut addr _)) =
              mkByronWitness unsignedTx net addr
          addrWits = zipWith (mkByronWitness' unsigned) inps pairs
          fee = toCardanoLovelace $ selectionDelta cs
          unsigned = either (error . show) id $
              mkUnsignedTx
                (Nothing, slotNo) (Right cs) md mempty [] fee
              TokenMap.empty TokenMap.empty Map.empty Map.empty Nothing Nothing
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
    it "size of empty transaction" prop_txConstraints_txBaseSize
    it "size of non-empty transaction" $
        property prop_txConstraints_txSize

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
    tx = makeShelleyTx era tc
    txBytes = Cardano.serialiseToCBOR tx
    sealedTxC = sealedTxFromCardano' tx
    sealedTxB = sealedTxFromBytes' currentEra txBytes

makeShelleyTx
    :: Write.IsRecentEra era
    => RecentEra era
    -> DecodeSetup
    -> Cardano.Tx (CardanoApiEra era)
makeShelleyTx _era testCase =
    Cardano.makeSignedTransaction addrWits unsigned
  where
    DecodeSetup utxo outs md slotNo pairs _netwk = testCase
    inps = Map.toList $ unUTxO utxo
    fee = toCardanoLovelace $ selectionDelta cs
    unsigned = either (error . show) id $
        mkUnsignedTx (Nothing, slotNo) (Right cs) md mempty [] fee
            TokenMap.empty TokenMap.empty Map.empty Map.empty Nothing Nothing
    addrWits =
        map (mkShelleyWitness unsigned) pairs
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

compareOnCBOR
    :: Cardano.IsShelleyBasedEra era => Cardano.Tx era -> SealedTx -> Property
compareOnCBOR b sealed = case cardanoTx sealed of
    InAnyCardanoEra era a ->
        unsafeWithShelleyBasedEra era
        $ Cardano.serialiseToCBOR a ==== Cardano.serialiseToCBOR b

unsafeWithShelleyBasedEra
    :: Cardano.CardanoEra era
    -> (Cardano.IsShelleyBasedEra era => a)
    -> a
unsafeWithShelleyBasedEra era a = case era of
    ByronEra ->
        error "TestSpec: Byron not supported anymore"
    ShelleyEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraShelley a
    AllegraEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraAllegra a
    MaryEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraMary a
    AlonzoEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraAlonzo a
    BabbageEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraBabbage a
    ConwayEra ->
        Cardano.shelleyBasedEraConstraints Cardano.ShelleyBasedEraConway a

--------------------------------------------------------------------------------

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

cardanoTx :: SealedTx -> InAnyCardanoEra Cardano.Tx
cardanoTx = cardanoTxIdeallyNoLaterThan maxBound

testTxLayer :: TransactionLayer ShelleyKey 'CredFromKeyK SealedTx
testTxLayer = newTransactionLayer ShelleyKeyS Cardano.Mainnet

--------------------------------------------------------------------------------
-- Transaction constraints
--------------------------------------------------------------------------------

emptyTxSkeleton :: TxSkeleton
emptyTxSkeleton =
    TxSkeleton TxWitnessShelleyUTxO 0 [] [] Nothing

mockTxConstraints :: TxConstraints
mockTxConstraints =
    txConstraints
        (mockPParamsForTxConstraints @Write.BabbageEra)
        TxWitnessShelleyUTxO
  where
    mockPParamsForTxConstraints
        :: forall era . Write.IsRecentEra era => Write.PParams era
    mockPParamsForTxConstraints = either (error . show) id $
        Cardano.toLedgerPParams
            Write.shelleyBasedEra
            mockCardanoApiPParamsForTxConstraints

    mockCardanoApiPParamsForTxConstraints :: Cardano.ProtocolParameters
    mockCardanoApiPParamsForTxConstraints = Cardano.ProtocolParameters
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
        , Cardano.protocolParamPoolRetireMaxEpoch = L.EpochInterval 2
        , Cardano.protocolParamStakePoolTargetNum = 100
        , Cardano.protocolParamPoolPledgeInfluence = 0
        , Cardano.protocolParamMonetaryExpansion = 0
        , Cardano.protocolParamTreasuryCut = 0
        , Cardano.protocolParamUTxOCostPerByte =
            Just $ Cardano.fromShelleyLovelace $
                Babbage.unCoinPerByte testParameter_coinsPerUTxOByte_Babbage
        -- Note: since 'txConstraints' does not make use of cost models, here
        -- we use the simplest possible value, which is 'mempty'.
        , Cardano.protocolParamCostModels = mempty
        , Cardano.protocolParamPrices =
            Just $ Cardano.ExecutionUnitPrices (721 % 10_000_000) (577 % 10_000)
        , Cardano.protocolParamMaxBlockExUnits =
            Just $ Cardano.ExecutionUnits 10_000_000_000 14_000_000
        , Cardano.protocolParamCollateralPercent = Just 150
        , Cardano.protocolParamMaxCollateralInputs = Just 3
        }

    testParameter_coinsPerUTxOByte_Babbage :: Ledger.CoinPerByte
    testParameter_coinsPerUTxOByte_Babbage
        = Ledger.CoinPerByte $ Ledger.Coin 4_310

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

-- Tests that using 'txBaseSize' to estimate the size of an empty selection
-- produces a result that is consistent with the result of using
-- 'estimateTxSize'.
--
prop_txConstraints_txBaseSize :: Property
prop_txConstraints_txBaseSize =
    txBaseSize mockTxConstraints === estimateTxSize emptyTxSkeleton

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
    MockSelection {txInputCount, txOutputs} = mock
    result :: TxSize
    result = mconcat
        [ txBaseSize mockTxConstraints
        , txInputCount `mtimesDefault` txInputSize mockTxConstraints
        , F.foldMap (txOutputSize mockTxConstraints . tokens) txOutputs
        ]
    lowerBound = estimateTxSize emptyTxSkeleton
        {txInputCount, txOutputs }
    -- We allow a small amount of overestimation due to the slight variation in
    -- the marginal size of an input:
    upperBound = lowerBound <> txInputCount `mtimesDefault` TxSize 4

newtype Large a = Large { unLarge :: a }
    deriving (Eq, Show)

instance Arbitrary (Large TokenBundle) where
    arbitrary = fmap Large . genTxOutTokenBundle =<< choose (1, 128)

instance Arbitrary AnyRecentEra where
    arbitrary = elements
        [ AnyRecentEra RecentEraBabbage
        , AnyRecentEra RecentEraConway
        ]

instance Arbitrary AssetId where
    arbitrary =
        AssetId
        <$> arbitrary
        -- In the calculation of the size of the Tx, the minting of assets
        -- increases the size of the Tx by both a constant factor per asset
        -- plus a variable factor (the size of the asset name). In a typical
        -- setting, the constant factor dominantes (it's about 40 bytes per
        -- asset, whereas the size of an asset name has a maximum of 32 bytes).
        -- So we create a generator here that forces the variable factor to
        -- dominate so we can test the sanity of the estimation algorithm.
        <*> (UnsafeAssetName . BS.pack <$> vector 128)

instance Arbitrary Coin where
    arbitrary = genCoinPositive
    shrink = shrinkCoinPositive

instance Arbitrary (Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ Hash $ BS.pack bs

instance Arbitrary Cardano.NetworkId where
    arbitrary = oneof
        [ pure Cardano.Mainnet
        , Cardano.Testnet . Cardano.NetworkMagic <$> arbitrary
        ]

instance Arbitrary TokenBundle where
    arbitrary = genTokenBundleSmallRange
    shrink = shrinkTokenBundleSmallRange

instance Arbitrary TokenPolicyId where
    arbitrary = genTokenPolicyId
    shrink = shrinkTokenPolicyId

instance Arbitrary (Script KeyHash) where
    arbitrary = do
        keyHashes <- vectorOf 10 arbitrary
        genScript keyHashes

instance Arbitrary TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ TxIn txId ix

instance Arbitrary TxOut where
    arbitrary =
        TxOut addr <$> scale (`mod` 4) genTokenBundleSmallRange
      where
        addr = Address $ BS.pack (1:replicate 56 0)
    shrink (TxOut addr bundle) =
        [ TxOut addr bundle'
        | bundle' <- shrinkTokenBundleSmallRange bundle
        ]

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- oneof [pure Payment, pure Delegation]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary

-- https://mail.haskell.org/pipermail/haskell-cafe/2016-August/124742.html
mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)

newtype ShowBuildable a = ShowBuildable a
    deriving newtype Arbitrary

instance Buildable a => Show (ShowBuildable a) where
    show (ShowBuildable x) = pretty x

instance Arbitrary (Hash "Datum") where
    arbitrary = pure $ Hash $ BS.pack $ replicate 28 0

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

-- | Hack until signTransaction and properties are converted to ledger types
withCardanoApiConstraints
    :: forall era a. CardanoEra era
    -> ( ( CardanoApiEra (ShelleyLedgerEra era) ~ era
         , IsRecentEra (ShelleyLedgerEra era)
         ) => a)
    -> a
withCardanoApiConstraints e a = case e of
    Cardano.ConwayEra -> a
    Cardano.BabbageEra -> a
    Cardano.AlonzoEra -> err
    Cardano.MaryEra -> err
    Cardano.AllegraEra -> err
    Cardano.ShelleyEra -> err
    Cardano.ByronEra -> err
  where
    err = error "withCardanoApiConstraints: unsupported era"
