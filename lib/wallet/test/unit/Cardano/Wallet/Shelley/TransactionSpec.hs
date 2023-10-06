{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
    ( XPrv, XPub, toXPub, xprvFromBytes, xprvToBytes, xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole (Delegation, Payment), Script (..) )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , CardanoEraStyle (..)
    , InAnyCardanoEra (..)
    , IsCardanoEra (..)
    , IsShelleyBasedEra (..)
    , ShelleyBasedEra (..)
    )
import Cardano.Api.Gen
    ( genEncodingBoundaryLovelace
    , genTx
    , genTxBodyContent
    , genTxInEra
    , genWitnesses
    )
import Cardano.Ledger.Api
    ( bootAddrTxWitsL, scriptTxWitsL, witsTxL )
import Cardano.Mnemonic
    ( SomeMnemonic (SomeMnemonic) )
import Cardano.Numeric.Util
    ( power )
import Cardano.Wallet
    ( Fee (..), Percentile (..), calculateFeePercentiles, signTransaction )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), deriveRewardAccount, hex, paymentAddress )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Keys.WalletKey
    ( getRawKey, liftRawKey, publicKey )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Gen
    ( genMnemonic, genScript )
import Cardano.Wallet.Primitive.NetworkId
    ( SNetworkId (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, genCoinPositive, shrinkCoin, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials, RootCredentials (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId, TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName), TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenPolicyId, shrinkTokenPolicyId )
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
import Cardano.Wallet.Read.Primitive.Tx.Features.Integrity
    ( txIntegrity )
import Cardano.Wallet.Read.Tx.Cardano
    ( fromCardanoApiTx )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoLovelace, toCardanoLovelace, toCardanoTxIn )
import Cardano.Wallet.Shelley.Transaction
    ( EraConstraints
    , TxWitnessTag (..)
    , mkByronWitness
    , mkShelleyWitness
    , mkUnsignedTx
    , _decodeSealedTx
    )
import Cardano.Wallet.Transaction
    ( SelectionOf (..), WitnessCountCtx (..), selectionDelta )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Cardano.Write.Tx
    ( AnyRecentEra (..)
    , FeePerByte (..)
    , RecentEra (..)
    , cardanoEraFromRecentEra
    , recentEra
    , shelleyBasedEraFromRecentEra
    )
import Cardano.Write.Tx.Balance
    ( ErrBalanceTx (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrMoreSurplusNeeded (..)
    , ErrUpdateSealedTx (..)
    , TxFeeAndChange (..)
    , TxFeeUpdate (..)
    , TxUpdate (..)
    , costOfIncreasingCoin
    , distributeSurplusDelta
    , maximumCostOfIncreasingCoin
    , noTxUpdate
    , sizeOfCoin
    , updateTx
    )
import Cardano.Write.Tx.BalanceSpec
    ( TxBalanceSurplus (..)
    , dummyPolicyK
    , mockPParamsForBalancing
    , prop_distributeSurplus_onSuccess
    , prop_distributeSurplus_onSuccess_conservesSurplus
    , prop_distributeSurplus_onSuccess_coversCostIncrease
    , prop_distributeSurplus_onSuccess_doesNotReduceChangeCoinValues
    , prop_distributeSurplus_onSuccess_doesNotReduceFeeValue
    , prop_distributeSurplus_onSuccess_preservesChangeAddresses
    , prop_distributeSurplus_onSuccess_preservesChangeLength
    , prop_distributeSurplus_onSuccess_preservesChangeNonAdaAssets
    , testTxLayer
    )
import Cardano.Write.Tx.Sign
    ( estimateKeyWitnessCount, estimateSignedTxSize )
import Cardano.Write.Tx.SizeEstimation
    ( TxSkeleton (..), estimateTxSize, txConstraints )
import Control.Arrow
    ( first )
import Control.Lens
    ( (^.) )
import Control.Monad
    ( forM, forM_, replicateM )
import Control.Monad.Random
    ( MonadRandom (..), Random (randomR, randomRs), random, randoms )
import Control.Monad.Trans.Except
    ( except, runExceptT )
import Crypto.Hash.Extra
    ( blake2b224 )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( isDigit )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( on, (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.IntCast
    ( intCast )
import Data.List
    ( isSuffixOf, nub, sortOn )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromJust, fromMaybe, isJust )
import Data.Ord
    ( comparing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Semigroup
    ( mtimesDefault )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word64, Word8 )
import Fmt
    ( Buildable (..), pretty, (+||), (||+) )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import System.Directory
    ( listDirectory )
import System.FilePath
    ( takeExtension, (</>) )
import Test.Hspec
    ( Spec
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
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , Testable
    , arbitraryPrintableChar
    , arbitrarySizedNatural
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
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
    , (.||.)
    , (===)
    )
import Test.QuickCheck.Extra
    ( chooseNatural, genNonEmpty, report, shrinkNatural, shrinkNonEmpty )
import Test.QuickCheck.Gen
    ( Gen (..), listOf1 )
import Test.QuickCheck.Random
    ( QCGen )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Pretty
    ( Pretty (..), (====) )
import Text.Read
    ( readMaybe )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as TxOutGen
import qualified Cardano.Wallet.Shelley.Compatibility as Compatibility
import qualified Cardano.Write.ProtocolParameters as Write
import qualified Cardano.Write.Tx as Write
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Hspec.Extra as Hspec

spec :: Spec
spec = describe "TransactionSpec" $ do
    decodeSealedTxSpec
    feeEstimationRegressionSpec
    forAllRecentEras binaryCalculationsSpec
    transactionConstraintsSpec
    updateTxSpec
    distributeSurplusSpec
    estimateSignedTxSizeSpec
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
    :: Testable prop => String -> (AnyCardanoEra -> prop) -> Spec
spec_forAllRecentErasPendingConway description p =
    describe description $
    forAllRecentEras'
        $ \(AnyCardanoEra era) ->
            if AnyCardanoEra era == AnyCardanoEra ConwayEra
            then it (show era) $ pendingWith "TODO: Conway"
            else it (show era) $ property $ p (AnyCardanoEra era)
  where
    forAllRecentEras' f = forAllRecentEras $ \(AnyRecentEra era) ->
        f $ AnyCardanoEra $ Write.cardanoEraFromRecentEra era

instance Arbitrary SealedTx where
    arbitrary = sealedTxFromCardano <$> genTx

showTransactionBody
    :: forall era
     . IsCardanoEra era
    => Cardano.TxBodyContent Cardano.BuildTx era
    -> String
showTransactionBody =
    either Cardano.displayError show . Cardano.createAndValidateTransactionBody

unsafeMakeTransactionBody
    :: forall era
     . IsCardanoEra era
    => Cardano.TxBodyContent Cardano.BuildTx era
    -> Cardano.TxBody era
unsafeMakeTransactionBody =
    either (error . Cardano.displayError) id
        . Cardano.createAndValidateTransactionBody

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

mkCredentials
    :: (XPrv, Passphrase "encryption")
    -> ClearCredentials ShelleyKey
mkCredentials (pk, hpwd) = RootCredentials (liftRawKey ShelleyKeyS pk) hpwd

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
                sealedTx' = signTransaction ShelleyKeyS
                    tl (AnyCardanoEra era) AnyWitnessCountCtx
                    (const Nothing) Nothing creds utxo Nothing sealedTx

                expectedWits :: [InAnyCardanoEra Cardano.KeyWitness]
                expectedWits = InAnyCardanoEra era <$>
                    case Cardano.cardanoEraStyle era of
                        LegacyByronEra -> error $ unwords
                            [ "Withdrawal witnesses are not supported in the"
                            , "Byron era."
                            ]
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
            expectedWits = InAnyCardanoEra era <$>
                case Cardano.cardanoEraStyle era of
                    LegacyByronEra ->
                        -- signTransaction does nothing in Byron era
                        []
                    ShelleyBasedEra _ ->
                        mkShelleyWitness txBody <$> extraKeys

        expectedWits `checkSubsetOf` (getSealedTxWitnesses sealedTx')

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genNonEmpty arbitrary
    shrink = shrinkNonEmpty shrink

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
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra era)
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
            sealedTx' = signTransaction ShelleyKeyS tl
                (AnyCardanoEra era)
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

feeEstimationRegressionSpec :: Spec
feeEstimationRegressionSpec = describe "Regression tests" $ do
    it "#1740 Fee estimation at the boundaries" $ do
        let requiredCostLovelace :: Natural
            requiredCostLovelace = 166_029
        let estimateFee = except $ Left
                $ ErrBalanceTxUnableToCreateChange
                $ ErrBalanceTxUnableToCreateChangeError
                    { requiredCost = Ledger.Coin $ intCast requiredCostLovelace
                    , shortfall = Ledger.Coin 100_000
                    }
        result <- runExceptT (calculateFeePercentiles estimateFee)
        result `shouldBe` Right
            ( Percentile $ Fee $ Coin requiredCostLovelace
            , Percentile $ Fee $ Coin requiredCostLovelace
            )

binaryCalculationsSpec :: AnyRecentEra -> Spec
binaryCalculationsSpec (AnyRecentEra era) =
    case era of
        RecentEraConway -> binaryCalculationsSpec' @Cardano.ConwayEra era
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
                    RecentEraConway -> mconcat
                      [ "84a400818258200000000000000000000000000000000000000000"
                      , "000000000000000000000000000182a20058390101010101010101"
                      , "010101010101010101010101010101010101010101010101010101"
                      , "01010101010101010101010101010101010101010101011a001e84"
                      , "80a200583901020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "0202020202020202011a0078175c021a0001faa403191e46a10281"
                      , "845820010000000000000000000000000000000000000000000000"
                      , "000000000000000058407154db81463825f150bb3b9b0824caf151"
                      , "3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d"
                      , "483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "41a0f5f6"
                      ]
                    RecentEraBabbage -> mconcat
                      [ "84a400818258200000000000000000000000000000000000000000"
                      , "000000000000000000000000000182a20058390101010101010101"
                      , "010101010101010101010101010101010101010101010101010101"
                      , "01010101010101010101010101010101010101010101011a001e84"
                      , "80a200583901020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "0202020202020202011a0078175c021a0001faa403191e46a10281"
                      , "845820010000000000000000000000000000000000000000000000"
                      , "000000000000000058407154db81463825f150bb3b9b0824caf151"
                      , "3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d"
                      , "483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "41a0f5f6"
                      ]
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
                    RecentEraConway -> mconcat
                      [ "84a400828258200000000000000000000000000000000000000000"
                      , "000000000000000000000000008258200000000000000000000000"
                      , "000000000000000000000000000000000000000000010183a20058"
                      , "390102020202020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "02020202011a005b8d80a200583901030303030303030303030303"
                      , "030303030303030303030303030303030303030303030303030303"
                      , "0303030303030303030303030303030303011a005b8d80a2005839"
                      , "010404040404040404040404040404040404040404040404040404"
                      , "040404040404040404040404040404040404040404040404040404"
                      , "040404011a007801e0021a0002102003191e46a102828458200100"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "00000058401a8667d2d0af4e24d4d385443002f1e9036063bdb7c6"
                      , "2d45447a2e176ded81a11683bd944c6d7db6e5fd886840025f6319"
                      , "2a382e526f4150e2b336ee9ed80808582000000000000000000000"
                      , "0000000000000000000000000000000000000000000041a0845820"
                      , "130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7c"
                      , "e38d477f515840320ed7d1513b0f1b61381f7942a07b627b246c85"
                      , "a13b2623e4868ea82488c778a7760124f3a17f924c08d425c0717d"
                      , "f6cd898eb4ab8439a16e08befdc415120e58200101010101010101"
                      , "01010101010101010101010101010101010101010101010141a0f5"
                      , "f6"
                      ]
                    RecentEraBabbage -> mconcat
                      [ "84a400828258200000000000000000000000000000000000000000"
                      , "000000000000000000000000008258200000000000000000000000"
                      , "000000000000000000000000000000000000000000010183a20058"
                      , "390102020202020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "02020202011a005b8d80a200583901030303030303030303030303"
                      , "030303030303030303030303030303030303030303030303030303"
                      , "0303030303030303030303030303030303011a005b8d80a2005839"
                      , "010404040404040404040404040404040404040404040404040404"
                      , "040404040404040404040404040404040404040404040404040404"
                      , "040404011a007801e0021a0002102003191e46a102828458200100"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "00000058401a8667d2d0af4e24d4d385443002f1e9036063bdb7c6"
                      , "2d45447a2e176ded81a11683bd944c6d7db6e5fd886840025f6319"
                      , "2a382e526f4150e2b336ee9ed80808582000000000000000000000"
                      , "0000000000000000000000000000000000000000000041a0845820"
                      , "130ae82201d7072e6fbfc0a1884fb54636554d14945b799125cf7c"
                      , "e38d477f515840320ed7d1513b0f1b61381f7942a07b627b246c85"
                      , "a13b2623e4868ea82488c778a7760124f3a17f924c08d425c0717d"
                      , "f6cd898eb4ab8439a16e08befdc415120e58200101010101010101"
                      , "01010101010101010101010101010101010101010101010141a0f5"
                      , "f6"
                      ]
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
                    RecentEraConway -> mconcat
                      [ "84a400818258200000000000000000000000000000000000000000"
                      , "000000000000000000000000000182a20058390101010101010101"
                      , "010101010101010101010101010101010101010101010101010101"
                      , "01010101010101010101010101010101010101010101011a001e84"
                      , "80a200583901020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "0202020202020202011a0078175c021a0001faa403191e46a10281"
                      , "845820010000000000000000000000000000000000000000000000"
                      , "000000000000000058407154db81463825f150bb3b9b0824caf151"
                      , "3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d"
                      , "483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "44a1024100f5f6"
                      ]
                    RecentEraBabbage -> mconcat
                      [ "84a400818258200000000000000000000000000000000000000000"
                      , "000000000000000000000000000182a20058390101010101010101"
                      , "010101010101010101010101010101010101010101010101010101"
                      , "01010101010101010101010101010101010101010101011a001e84"
                      , "80a200583901020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "0202020202020202011a0078175c021a0001faa403191e46a10281"
                      , "845820010000000000000000000000000000000000000000000000"
                      , "000000000000000058407154db81463825f150bb3b9b0824caf151"
                      , "3716f73498afe61d917a5621912a2b3df252bea14683a9ee56710d"
                      , "483a53a5aa35247e0d2b80e6300f7bdec763a20458200000000000"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "44a1024100f5f6"
                      ]
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
            let binary = mconcat
                      [ "84a400828258200000000000000000000000000000000000000000"
                      , "000000000000000000000000008258200000000000000000000000"
                      , "000000000000000000000000000000000000000000010183a20058"
                      , "390102020202020202020202020202020202020202020202020202"
                      , "020202020202020202020202020202020202020202020202020202"
                      , "02020202011a005b8d80a200583901030303030303030303030303"
                      , "030303030303030303030303030303030303030303030303030303"
                      , "0303030303030303030303030303030303011a005b8d80a2005839"
                      , "010404040404040404040404040404040404040404040404040404"
                      , "040404040404040404040404040404040404040404040404040404"
                      , "040404011a007801e0021a0002102003191e46a10282845820130a"
                      , "e82201d7072e6fbfc0a1884fb54636554d14945b799125cf7ce38d"
                      , "477f515840320ed7d1513b0f1b61381f7942a07b627b246c85a13b"
                      , "2623e4868ea82488c778a7760124f3a17f924c08d425c0717df6cd"
                      , "898eb4ab8439a16e08befdc415120e582001010101010101010101"
                      , "0101010101010101010101010101010101010101010144a1024100"
                      , "845820010000000000000000000000000000000000000000000000"
                      , "000000000000000058401a8667d2d0af4e24d4d385443002f1e903"
                      , "6063bdb7c62d45447a2e176ded81a11683bd944c6d7db6e5fd8868"
                      , "40025f63192a382e526f4150e2b336ee9ed8080858200000000000"
                      , "000000000000000000000000000000000000000000000000000000"
                      , "44a1024100f5f6"
                      ]
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
          fee = toCardanoLovelace $ selectionDelta cs
          unsigned = either (error . show) id $
              mkUnsignedTx (shelleyBasedEraFromRecentEra era)
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
    fee = toCardanoLovelace $ selectionDelta cs
    unsigned = either (error . show) id $
        mkUnsignedTx era (Nothing, slotNo) (Right cs) md mempty [] fee
        TokenMap.empty TokenMap.empty Map.empty Map.empty Nothing Nothing
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


--------------------------------------------------------------------------------
-- Transaction constraints
--------------------------------------------------------------------------------

emptyTxSkeleton :: TxSkeleton
emptyTxSkeleton =
    TxSkeleton TxWitnessShelleyUTxO 0 [] [] Nothing

mockTxConstraints :: TxConstraints
mockTxConstraints =
    txConstraints
        (mockPParamsForBalancing @Cardano.BabbageEra)
        TxWitnessShelleyUTxO

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

-- Tests that using 'txBaseSize' to estimate the size of an empty selection                                                                        ....
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

            let expectedCostIncrease = Coin 176
            let mainnet = mainnetFeePerByte
            costOfIncreasingCoin mainnet (Coin $ 2 `power` 32 - 1) (Coin 1)
                `shouldBe` expectedCostIncrease

        it "produces results in the range [0, 8 * feePerByte]" $
            property $ \c increase -> do
                let res = costOfIncreasingCoin (FeePerByte 1) c increase
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

        -- NOTE: The test values below make use of 255 being encoded as 2 bytes,
        -- and 256 as 3 bytes.

        describe "when increasing change increases fee" $
            it "will increase fee (99 lovelace for change, 1 for fee)" $
                distributeSurplusDelta
                    (FeePerByte 1)
                    (Coin 100)
                    (TxFeeAndChange (Coin 200) [Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 1) [Coin 99])

        describe "when increasing fee increases fee" $
            it "will increase fee (98 lovelace for change, 2 for fee)" $ do
                distributeSurplusDelta
                    (FeePerByte 1)
                    (Coin 100)
                    (TxFeeAndChange (Coin 255) [Coin 200])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 2) [Coin 98])

        describe
            (unwords
                [ "when increasing the change costs more in fees than the"
                , "increase itself"
                ]) $ do
            it "will try burning the surplus as fees" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (Coin 10)
                    (TxFeeAndChange (Coin 200) [Coin 255])
                    `shouldBe`
                    Right (TxFeeAndChange (Coin 10) [Coin 0])

            it "will fail if neither the fee can be increased" $ do
                distributeSurplusDelta
                    mainnetFeePerByte
                    (Coin 10)
                    (TxFeeAndChange (Coin 255) [Coin 255])
                    `shouldBe`
                    Left (ErrMoreSurplusNeeded $ Coin 34)

        describe "when no change output is present" $ do
            it "will burn surplus as excess fees" $
                property $ \surplus fee0 -> do
                    distributeSurplusDelta
                        (FeePerByte 1)
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
    :: FeePerByte -> Coin -> Coin -> [Coin] -> Property
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

instance Arbitrary FeePerByte where
    arbitrary = frequency
        [ (1, pure mainnetFeePerByte)
        , (7, FeePerByte <$> arbitrarySizedNatural)
        ]

    shrink (FeePerByte x) =
        FeePerByte <$> shrinkNatural x

mainnetFeePerByte :: FeePerByte
mainnetFeePerByte = FeePerByte 44

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
    :: FeePerByte -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
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
    :: FeePerByte -> TxBalanceSurplus Coin -> TxFeeAndChange [TxOut] -> Property
prop_distributeSurplus_onSuccess_increasesValuesByDelta =
    prop_distributeSurplus_onSuccess $ \policy surplus
        (TxFeeAndChange feeOriginal changeOriginal)
        (TxFeeAndChange feeModified changeModified) ->
            let (TxFeeAndChange feeDelta changeDeltas) =
                    either (error . show) id
                    $ distributeSurplusDelta policy surplus
                    $ TxFeeAndChange
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

newtype ShowBuildable a = ShowBuildable a
    deriving newtype Arbitrary

instance Buildable a => Show (ShowBuildable a) where
    show (ShowBuildable x) = pretty x

instance Arbitrary (Hash "Datum") where
    arbitrary = pure $ Hash $ BS.pack $ replicate 28 0

updateTxSpec :: Spec
updateTxSpec = describe "updateTx" $ do
    describe "no existing key witnesses" $ do
        txs <- readTestTransactions
        forM_ txs $ \(filepath, sealedTx) -> do
            let anyRecentEraTx
                    = fromJust $ Write.asAnyRecentEra $ cardanoTx sealedTx
            it ("without TxUpdate: " <> filepath) $ do
                Write.withInAnyRecentEra anyRecentEraTx $ \tx ->
                    case updateTx tx noTxUpdate of
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
                                    newEncoding = convertToBase Base16 $
                                        Cardano.serialiseToCBOR tx'
                                    rejectFilePath
                                        = $(getTestData)
                                        </> "plutus"
                                        </> filepath <> ".rej"
                                BS.writeFile rejectFilePath newEncoding
                                expectationFailure $ mconcat
                                    [ "Transaction encoding has changed, "
                                    , "making comparison impossible. "
                                    , "See .rej file: "
                                    , rejectFilePath
                                    ]
                            else
                                Cardano.serialiseToCBOR tx
                                  `shouldBe` Cardano.serialiseToCBOR tx'

            prop ("with TxUpdate: " <> filepath) $
                prop_updateTx anyRecentEraTx

    describe "existing key witnesses" $ do

        signedTxs <- runIO signedTxTestData

        it "returns `Left err` with noTxUpdate" $ do
            -- Could be argued that it should instead return `Right tx`.
            let anyRecentEraTx = recentEraTxFromBytes
                    $ snd $ head signedTxs
            Write.withInAnyRecentEra anyRecentEraTx $ \tx ->
                updateTx tx noTxUpdate
                    `shouldBe` Left (ErrExistingKeyWitnesses 1)

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

prop_updateTx
    :: Write.InAnyRecentEra Cardano.Tx
    -> [(TxIn, TxOut)]
    -> [TxIn]
    -> [TxOut]
    -> Coin
    -> Property
prop_updateTx
    (Write.InAnyRecentEra _era tx)
    extraIns extraCol extraOuts newFee =
    do
        let extra = TxUpdate extraIns extraCol extraOuts [] (UseNewTxFee newFee)
        let tx' = either (error . show) id
                $ updateTx tx extra
        conjoin
            [ inputs tx' === inputs tx <> Set.fromList (fst <$> extraIns)
            , outputs tx' === outputs tx <> Set.fromList extraOuts
            , sealedFee tx' === Just newFee
            , collateralIns tx' === collateralIns tx <> Set.fromList extraCol
            ]
  where
    inputs = sealedInputs . sealedTxFromCardano'
    outputs = sealedOutputs . sealedTxFromCardano'
    collateralIns = sealedCollateralInputs . sealedTxFromCardano'

estimateSignedTxSizeSpec :: Spec
estimateSignedTxSizeSpec = describe "estimateSignedTxSize" $ do
    txBinaries <- runIO signedTxTestData
    describe "equals the binary size of signed txs" $
        forAllGoldens txBinaries test
  where
    test
        :: forall era. Write.IsRecentEra era
        => String
        -> ByteString
        -> Cardano.Tx era
        -> IO ()
    test _name bs cTx@(Cardano.Tx body _) = do
        let pparams = Write.pparamsLedger $ mockPParamsForBalancing @era
            witCount dummyAddr = estimateKeyWitnessCount
                (Write.fromCardanoUTxO
                    $ utxoPromisingInputsHaveAddress dummyAddr body)
                body
            era = recentEra @era

            tx :: Write.Tx (Write.ShelleyLedgerEra era)
            tx = Write.fromCardanoTx @era cTx

            noScripts = Write.withConstraints (recentEra @era) $
                Map.null $ tx ^. witsTxL . scriptTxWitsL
            noBootWits = Write.withConstraints (recentEra @era) $
                Set.null $ tx ^. witsTxL . bootAddrTxWitsL
            testDoesNotYetSupport x =
                pendingWith $ "Test setup does not work for txs with " <> x

            signedBinarySize = TxSize $ fromIntegral $ BS.length bs

        case (noScripts, noBootWits) of
                (True, True) -> do
                    estimateSignedTxSize era pparams (witCount vkCredAddr) tx
                        `shouldBeInclusivelyWithin`
                        ( signedBinarySize - correction
                        , signedBinarySize
                        )
                (False, False) ->
                    testDoesNotYetSupport "bootstrap wits + scripts"
                (True, False) ->
                    estimateSignedTxSize era pparams (witCount bootAddr) tx
                        `shouldBeInclusivelyWithin`
                        ( signedBinarySize - correction
                        , signedBinarySize + bootWitsCanBeLongerBy
                        )
                (False, True) -> testDoesNotYetSupport "scripts"
      where
        -- Apparently the cbor encoding used by the ledger for size checks
        -- (`toCBORForSizeComputation`) is a few bytes smaller than the actual
        -- serialized size for these goldens.
        correction = TxSize 6

    -- | Checks for membership in the given closed interval [a, b]
    x `shouldBeInclusivelyWithin` (a, b) =
        if a <= x && x <= b
        then pure ()
        else expectationFailure $ unwords
            [ show x
            , "not in the expected interval"
            , "[" <> show a <> ", " <> show b <> "]"
            ]

    forAllGoldens
        :: [(String, ByteString)]
        -> (forall era. Write.IsRecentEra era
                => String
                -> ByteString
                -> Cardano.Tx era
                -> IO ())
        -> Spec
    forAllGoldens goldens f = forM_ goldens $ \(name, bs) -> it name $
        Write.withInAnyRecentEra (recentEraTxFromBytes bs) $ \tx ->
            let
                msg = unlines
                    [ B8.unpack $ hex bs
                    , pretty
                        $ sealedTxFromCardano
                        $ InAnyCardanoEra cardanoEra tx
                    ]
            in
                Hspec.counterexample msg $ f name bs tx

    -- estimateSignedTxSize now depends upon being able to resolve inputs. To
    -- keep tese tests working, we can create a UTxO with dummy values as long
    -- as estimateSignedTxSize can tell that all inputs in the tx correspond to
    -- outputs with vk payment credentials.
    utxoPromisingInputsHaveAddress
        :: forall era. (HasCallStack, Cardano.IsShelleyBasedEra era)
        => Address
        -> Cardano.TxBody era
        -> Cardano.UTxO era
    utxoPromisingInputsHaveAddress addr (Cardano.TxBody body) =
        Cardano.UTxO $ Map.fromList $
            [ (i
               , Compatibility.toCardanoTxOut
                     (shelleyBasedEra @era)
                     Nothing
                     (TxOut addr mempty)
               )
            | i <- allTxIns body
            ]

      where
        allTxIns b = col ++ map fst ins
          where
            col = case Cardano.txInsCollateral b of
                Cardano.TxInsCollateral _ c -> c
                Cardano.TxInsCollateralNone -> []
            ins = Cardano.txIns body

    -- An address with a vk payment credential. For the test above, this is the
    -- only aspect which matters.
    vkCredAddr = Address $ unsafeFromHex
        "6000000000000000000000000000000000000000000000000000000000"

    -- This is a short bootstrap address retrieved from
    -- "byron-address-format.md".
    bootAddr = Address $ unsafeFromHex
        "82d818582183581cba970ad36654d8dd8f74274b733452ddeab9a62a397746be3c42ccdda0001a9026da5b"

    -- With more attributes, the address can be longer. This value was chosen
    -- /experimentally/ to make the tests pass. The ledger has been validating
    -- new outputs with bootstrap addresses have attributes not larger than 64
    -- bytes. The ledger has done so since the middle of the Byron era.
    -- Address attributes are included in the bootstrap witnesses.
    --
    -- NOTE: If we had access to the real UTxO set for the inputs of the test
    -- txs, we wouldn't need this fuzziness. Related: ADP-2987.
    bootWitsCanBeLongerBy = TxSize 45

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

sealedFee
    :: forall era. Cardano.IsCardanoEra era => Cardano.Tx era -> Maybe Coin
sealedFee =
    view #fee
    . fst6
    . _decodeSealedTx maxBound (ShelleyWalletCtx dummyPolicyK)
    . sealedTxFromCardano'

-- | A collection of signed transaction bytestrings useful for testing.
--
-- These bytestrings can be regenerated by running the integration tests
-- with lib/wallet/test/data/signedTxs/genData.patch applied.
--
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

recentEraTxFromBytes :: ByteString -> Write.InAnyRecentEra Cardano.Tx
recentEraTxFromBytes bytes =
    let
        anyEraTx
            = cardanoTx
            $ either (error . show) id
            $ sealedTxFromBytes bytes
    in
        case Write.asAnyRecentEra anyEraTx of
            Just recentEraTx -> recentEraTx
            Nothing -> error "recentEraTxFromBytes: older eras not supported"

cardanoTx :: SealedTx -> InAnyCardanoEra Cardano.Tx
cardanoTx = cardanoTxIdeallyNoLaterThan maxBound
