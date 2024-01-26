{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet
    ( ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , LocalTxSubmissionConfig (..)
    , SelectionWithoutChange
    , WalletLayer (..)
    , migrationPlanToSelectionWithdrawals
    , submitTx
    , throttle
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , Role (..)
    , deriveAccountPrivateKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    , generateKeyFromSeed
    )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Address.States.Features
    ( TestFeatures (..)
    , defaultTestFeatures
    )
import Cardano.Wallet.Address.States.Test.State
    ( TestState (..)
    )
import Cardano.Wallet.Balance.Migration.SelectionSpec
    ( MockTxConstraints (..)
    , genTokenBundleMixed
    , unMockTxConstraints
    )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , hoistDBLayer
    , putTxHistory
    )
import Cardano.Wallet.DB.Fixtures
    ( logScale'
    )
import Cardano.Wallet.DB.Layer
    ( newBootDBLayerInMemory
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( TxSubmissionsStatus
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0
    , dummyNetworkLayer
    , dummyNetworkParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    )
import Cardano.Wallet.Flavor
    ( CredFromOf
    , KeyOf
    , WalletFlavorS (TestStateS)
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    , genSlotNo
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (Mainnet)
    )
import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (..)
    , Passphrase (..)
    )
import Cardano.Wallet.Primitive.Passphrase.Current
    ( preparePassphrase
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (BlockHeader)
    , NetworkParameters (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress
    )
import Cardano.Wallet.Primitive.Types.AssetId.Gen
    ( genAssetIdLargeRange
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (TokenBundle)
    , getAssets
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityPositive
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , Tx (..)
    , mockSealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx
    , shrinkTx
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxInLargeRange
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxMeta (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..)
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..)
    , Withdrawal (..)
    , emptyTokenMapWithScripts
    , emptyWitnessCount
    )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Cardano.Write.Tx
    ( ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Monad
    ( forM_
    , guard
    , replicateM
    , void
    )
import Control.Monad.Class.MonadTime
    ( MonadMonotonicTimeNSec (..)
    , MonadTime (..)
    , UTCTime
    )
import Control.Monad.Class.MonadTime.SI
    ( MonadMonotonicTime
    , Time (..)
    , addTime
    , diffTime
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..)
    , MonadUnliftIO (..)
    , wrappedWithRunInIO
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , except
    , runExceptT
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..)
    )
import Control.Monad.Trans.Reader
    ( ReaderT (..)
    , ask
    )
import Control.Monad.Trans.State.Strict
    ( State
    , StateT (..)
    , evalState
    , get
    , put
    , state
    )
import Control.Tracer
    ( natTracer
    , nullTracer
    )
import Crypto.Hash
    ( hash
    )
import Data.ByteString
    ( ByteString
    )
import Data.Coerce
    ( coerce
    )
import Data.Function
    ( on
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Generics.Internal.VL
    ( iso
    , view
    , (^.)
    )
import Data.List
    ( nubBy
    , sort
    , sortOn
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( catMaybes
    , fromMaybe
    , isJust
    , isNothing
    )
import Data.Ord
    ( Down (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( DiffTime
    , diffTimeToPicoseconds
    , picosecondsToDiffTime
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Internal.Cardano.Write.Tx.SizeEstimation
    ( TxWitnessTag (..)
    )
import System.Random
    ( Random
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    , xit
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , NonEmptyList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitrarySizedFractional
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllBlind
    , frequency
    , label
    , liftArbitrary
    , listOf1
    , oneof
    , property
    , scale
    , sized
    , suchThat
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Extra
    ( report
    )
import Test.QuickCheck.Monadic
    ( PropertyM
    , assert
    , monadicIO
    , monitor
    , run
    )
import Test.Utils.Time
    ( UniformTime
    )
import Test.Utils.Trace
    ( captureLogging'
    )
import UnliftIO.Concurrent
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , threadDelay
    )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Address.Book as Sqlite
import qualified Cardano.Wallet.Balance.Migration as Migration
import qualified Cardano.Wallet.DB.Sqlite.Types as DB
import qualified Cardano.Wallet.DB.Store.Checkpoints.Store as Sqlite
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Submissions.Submissions as Smbs
import qualified Cardano.Wallet.Submissions.TxStatus as Sbms
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Internal.Cardano.Write.Tx as Write

spec :: Spec
spec = describe "Cardano.WalletSpec" $ do
    describe "Pointless mockEventSource to cover 'Show' instances for errors" $ do
        let wid = WalletId (hash @ByteString "arbitrary")
        it (show $ ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase) True

    describe "WalletLayer works as expected" $ do
        it "Two wallets with same mnemonic have a same public id"
            (property walletIdDeterministic)
        it "Two wallets with different mnemonic have a different public id"
            (property walletIdInjective)
        it "Wallet has name corresponding to its last update"
            (property walletUpdateName)
        it "Can change passphrase of the last private key attached, if any"
            (property walletUpdatePassphrase)
        it "Can't change passphrase with a wrong old passphrase"
            (property walletUpdatePassphraseWrong)
        it "Can't change passphrase if wallet doesn't exist"
            (property walletUpdatePassphraseNoRootKey)
        it "Passphrase info is up-to-date after wallet passphrase update"
            (property walletUpdatePassphraseDate)
        -- fixme: [ADP-1132] Rework property for new transactions code.
        xit "Root key is re-encrypted with new passphrase"
            (withMaxSuccess 10 $ property walletKeyIsReencrypted)
        it "Wallet can list transactions with sorting"
            (property walletListTransactionsSorted)
        it "Wallet can list transactions wih a limited number of results"
            (property walletListTransactionsWithLimit)
        it "Wallet won't list unrelated assets used in related transactions"
            (property walletListsOnlyRelatedAssets)

    describe "Tx fee estimation spread" $
        it "Estimated fee spreads are sound"
            (property prop_calculateFeePercentiles)

    describe "LocalTxSubmission" $ do
        it "LocalTxSubmission pool retries pending transactions"
            (property prop_localTxSubmission)
        it "LocalTxSubmission updates are limited in frequency"
            (property prop_throttle)

    describe "Migration" $ do
        describe "migrationPlanToSelectionWithdrawals" $ do
            it "Target addresses are cycled correctly." $
                property prop_migrationPlanToSelectionWithdrawals_addresses
            it "Inputs and outputs are preserved in the correct order." $
                property prop_migrationPlanToSelectionWithdrawals_io

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

walletIdDeterministic
    :: (WalletId, WalletName, DummyState)
    -> Property
walletIdDeterministic newWallet = monadicIO $ do
    WalletLayerFixture _ _ widsA <- run $ setupFixture dummyStateF newWallet
    WalletLayerFixture _ _ widsB <- run $ setupFixture dummyStateF newWallet
    assert (widsA == widsB)

walletIdInjective
    :: ((WalletId, WalletName, DummyState), (WalletId, WalletName, DummyState))
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ do
    WalletLayerFixture _ _ widsA <- run $ setupFixture dummyStateF walletA
    WalletLayerFixture _ _ widsB <- run $ setupFixture dummyStateF walletB
    assert (widsA /= widsB)

walletUpdateName
    :: (WalletId, WalletName, DummyState)
    -> WalletName
    -> Property
walletUpdateName wallet wName = monadicIO $ do
    wName' <- run $ do
        WalletLayerFixture _ wl _ <- setupFixture dummyStateF wallet
        W.updateWallet wl (\x -> x { name = wName })
        (name . (\(_, (b, _), _) -> b)) <$> W.readWallet wl
    assert (wName == wName')

walletUpdatePassphrase
    :: (WalletId, WalletName, DummyState)
    -> Passphrase "user"
    -> Maybe (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> Property
walletUpdatePassphrase wallet new mxprv = monadicIO $ do
    WalletLayerFixture _ wl wid <- run $ setupFixture dummyStateF wallet
    case mxprv of
        Nothing -> prop_withoutPrivateKey wl wid
        Just (xprv, pwd) -> prop_withPrivateKey wl wid (xprv, pwd)
  where
    prop_withoutPrivateKey wl wid = do
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase dummyStateF wl wid (new, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)
    prop_withPrivateKey wl wid (xprv, pwd) = do
        run $ W.attachPrivateKeyFromPwd wl (xprv, pwd)
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase dummyStateF
                wl wid (coerce pwd, new)
        assert (attempt == Right ())

walletUpdatePassphraseWrong
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> (Passphrase "user", Passphrase "user")
    -> Property
walletUpdatePassphraseWrong wallet (xprv, pwd) (old, new) =
    pwd /= coerce old ==> monadicIO $ do
        WalletLayerFixture _ wl wid <- run $ setupFixture dummyStateF wallet
        attempt <- run $ do
            W.attachPrivateKeyFromPwd wl (xprv, pwd)
            runExceptT
                $ W.updateWalletPassphraseWithOldPassphrase
                    dummyStateF wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey
                $ ErrWithRootKeyWrongPassphrase wid
                ErrWrongPassphrase
        assert (attempt == Left err)

walletUpdatePassphraseNoRootKey
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> (Passphrase "user", Passphrase "user")
    -> Property
walletUpdatePassphraseNoRootKey wallet@(wid', _, _) wid (old, new) =
    wid /= wid' ==> monadicIO $ do
        WalletLayerFixture _ wl _ <- run $ setupFixture dummyStateF wallet
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase
                dummyStateF wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)

walletUpdatePassphraseDate
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> Property
walletUpdatePassphraseDate wallet (xprv, pwd) = monadicIO $ liftIO $ do
    WalletLayerFixture _ wl wid  <- liftIO $ setupFixture dummyStateF wallet
    let infoShouldSatisfy predicate = do
            info <- (passphraseInfo . (\(_, (b, _), _) -> b))
                <$> W.readWallet wl
            info `shouldSatisfy` predicate
            return info

    void $ infoShouldSatisfy isNothing
    W.attachPrivateKeyFromPwd wl (xprv, pwd)
    info <- infoShouldSatisfy isJust
    pause
    unsafeRunExceptT
        $ W.updateWalletPassphraseWithOldPassphrase
            dummyStateF wl wid (coerce pwd, coerce pwd)
    void $ infoShouldSatisfy (\info' -> isJust info' && info' > info)
  where
    pause = threadDelay 500

walletKeyIsReencrypted
    :: (WalletId, WalletName)
    -> (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> Passphrase "user"
    -> Property
walletKeyIsReencrypted (_wid, _wname) (_xprv, _pwd) _newPwd = property True

walletListTransactionsSorted
    :: (WalletId, WalletName, DummyState)
    -> SortOrder
    -> (Maybe UniformTime, Maybe UniformTime)
    -> Property
walletListTransactionsSorted wallet@(_, _, _) _order (_mstart, _mend) =
    forAll (logScale' 1.5 arbitrary) $ \history ->
    monadicIO $ liftIO $ do
        WalletLayerFixture DBLayer{..} wl _ <- setupFixture dummyStateF wallet
        atomically $ putTxHistory history
        txs <- unsafeRunExceptT $
            W.listTransactions wl Nothing Nothing Nothing
                Descending Nothing Nothing
        length txs `shouldBe` L.length history
        -- With the 'Down'-wrapper, the sort is descending.
        txs `shouldBe` L.sortOn (Down . slotNo . txInfoMeta) txs
        -- Check transaction time calculation
        let times = Map.fromList [(txInfoId i, txInfoTime i) | i <- txs]
        let expTimes = Map.fromList
                [ (tx ^. #txId, slotNoTime (meta ^. #slotNo))
                | (tx, meta) <- history ]
        times `shouldBe` expTimes

genLimit :: (Random a, Integral a) => a -> Gen a
genLimit n =
    frequency
        [ (1, pure 0)
        , (5, choose (1, n `div` 5))
        , (3, choose (n `div` 5, n * 3 `div` 2))
        ]

walletListTransactionsWithLimit
    :: (WalletId, WalletName, DummyState)
    -> Property
walletListTransactionsWithLimit wallet@(_, _, _) =
    forAll (logScale' 1.5 arbitrary) $ \history ->
    forAll (genLimit $ length history) $ \limit ->
    -- collect (length history) $
    cover 50 (limit < length history) "limit is limiting full query" $
    let history'
            = nubBy ((==) `on` \(_,meta) -> meta ^. #slotNo)
            $ nubBy ((==) `on` \(tx,_) -> tx ^. #txId) history
        test :: Ord (f UTCTime)
            => Maybe UTCTime
            -> Maybe UTCTime
            -> SortOrder
            -> (UTCTime -> f UTCTime)
            -> (UTCTime -> Bool)
            -> PropertyM IO ()
        limitNatural = fromIntegral limit
        test start stop order dir cut = liftIO @(PropertyM IO) $ do
            WalletLayerFixture DBLayer{..} wl _ <- setupFixture dummyStateF wallet
            atomically $ putTxHistory history'
            txs <- unsafeRunExceptT $
                W.listTransactions wl Nothing start stop order
                    (Just limitNatural) Nothing
            let times = [(txInfoId i, txInfoTime i) | i <- txs]
            shouldBe times $ take limit $ sortOn (dir . snd) $ do
                (tx, meta) <- history'
                let slot = slotNoTime (meta ^. #slotNo)
                guard $ cut slot
                pure  (tx ^. #txId, slot )
        pointInRange xs f = slotNoTime
                    $ SlotNo
                    $ f $ unSlotNo (last xs) - unSlotNo (head xs)

    in
    monadicIO $ do
        test Nothing Nothing Descending Down $ const True
        test Nothing Nothing Ascending Identity $ const True
        case sort [ meta ^. #slotNo | (_, meta) <- history' ] of
            [] -> pure ()
            xs -> do
                let h = pointInRange xs (`div` 2)

                test (Just h) Nothing Descending Down (>= h)
                test (Just h) Nothing Ascending Identity (>= h)
                test Nothing (Just h) Descending Down (<= h)
                test Nothing (Just h) Ascending Identity (<= h)

                let l = pointInRange xs  (`div` 3)
                let r = pointInRange xs  ((`div` 3) . (*2))

                test (Just l) (Just r) Descending Down
                    $ \slot -> slot >= l && slot <= r
                test (Just l) (Just r) Ascending Identity
                    $ \slot -> slot >= l && slot <= r

type DummyStateWithAddresses = TestState [Address] 'Mainnet ShelleyKey 'CredFromKeyK

dummyStateWithAddressesF :: WalletFlavorS DummyStateWithAddresses
dummyStateWithAddressesF = TestStateS defaultTestFeatures

instance IsOurs DummyStateWithAddresses Address where
    isOurs a s@(TestState addr) =
        if a `elem` addr
            then (Just (DerivationIndex 0 :| []), s)
            else (Nothing, s)

instance IsOurs DummyStateWithAddresses RewardAccount where
    isOurs _ s = (Nothing, s)

instance Sqlite.AddressBookIso DummyStateWithAddresses where
    data Prologue DummyStateWithAddresses = DummyWithAddressPrologue
    data Discoveries DummyStateWithAddresses
        = DummyWithAddressDiscoveries DummyStateWithAddresses
    addressIso = iso from to
      where
        from x = (DummyWithAddressPrologue, DummyWithAddressDiscoveries x)
        to (_,DummyWithAddressDiscoveries x) = x

-- instance Eq
instance Eq (Sqlite.Prologue DummyStateWithAddresses) where _ == _ = True
instance Eq (Sqlite.Discoveries DummyStateWithAddresses) where
    DummyWithAddressDiscoveries a == DummyWithAddressDiscoveries b = a == b

instance Sqlite.PersistAddressBook DummyStateWithAddresses where
    insertPrologue _ _ = pure ()
    insertDiscoveries _ _ _ = pure ()
    loadPrologue _ = error "DummyStateWithAddresses.loadPrologue: not implemented"
    loadDiscoveries _ _ = error "DummyStateWithAddresses.loadDiscoveries: not implemented"

walletListsOnlyRelatedAssets :: Hash "Tx" -> TxMeta -> Property
walletListsOnlyRelatedAssets txId txMeta =
    forAll genOuts $ \(out1, out2, wallet) -> monadicIO $ do
        WalletLayerFixture DBLayer{..} wl _ <- liftIO
            $ setupFixture dummyStateWithAddressesF wallet
        let listHistoricalAssets hry = do
                liftIO . atomically $ putTxHistory hry
                liftIO $ W.listAssets wl
        let tx = Tx
                { txId
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs = mempty
                , resolvedCollateralInputs = mempty
                , outputs = [out1, out2]
                , collateralOutput = Nothing
                , metadata = mempty
                , withdrawals = mempty
                , scriptValidity = Nothing
                }
        assets <- listHistoricalAssets [ (tx, txMeta) ]
        monitor $ report out1 "Output with related address"
        monitor $ report out2 "Output with unrelated address"
        monitor $ report assets "Discovered assets"
        assert $ assets == getAssets (out1 ^. #tokens)
  where
    genOuts ::
        Gen (TxOut, TxOut, (WalletId, WalletName, DummyStateWithAddresses))
    genOuts = do
        relatedAddress <- genAddress
        unrelatedAddress <- genAddress `suchThat` (/= relatedAddress)
        coin <- genCoinPositive
        let bundle aid = TokenBundle coin . TokenMap.singleton aid <$>
                genTokenQuantityPositive
        tokenBundle1 <- genAssetIdLargeRange >>= bundle
        tokenBundle2 <- genAssetIdLargeRange >>= bundle
        wId <- arbitrary
        wName <- arbitrary
        pure ( TxOut { tokens = tokenBundle1, address = relatedAddress }
             , TxOut { tokens = tokenBundle2, address = unrelatedAddress }
             , (wId, wName, TestState [relatedAddress])
             )

{-------------------------------------------------------------------------------
                        Properties of tx fee estimation
-------------------------------------------------------------------------------}

-- | Properties of 'estimateFeeForCoinSelection':
-- 1. There is no coin selection with a fee above the estimated maximum.
-- 2. The minimum estimated fee is no greater than the maximum estimated fee.
-- 3. Around 10% of fees are below the estimated minimum.
prop_calculateFeePercentiles
    :: Write.AnyRecentEra
    -> NonEmptyList (Maybe Coin)
    -> Property
prop_calculateFeePercentiles
    (Write.AnyRecentEra (_era :: Write.RecentEra era))
    (NonEmpty coins) =
    case evalState (runExceptT $ W.calculateFeePercentiles estimateFee) 0 of
        Left (err :: ErrBalanceTx era) ->
            label "errors: all" $ err === (genericError :: ErrBalanceTx era)

        Right percentiles@(W.Percentile minFee, W.Percentile maxFee) ->
            label ("errors: " <> if any isNothing coins then "some" else "none") $
            counterexample (show percentiles) $ conjoin
                [ property $ W.feeToCoin maxFee <= maximum (catMaybes coins)
                , property $ minFee <= maxFee
                , proportionBelow (W.feeToCoin minFee) coins
                    `closeTo` (1/10 :: Double)
                ]
  where
    genericError :: ErrBalanceTx era
    genericError
        = ErrBalanceTxAssetsInsufficient
        $ ErrBalanceTxAssetsInsufficientError
            mempty
            mempty
            mempty

    estimateFee :: ExceptT (ErrBalanceTx era) (State Int) W.Fee
    estimateFee = do
        i <- lift get
        lift $ put $ (i + 1) `mod` length coins
        case (coins !! i) of
            Nothing -> except $ Left genericError
            Just c  -> except $ Right $ W.Fee c

    proportionBelow :: Coin -> [Maybe Coin] -> Double
    proportionBelow minFee xs =
        fromIntegral (countBelow minFee xs) / fromIntegral (count isJust xs)
      where
        count :: (a -> Bool) -> [a] -> Int
        count p = length . filter p

        -- Find the number of results below the "minimum" estimate.
        countBelow :: Coin -> [Maybe Coin] -> Int
        countBelow sup =
            count ((< sup) . fromMaybe txOutMaxCoin)

    -- Two fractions are close to each other if they are within 20% either way.
    closeTo a b =
        counterexample (show a <> " & " <> show b <> " are not close enough") $
        property $ abs (a - b) < (1/5)

instance Arbitrary Write.AnyRecentEra where
    arbitrary = elements
        [ Write.AnyRecentEra Write.RecentEraBabbage
        , Write.AnyRecentEra Write.RecentEraConway
        ]

{-------------------------------------------------------------------------------
                               LocalTxSubmission
-------------------------------------------------------------------------------}

data TxRetryTest = TxRetryTest
    { retryTestPool :: [BuiltTx]
    , postTxResults :: [(SealedTx, Bool)]
    , testSlottingParameters :: SlottingParameters
    , retryTestWallet :: (WalletId, WalletName, DummyState)
    } deriving (Generic, Show, Eq)

numSlots :: TxRetryTest -> Word64
numSlots = const 100
newtype GenSubmissions = GenSubmissions { genSubmissions :: [BuiltTx] }
    deriving (Generic, Show, Eq)

instance Arbitrary GenSubmissions where
    arbitrary = GenSubmissions <$> logScale' 2.7 (listOf1 genBuiltTx)
      where
        mkTx txId = Tx
            { txId
            , txCBOR = Nothing
            , fee = Nothing
            , resolvedInputs = []
            , resolvedCollateralInputs = []
            , outputs = []
            , collateralOutput = Nothing
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
        genBuiltTx = do
            sl <- genSmallSlot
            let bh = Quantity $ fromIntegral $ unSlotNo sl
            expry <- oneof [fmap (Just . (+ sl)) genSmallSlot, pure Nothing]
            i <- arbitrary
            pure $ BuiltTx
                (mkTx i)
                (TxMeta Pending Outgoing sl bh (Coin 0) expry)
                (fakeSealedTx (i, []))
        genSmallSlot =
            SlotNo . fromIntegral <$> sized (\n -> choose (1, 1+ 4 * n))

instance Arbitrary TxRetryTest where
    arbitrary = do
        GenSubmissions metas <- arbitrary
        let results = fmap (, True) (builtSealedTx <$> metas)
        TxRetryTest metas results <$> arbitrary <*> arbitrary

instance Arbitrary SlottingParameters where
    arbitrary = mk <$> choose (0.5, 1)
      where
        mk f = dummySlottingParameters
            { getActiveSlotCoefficient = ActiveSlotCoefficient f }

-- | 'WalletLayer' context.
type TxRetryTestCtx = WalletLayer TxRetryTestM DummyState

-- | Context of 'TxRetryTestM'.
data TxRetryTestState = TxRetryTestState
    { testCase :: TxRetryTest
    , timeStep :: DiffTime
    , timeVar :: MVar Time
    } deriving (Generic)

-- | Collected info from test execution.
data TxRetryTestResult a = TxRetryTestResult
    { resLogs :: [W.WalletWorkerLog]
    , resAction :: a
    , resSubmittedTxs :: [SealedTx]
    } deriving (Generic, Show, Eq)

-- | The test runs in this monad so that time can be mocked out.
newtype TxRetryTestM a = TxRetryTestM
    { unTxRetryTestM :: ReaderT TxRetryTestState IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance MonadUnliftIO TxRetryTestM where
    withRunInIO = wrappedWithRunInIO TxRetryTestM unTxRetryTestM

{- instance MonadMonotonicTime TxRetryTestM where
    getMonotonicTime = do
        st <- TxRetryTestM ask
        modifyMVar (timeVar st) $ \t -> do
            let t' = addTime (timeStep st) t
            pure (t', t') -}

instance MonadMonotonicTime TxRetryTestM
instance MonadMonotonicTimeNSec TxRetryTestM where
    getMonotonicTimeNSec = do
        TxRetryTestState {timeVar, timeStep} <- TxRetryTestM ask
        modifyMVar timeVar $ \t -> let t' = timeStep + coerce t
            in pure (coerce t' , timeToNanoTime $ coerce t')

timeToNanoTime :: Time -> Word64
timeToNanoTime = fromIntegral . (`div` 1_000)
    . diffTimeToPicoseconds . coerce

instance MonadTime TxRetryTestM where
    getCurrentTime = liftIO getCurrentTime

prop_localTxSubmission :: TxRetryTest -> Property
prop_localTxSubmission tc = monadicIO $ do
    st <- TxRetryTestState tc 2 <$> newMVar (Time 0)
    assert $ not $ null $ retryTestPool tc
    res <- run $ runTest st
        $ \ctx@(WalletLayer tr _ nl _ db) -> do
        unsafeRunExceptT
            $ forM_ (retryTestPool tc) $ submitTx tr db nl
        res0 <- W.readLocalTxSubmissionPending ctx
        -- Run test
        let cfg = LocalTxSubmissionConfig (timeStep st) 10
        W.runLocalTxSubmissionPool cfg ctx

        -- Gather state
        res1 <- W.readLocalTxSubmissionPending ctx
        pure (res0, res1)

    let (resStart, resEnd) = resAction res
    monitor $ counterexample $ unlines $
        [ "posted txs = " ++ show (resSubmittedTxs res)
        , "final pool state = " ++ show resEnd
        , "logs:"
        ] ++ map (T.unpack . toText) (resLogs res)
    -- props:
    --  1. pending transactions in pool are retried
    let requested x = x `elem` (builtSealedTx <$> retryTestPool tc)
    assert' "all txs in submissions pool were required"
        $ all requested $ resSubmittedTxs res

    let inPool BuiltTx{builtTx} = (Just . DB.TxId $ builtTx ^. #txId)  `elem`
            fmap (fmap fst . Sbms.getTx . view Smbs.txStatus) resEnd

    assert' "all required txs are in submissions pool"
        $ all inPool $ retryTestPool tc

    assert' "start submissions pool is not empty"
        $ not $ null resStart

    assert' "end submissions pool has all txs of start pool"
        $ resStart `eqByTxIds` resEnd
  where
    assert' :: String -> Bool -> PropertyM IO ()
    assert' _msg True  = return ()
    assert' msg False = fail msg
    eqByTxIds
        :: [TxSubmissionsStatus]
        -> [TxSubmissionsStatus] -> Bool
    eqByTxIds = (==) `on` (sort . fmap (fmap fst . Sbms.getTx . view Smbs.txStatus))
    runTest
        :: TxRetryTestState
        -> (TxRetryTestCtx -> TxRetryTestM a)
        -> IO (TxRetryTestResult a)
    runTest st testAction = do
        submittedVar <- newMVar []
        (msgs, res) <- captureLogging' $ \tr -> do
            flip runReaderT st $ unTxRetryTestM $ do
                WalletLayerFixture db _wl _wid <-
                    setupFixture dummyStateF $ retryTestWallet tc
                let ctx = WalletLayer
                        (natTracer liftIO tr)
                        undefined
                        (mockNetwork submittedVar)
                        undefined
                        db
                testAction ctx
        TxRetryTestResult msgs res <$> readMVar submittedVar

    mockNetwork :: MVar [SealedTx] -> NetworkLayer TxRetryTestM Read.ConsensusBlock
    mockNetwork var = dummyNetworkLayer
        { currentSlottingParameters = pure (testSlottingParameters tc)
        , postTx = \tx -> ExceptT $ do
                stash var tx
                pure $ case lookup tx (postTxResults tc) of
                    Just True -> Right ()
                    Just False -> Left (W.ErrPostTxValidationError "intended")
                    Nothing -> Left (W.ErrPostTxValidationError "unexpected")
        , watchNodeTip = mockNodeTip (numSlots tc) 0
        }

    mockNodeTip end sl cb
        | sl < end = do
            let h = Hash ""
            void $ cb $ BlockHeader (SlotNo sl) (Quantity (fromIntegral sl)) h (Just h)
            mockNodeTip end (sl + 1) cb
        | otherwise = pure ()

    stash :: MVar [a] -> a -> TxRetryTestM ()
    stash var x = modifyMVar_ var (\xs -> pure (x:xs))

{-------------------------------------------------------------------------------
                            'throttle' Util Function
-------------------------------------------------------------------------------}

data ThrottleTest = ThrottleTest
    { interval :: DiffTime
        -- ^ Interval parameter provided to 'throttle'
    , diffTimes :: [DiffTime]
        -- ^ Times when throttled function is called.
    } deriving (Generic, Show, Eq)

instance Arbitrary ThrottleTest where
    arbitrary = ThrottleTest <$> genInterval <*> listOf1 genDiffTime
      where
        genInterval = genDiffTime `suchThat` (> 0)
        -- need to discard picoseconds to match `Time` precision (nanoseconds)
        genDiffTime
            = picosecondsToDiffTime
            . (* 1_000)
            . (`div` 1_000)
            . diffTimeToPicoseconds
            . abs
            <$> arbitrarySizedFractional
    shrink (ThrottleTest i dts) =
        [ ThrottleTest (fromRational i') (map fromRational dts')
        | (i', dts') <- shrink (toRational i, map toRational dts)
        , i' > 0, not (null dts') ]

data ThrottleTestState = ThrottleTestState
    { remainingDiffTimes :: [DiffTime]
    , now :: Time
    , actions :: [(Time, Int)]
    } deriving (Generic, Show, Eq)

newtype ThrottleTestT m a = ThrottleTestT
    { unThrottleTestT :: MaybeT (StateT ThrottleTestState m) a
    } deriving (Functor, Applicative, Monad, MonadIO)

runThrottleTest
    :: MonadIO m
    => ThrottleTestT m a
    -> ThrottleTestState
    -> m (Maybe a, ThrottleTestState)
runThrottleTest action = fmap r . runStateT (runMaybeT (unThrottleTestT action))
  where
    r (res, ThrottleTestState d n a) = (res, ThrottleTestState d n (reverse a))

initState :: ThrottleTest -> ThrottleTestState
initState (ThrottleTest _ dts) = ThrottleTestState dts (Time 0) []

recordTime :: Monad m => (Time, Int) -> ThrottleTestT m ()
recordTime x = ThrottleTestT $ lift $ state $
    \(ThrottleTestState ts now xs) -> ((), ThrottleTestState ts now (x:xs))

instance MonadMonotonicTime m => MonadMonotonicTime (ThrottleTestT m) where

instance MonadMonotonicTimeNSec m => MonadMonotonicTimeNSec (ThrottleTestT m) where
    getMonotonicTimeNSec = ThrottleTestT $ MaybeT $ state mockTime
      where
        mockTime :: ThrottleTestState -> (Maybe Word64, ThrottleTestState)
        mockTime (ThrottleTestState later now as) = case later of
            [] -> (Nothing, ThrottleTestState later now as)
            (t:ts) ->
                let now' = addTime t now
                in  ( Just $ timeToNanoTime now'
                    , ThrottleTestState ts now' as
                    )

instance MonadUnliftIO m => MonadUnliftIO (StateT ThrottleTestState m) where
  withRunInIO inner = StateT $ \tts -> do
      -- smuggle the test state in an mvar
      var <- newEmptyMVar
      withRunInIO $ \io -> do
          a <- inner $ \action -> do
              (a, tts') <- io $ runStateT action tts
              putMVar var tts'
              pure a
          tts' <- takeMVar var
          pure (a, tts')

instance MonadUnliftIO m => MonadUnliftIO (ThrottleTestT m) where
    withRunInIO inner = ThrottleTestT $ MaybeT $ fmap Just $
        withRunInIO $ \io -> inner $ \(ThrottleTestT action) ->
            io (runMaybeT action) >>= maybe (error "bad test") pure

-- | 'throttle' ensures than the action runs when called to, at most once per
-- interval.
prop_throttle :: ThrottleTest -> Property
prop_throttle tc@(ThrottleTest interval diffTimes) = monadicIO $ do
    (res, st) <- run $ runThrottleTest testAction (initState tc)
    -- check test case
    monitor coverageTests
    -- info for debugging failed mockEventSource
    monitor $ counterexample $ unlines
        [ ("res      = " ++ show res)
        , ("st       = " ++ show st)
        , ("accTimes = " ++ show accTimes)
        , ("actions  = " ++ show (actions st))
        , ("expected = " ++ show expected)
        , ("timeDeltas= " ++ show (timeDeltas (map fst (actions st))))
        , ("interval = " ++ show interval)
        , ("diffTimes= " ++ show diffTimes)
        ]

    -- sanity-check test runner
    assertNamed "consumed test data" (null $ remainingDiffTimes st)
    assertNamed "expected final time" (now st == finalTime)
    assertNamed "runner success" (isJust res)
    -- properties
    assertNamed "action runs whenever interval has passed" $
        length diffTimes <= 1 || actions st == expected
    assertNamed "action runs at most once per interval" $
        all (>= interval) (timeDeltas (map fst (actions st)))
  where
    testAction :: ThrottleTestT IO ()
    testAction = do
        rateLimited <- throttle interval (curry recordTime)
        mockEventSource rateLimited 0

    mockEventSource cb n
         | n < length diffTimes = cb n >> mockEventSource cb (n + 1)
         | otherwise = pure ()

    finalTime = addTime (sum diffTimes) (Time 0)
    accTimes = drop 1 $ L.scanl' (flip addTime) (Time 0) diffTimes

    expected = reverse $ snd $ L.foldl' model (Time (negate interval), []) $
        zip accTimes [0..]

    model (prev, xs) (now, i)
        | diffTime now prev >= interval = (now, (now, i):xs)
        | otherwise = (prev, xs)

    timeDeltas xs = zipWith diffTime (drop 1 xs) xs

    assertNamed lbl prop = do
        monitor $ counterexample $ lbl ++ ": " ++ show prop
        assert prop

    coverageTests = checkCoverage
        . cover 1 (interval < 1) "sub-second interval"
        . cover 25 (interval >= 1) "super-second interval"
        . cover 25 (length diffTimes >= 10) "long mockEventSource"
        . cover 25 (testRatio >= 0.5 && testRatio <= 1.5) "reasonable interval"
      where
        avgDiffTime = sum diffTimes / fromIntegral (length diffTimes)
        testRatio = avgDiffTime / interval

{-------------------------------------------------------------------------------
                                Migration
-------------------------------------------------------------------------------}

genMigrationTargetAddresses :: Gen (NonEmpty Address)
genMigrationTargetAddresses = do
    addressCount <- choose (1, 8)
    pure $ (:|)
        (mkAddress 'A')
        (mkAddress <$> take (addressCount - 1) ['B' ..])
  where
    mkAddress :: Char -> Address
    mkAddress c = Address $ B8.singleton c

genMigrationUTxO :: MockTxConstraints -> Gen UTxO
genMigrationUTxO mockTxConstraints = do
    entryCount <- choose (1, 128)
    UTxO . Map.fromList <$> replicateM entryCount genUTxOEntry
  where
    genUTxOEntry :: Gen (TxIn, TxOut)
    genUTxOEntry =
        (,)
        <$> genTxInLargeRange
        <*> (TxOut <$> genAddress <*> genTokenBundleMixed mockTxConstraints)

-- Tests that user-specified target addresses are assigned to generated outputs
-- in the correct cyclical order.
--
prop_migrationPlanToSelectionWithdrawals_addresses
    :: Blind MockTxConstraints
    -> Property
prop_migrationPlanToSelectionWithdrawals_addresses (Blind mockTxConstraints) =
    forAllBlind genMigrationTargetAddresses $ \targetAddresses ->
    forAllBlind (genMigrationUTxO mockTxConstraints) $ \utxo ->
    prop_migrationPlanToSelectionWithdrawals_addresses_inner
        mockTxConstraints utxo targetAddresses

prop_migrationPlanToSelectionWithdrawals_addresses_inner
    :: MockTxConstraints
    -> UTxO
    -> NonEmpty Address
    -> Property
prop_migrationPlanToSelectionWithdrawals_addresses_inner
    mockTxConstraints utxo targetAddresses =
        case maybeSelectionWithdrawals of
            Nothing ->
                -- If this case matches, it means the plan is empty:
                length (view #selections plan) === 0
            Just selectionWithdrawals ->
                test (fst <$> selectionWithdrawals)
  where
    test :: NonEmpty SelectionWithoutChange -> Property
    test selections = makeCoverage $ makeReports $
        cycledTargetAddressesActual ==
        cycledTargetAddressesExpected
      where
        cycledTargetAddressesActual = view #address <$>
            (view #outputs =<< NE.toList selections)
        cycledTargetAddressesExpected = NE.take
            (length cycledTargetAddressesActual)
            (NE.cycle targetAddresses)
        totalOutputCount = F.sum (length . view #outputs <$> selections)
        makeCoverage
            = cover 4 (totalOutputCount > length targetAddresses)
                "total output count > target address count"
            . cover 4 (totalOutputCount < length targetAddresses)
                "total output count < target address count"
            . cover 1 (totalOutputCount == length targetAddresses)
                "total output count = target address count"
            . cover 4 (length selections == 1)
                "number of selections = 1"
            . cover 4 (length selections == 2)
                "number of selections = 2"
            . cover 4 (length selections > 2)
                "number of selections > 2"
        makeReports
            = report mockTxConstraints
                "mockTxConstraints"
            . report (NE.toList targetAddresses)
                "targetAddresses"
            . report cycledTargetAddressesExpected
                "cycledTargetAddressesExpected"
            . report cycledTargetAddressesActual
                "cycledTargetAddressesActual"

    constraints = unMockTxConstraints mockTxConstraints
    maybeSelectionWithdrawals =
        migrationPlanToSelectionWithdrawals plan NoWithdrawal targetAddresses
    plan = Migration.createPlan constraints utxo reward
    reward = Migration.RewardWithdrawal (Coin 0)

-- Tests that inputs and outputs are preserved in the correct order.
--
prop_migrationPlanToSelectionWithdrawals_io
    :: Blind MockTxConstraints
    -> Property
prop_migrationPlanToSelectionWithdrawals_io (Blind mockTxConstraints) =
    forAllBlind genMigrationTargetAddresses $ \targetAddresses ->
    forAllBlind (genMigrationUTxO mockTxConstraints) $ \utxo ->
    prop_migrationPlanToSelectionWithdrawals_io_inner
        mockTxConstraints utxo targetAddresses

prop_migrationPlanToSelectionWithdrawals_io_inner
    :: MockTxConstraints
    -> UTxO
    -> NonEmpty Address
    -> Property
prop_migrationPlanToSelectionWithdrawals_io_inner
    mockTxConstraints utxo targetAddresses =
        case maybeSelectionWithdrawals of
            Nothing ->
                -- If this case matches, it means the plan is empty:
                length (view #selections plan) === 0
            Just selectionWithdrawals ->
                test (fst <$> selectionWithdrawals)
  where
    test :: NonEmpty SelectionWithoutChange -> Property
    test selections = makeCoverage $ makeReports $ conjoin
        [ inputsActual == inputsExpected
        , outputsActual == outputsExpected
        ]
      where
        inputsActual :: [[(TxIn, TxOut)]]
        inputsActual =
            NE.toList (NE.toList . view #inputs <$> selections)
        inputsExpected :: [[(TxIn, TxOut)]]
        inputsExpected =
            NE.toList . view #inputIds <$> view #selections plan

        outputsActual :: [[TokenBundle]]
        outputsActual = NE.toList
            (fmap (view #tokens) . view #outputs <$> selections)
        outputsExpected :: [[TokenBundle]]
        outputsExpected =
            NE.toList . view #outputs <$> view #selections plan

        makeCoverage
            = cover 4 (length selections == 1)
                "number of selections = 1"
            . cover 4 (length selections == 2)
                "number of selections = 2"
            . cover 4 (length selections > 2)
                "number of selections > 2"
        makeReports
            = report inputsActual
                "inputsActual"
            . report inputsExpected
                "inputsExpected"
            . report outputsActual
                "outputsActual"
            . report outputsExpected
                "outputsExpected"

    constraints = unMockTxConstraints mockTxConstraints
    maybeSelectionWithdrawals =
        migrationPlanToSelectionWithdrawals plan NoWithdrawal targetAddresses
    plan = Migration.createPlan constraints utxo reward
    reward = Migration.RewardWithdrawal (Coin 0)

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList utxo

data WalletLayerFixture s m = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer m s
    , _fixtureWalletLayer :: WalletLayer m s
    , _fixtureWallet :: WalletId
    }

setupFixture
    :: forall s m
     . ( MonadUnliftIO m
       , IsOurs s Address
       , IsOurs s RewardAccount
       , Sqlite.PersistAddressBook s
       , KeyOf s ~ ShelleyKey
       , CredFromOf s ~ 'CredFromKeyK
       )
    => WalletFlavorS s
    -> (WalletId, WalletName, s)
    -> m (WalletLayerFixture s m)
setupFixture wF (wid,wname,wstate) = do
    params <- liftIO
        $ W.createWallet (block0, dummyNetworkParameters) wid wname wstate
    (_kill, db) <- liftIO
        $ newBootDBLayerInMemory wF
            nullTracer
            dummyTimeInterpreter
            wid
            params
    let db' = hoistDBLayer liftIO db
        wl =
            WalletLayer
                nullTracer
                (block0, dummyNetworkParameters)
                mockNetworkLayer
                dummyTransactionLayer
                db'
        wal = walletId_ db
    pure $ WalletLayerFixture db' wl wal

slotNoTime :: SlotNo -> UTCTime
slotNoTime = posixSecondsToUTCTime . fromIntegral . unSlotNo

-- | A dummy transaction layer to see the effect of a root private key. It
-- implements a fake signer that still produces sort of witnesses
dummyTransactionLayer :: TransactionLayer ShelleyKey 'CredFromKeyK SealedTx
dummyTransactionLayer = TransactionLayer
    { addVkWitnesses =
        error "dummyTransactionLayer: addVkWitnesses not implemented"
    , decodeTx = \_era _witCtx _sealed ->
        ( Tx
            { txId = Hash ""
            , txCBOR = Nothing
            , fee = mempty
            , resolvedInputs = mempty
            , resolvedCollateralInputs = mempty
            , outputs = mempty
            , collateralOutput = Nothing
            , withdrawals = mempty
            , metadata = mempty
            , scriptValidity = Nothing
            }
        , emptyTokenMapWithScripts
        , emptyTokenMapWithScripts
        , []
        , Nothing
        , emptyWitnessCount
        )
    , transactionWitnessTag = TxWitnessShelleyUTxO
    }

fakeSealedTx :: HasCallStack => (Hash "Tx", [ByteString]) -> SealedTx
fakeSealedTx (tx, wit) = mockSealedTx $ B8.pack repr
  where
    repr = show (tx, wit)

mockNetworkLayer :: Monad m => NetworkLayer m block
mockNetworkLayer = dummyNetworkLayer
    { currentNodeTip =
        pure dummyTip
    , currentNodeEra =
        pure (AnyCardanoEra AllegraEra)
    , currentProtocolParameters =
        pure (protocolParameters dummyNetworkParameters)
    , timeInterpreter = dummyTimeInterpreter
    , syncProgress =
        error "dummyNetworkLayer: syncProgress not implemented"
    }
  where
    dummyTip = BlockHeader (SlotNo 0) (Quantity 0) dummyHash (Just dummyHash)
    dummyHash = Hash "dummy hash"

type DummyState =
    TestState
        (Map Address (Index 'Soft 'CredFromKeyK))
        'Mainnet
        ShelleyKey
        'CredFromKeyK

dummyStateF :: WalletFlavorS DummyState
dummyStateF =
    TestStateS
        $ defaultTestFeatures
            { isOwnedTest = \(TestState m) (rootK, pwd) addr -> do
                ix <- Map.lookup addr m
                let accXPrv = deriveAccountPrivateKey pwd rootK minBound
                    addrXPrv
                        = deriveAddressPrivateKey pwd accXPrv UtxoExternal ix
                return (addrXPrv, pwd)
            }

instance Sqlite.AddressBookIso DummyState where
    data Prologue DummyState = DummyPrologue
    data Discoveries DummyState = DummyDiscoveries DummyState
    addressIso = iso from to
      where
        from x = (DummyPrologue, DummyDiscoveries x)
        to (_,DummyDiscoveries x) = x

instance Eq (Sqlite.Prologue DummyState) where _ == _ = True
instance Eq (Sqlite.Discoveries DummyState) where
    DummyDiscoveries a == DummyDiscoveries b = a == b

instance Sqlite.PersistAddressBook DummyState where
    insertPrologue _ _ = pure ()
    insertDiscoveries _ _ _ = pure ()
    loadPrologue _ = error "DummyState.loadPrologue: not implemented"
    loadDiscoveries _ _ = error "DummyState.loadDiscoveries: not implemented"

instance NFData DummyState

instance Arbitrary DummyState where
    shrink _ = []
    arbitrary = return (TestState mempty)

instance IsOurs DummyState Address where
    isOurs _ s = (Just (DerivationIndex 0 :| []), s)

instance IsOurs DummyState RewardAccount where
    isOurs _ s = (Nothing, s)
instance GenChange DummyState where
    type ArgGenChange DummyState = ()
    genChange _ s = (Address "dummy", s)

instance CompareDiscovery DummyState where
    compareDiscovery _ _ _ = EQ

instance KnownAddresses DummyState where
    knownAddresses _ = []

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    shrink _ = []
    arbitrary = elements
        [ WalletName "My Wallet"
        , WalletName mempty
        ]

instance Arbitrary (Passphrase purpose) where
    shrink _ = []
    arbitrary =
        Passphrase . BA.convert . BS.pack <$> replicateM 16 arbitrary

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

instance {-# OVERLAPS #-} Arbitrary (ShelleyKey 'RootK XPrv, Passphrase "user")
  where
    shrink _ = []
    arbitrary = do
        pwd <- arbitrary
        mw <- arbitrary
        let key = generateKeyFromSeed (mw, Nothing) (preparePassphrase pwd)
        return (key, pwd)

instance Arbitrary SortOrder where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Show XPrv where
    show = show . CC.unXPrv

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary =
        Hash . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Coin where
    shrink _ = []
    arbitrary = genCoinPositive

instance Arbitrary Tx where
    arbitrary = genTx
    shrink = shrinkTx

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> (Hash . B8.pack <$> vector 32)
        <*> scale (`mod` 3) arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut (Address "address") . TokenBundle.fromCoin
        <$> genCoinPositive

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger, Expired]
        <*> elements [Incoming, Outgoing]
        <*> genSlotNo
        <*> fmap Quantity arbitrary
        <*> arbitrary
        <*> liftArbitrary genSlotNo
