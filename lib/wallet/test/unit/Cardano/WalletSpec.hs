{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xpubToBytes )
import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..) )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionError (..)
    )
import Cardano.Wallet
    ( ErrSignPayment (..)
    , ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , LocalTxSubmissionConfig (..)
    , SelectionWithoutChange
    , WalletLayer (..)
    , migrationPlanToSelectionWithdrawals
    , runLocalTxSubmissionPool
    , submitTx
    , throttle
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , Role (..)
    , publicKey
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.DB
    ( DBFresh, DBLayer (..), hoistDBFresh, hoistDBLayer, putTxHistory )
import Cardano.Wallet.DB.Fixtures
    ( logScale' )
import Cardano.Wallet.DB.Layer
    ( newDBFreshInMemory )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( TxSubmissionsStatus )
import Cardano.Wallet.DB.WalletState
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0
    , dummyNetworkLayer
    , dummyNetworkParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , mkTxId
    )
import Cardano.Wallet.Gen
    ( genMnemonic, genSlotNo )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockTxConstraints (..), genTokenBundleMixed, unMockTxConstraints )
import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (..), Passphrase (..) )
import Cardano.Wallet.Primitive.Passphrase.Current
    ( preparePassphrase )
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
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (TokenBundle), getAssets )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity.Gen
    ( genTokenQuantityPositive )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxMeta (..)
    , TxStatus (..)
    , mockSealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx, shrinkTx )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxInLargeRange )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..)
    , Withdrawal (..)
    , emptyTokenMapWithScripts
    , emptyWitnessCount
    )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Cardano.Wallet.Util
    ( HasCallStack )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM_, guard, replicateM, void )
import Control.Monad.Class.MonadTime
    ( DiffTime
    , MonadMonotonicTime (..)
    , MonadTime (..)
    , Time (..)
    , addTime
    , diffTime
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..), wrappedWithRunInIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, runExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..) )
import Control.Monad.Trans.Reader
    ( ReaderT (..), ask )
import Control.Monad.Trans.State.Strict
    ( State, StateT (..), evalState, get, put, state )
import Control.Tracer
    ( Tracer (..), natTracer, nullTracer )
import Crypto.Hash
    ( hash )
import Data.Bifunctor
    ( second )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft )
import Data.Function
    ( on )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL
    ( iso, view, (^.) )
import Data.List
    ( nubBy, sort, sortOn )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, isNothing, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import System.Random
    ( Random )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy, xit )
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
    ( report )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )
import Test.Utils.Time
    ( UniformTime )
import Test.Utils.Trace
    ( captureLogging' )
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
import qualified Cardano.Wallet.DB.Sqlite.Types as DB
import qualified Cardano.Wallet.DB.Store.Checkpoints as Sqlite
import qualified Cardano.Wallet.Primitive.Migration as Migration
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

spec :: Spec
spec = describe "Cardano.WalletSpec" $ do
    describe "Pointless mockEventSource to cover 'Show' instances for errors" $ do
        let wid = WalletId (hash @ByteString "arbitrary")
        it (show $ ErrSignPaymentNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase) True

    describe "WalletLayer works as expected" $ do
        it "Wallet cannot be created more than once"
            (property walletDoubleCreationProp)
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
            (property walletUpdatePassphraseNoSuchWallet)
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

walletDoubleCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletDoubleCreationProp newWallet =
    monadicIO $ do
        WalletLayerFixture dbf _db _wl _walletIds <- run $ setupFixture newWallet
        secondTrial <- run $ runExceptT $ createFixtureWallet dbf newWallet
        assert (isLeft secondTrial)

walletIdDeterministic
    :: (WalletId, WalletName, DummyState)
    -> Property
walletIdDeterministic newWallet = monadicIO $ do
    WalletLayerFixture _ _ _ widsA <- run $ setupFixture newWallet
    WalletLayerFixture _ _ _ widsB <- run $ setupFixture newWallet
    assert (widsA == widsB)

walletIdInjective
    :: ((WalletId, WalletName, DummyState), (WalletId, WalletName, DummyState))
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ do
    WalletLayerFixture _ _ _ widsA <- run $ setupFixture walletA
    WalletLayerFixture _ _ _ widsB <- run $ setupFixture walletB
    assert (widsA /= widsB)

walletUpdateName
    :: (WalletId, WalletName, DummyState)
    -> WalletName
    -> Property
walletUpdateName wallet wName = monadicIO $ do
    wName' <- run $ do
        WalletLayerFixture _ _ wl _ <- setupFixture wallet
        W.updateWallet wl (\x -> x { name = wName })
        (name . (\(_, (b, _), _) -> b)) <$> W.readWallet wl
    assert (wName == wName')

walletUpdatePassphrase
    :: (WalletId, WalletName, DummyState)
    -> Passphrase "user"
    -> Maybe (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> Property
walletUpdatePassphrase wallet new mxprv = monadicIO $ do
    WalletLayerFixture _ _ wl wid <- run $ setupFixture wallet
    case mxprv of
        Nothing -> prop_withoutPrivateKey wl wid
        Just (xprv, pwd) -> prop_withPrivateKey wl wid (xprv, pwd)
  where
    prop_withoutPrivateKey wl wid = do
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase wl wid (new, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)

    prop_withPrivateKey wl wid (xprv, pwd) = do
        run $ W.attachPrivateKeyFromPwd wl (xprv, pwd)
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase wl wid (coerce pwd, new)
        assert (attempt == Right ())

walletUpdatePassphraseWrong
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> (Passphrase "user", Passphrase "user")
    -> Property
walletUpdatePassphraseWrong wallet (xprv, pwd) (old, new) =
    pwd /= coerce old ==> monadicIO $ do
        WalletLayerFixture _ _ wl wid <- run $ setupFixture wallet
        attempt <- run $ do
            W.attachPrivateKeyFromPwd wl (xprv, pwd)
            runExceptT
                $ W.updateWalletPassphraseWithOldPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey
                $ ErrWithRootKeyWrongPassphrase wid
                ErrWrongPassphrase
        assert (attempt == Left err)

walletUpdatePassphraseNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> (Passphrase "user", Passphrase "user")
    -> Property
walletUpdatePassphraseNoSuchWallet wallet@(wid', _, _) wid (old, new) =
    wid /= wid' ==> monadicIO $ do
        WalletLayerFixture _ _ wl _ <- run $ setupFixture wallet
        attempt <- run $ runExceptT
            $ W.updateWalletPassphraseWithOldPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)

walletUpdatePassphraseDate
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "user")
    -> Property
walletUpdatePassphraseDate wallet (xprv, pwd) = monadicIO $ liftIO $ do
    WalletLayerFixture _ _ wl wid  <- liftIO $ setupFixture wallet
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
        $ W.updateWalletPassphraseWithOldPassphrase wl wid
            (coerce pwd, coerce pwd)
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
        WalletLayerFixture _ DBLayer{..} wl _ <- setupFixture wallet
        atomically $ putTxHistory history
        txs <- unsafeRunExceptT $
            W.listTransactions @_ @_ @_ wl Nothing Nothing Nothing
                Descending Nothing
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
            WalletLayerFixture _ DBLayer{..} wl _ <- setupFixture wallet
            atomically $ putTxHistory history'
            txs <- unsafeRunExceptT $
                W.listTransactions @_ @_ @_ wl Nothing start stop order
                    $ Just limitNatural
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

newtype DummyStateWithAddresses = DummyStateWithAddresses [Address]
  deriving stock (Show, Eq)

instance IsOurs DummyStateWithAddresses Address where
    isOurs a s@(DummyStateWithAddresses addr) =
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
        WalletLayerFixture _ DBLayer{..} wl _ <- liftIO $ setupFixture wallet
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
             , (wId, wName, DummyStateWithAddresses [relatedAddress])
             )

{-------------------------------------------------------------------------------
                        Properties of tx fee estimation
-------------------------------------------------------------------------------}

-- | Properties of 'estimateFeeForCoinSelection':
-- 1. There is no coin selection with a fee above the estimated maximum.
-- 2. The minimum estimated fee is no greater than the maximum estimated fee.
-- 3. Around 10% of fees are below the estimated minimum.
prop_calculateFeePercentiles :: NonEmptyList (Maybe Coin) -> Property
prop_calculateFeePercentiles (NonEmpty coins) =
    case evalState (runExceptT $ W.calculateFeePercentiles estimateFee) 0 of
        Left err ->
            label "errors: all" $ err === genericError

        Right percentiles@(W.Percentile minFee, W.Percentile maxFee) ->
            label ("errors: " <> if any isNothing coins then "some" else "none") $
            counterexample (show percentiles) $ conjoin
                [ property $ W.feeToCoin maxFee <= maximum (catMaybes coins)
                , property $ minFee <= maxFee
                , proportionBelow (W.feeToCoin minFee) coins
                    `closeTo` (1/10 :: Double)
                ]
  where
    genericError :: W.ErrSelectAssets
    genericError
        = W.ErrSelectAssetsSelectionError
        $ SelectionBalanceErrorOf
        $ BalanceInsufficient
        $ BalanceInsufficientError
            TokenBundle.empty
            TokenBundle.empty
            TokenBundle.empty

    estimateFee :: ExceptT W.ErrSelectAssets (State Int) W.Fee
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
            st <- elements [Pending]
            dir <- elements [Outgoing]
            expry <- oneof [fmap (Just . (+ sl)) genSmallSlot, pure Nothing]
            i <- arbitrary
            pure $ BuiltTx
                (mkTx i)
                (TxMeta st dir sl bh (Coin 0) expry)
                (fakeSealedTx (i, []))
        genSmallSlot =
            SlotNo . fromIntegral <$> sized (\n -> choose (1, 1+ 4 * n))

instance Arbitrary TxRetryTest where
    arbitrary = do
        GenSubmissions metas <- arbitrary
        let results = zip (builtSealedTx <$> metas) (repeat True)
        TxRetryTest metas results <$> arbitrary <*> arbitrary

instance Arbitrary SlottingParameters where
    arbitrary = mk <$> choose (0.5, 1)
      where
        mk f = dummySlottingParameters
            { getActiveSlotCoefficient = ActiveSlotCoefficient f }

-- | 'WalletLayer' context.
data TxRetryTestCtx = TxRetryTestCtx
    { ctxDbLayer :: DBLayer TxRetryTestM DummyState ShelleyKey
    , ctxNetworkLayer :: NetworkLayer TxRetryTestM Read.Block
    , ctxRetryTracer :: Tracer TxRetryTestM W.WalletWorkerLog
    , ctxTracer :: Tracer IO W.WalletWorkerLog
    , ctxWalletId :: WalletId
    } deriving (Generic)

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

instance MonadMonotonicTime TxRetryTestM where
    getMonotonicTime = do
        st <- TxRetryTestM ask
        modifyMVar (timeVar st) $ \t -> do
            let t' = addTime (timeStep st) t
            pure (t', t')

instance MonadTime TxRetryTestM where
    getCurrentTime = liftIO getCurrentTime

prop_localTxSubmission :: TxRetryTest -> Property
prop_localTxSubmission tc = monadicIO $ do
    st <- TxRetryTestState tc 2 <$> newMVar (Time 0)
    assert $ not $ null $ retryTestPool tc
    res <- run $ runTest st
        $ \ctx@(TxRetryTestCtx dbl@(DBLayer{..}) nl tr _ _) -> do
        unsafeRunExceptT
            $ forM_ (retryTestPool tc) $ submitTx tr dbl nl
        res0 <- atomically readLocalTxSubmissionPending
        -- Run test
        let cfg = LocalTxSubmissionConfig (timeStep st) 10
        runLocalTxSubmissionPool @_ @DummyState @ShelleyKey cfg ctx

        -- Gather state
        res1 <- atomically readLocalTxSubmissionPending
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
                WalletLayerFixture _ db _wl wid <-
                    setupFixture $ retryTestWallet tc
                let ctx = TxRetryTestCtx db (mockNetwork submittedVar)
                        (natTracer liftIO tr) tr wid

                testAction ctx
        TxRetryTestResult msgs res <$> readMVar submittedVar

    mockNetwork :: MVar [SealedTx] -> NetworkLayer TxRetryTestM Read.Block
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
        genDiffTime = abs <$> arbitrarySizedFractional
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
    getMonotonicTime = ThrottleTestT $ MaybeT $ state mockTime
      where
        mockTime (ThrottleTestState later now as) = case later of
            [] -> (Nothing, ThrottleTestState later now as)
            (t:ts) ->
                let now' = addTime t now
                in  (Just now', ThrottleTestState ts now' as)

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
        , ("actuals  = " ++ show (actions st))
        , ("expected = " ++ show expected)
        ]
    -- sanity-check test runner
    assertNamed "consumed test data" (null $ remainingDiffTimes st)
    assertNamed "expected final time" (now st == finalTime)
    assertNamed "runner success" (isJust res)
    -- properties
    assertNamed "action runs at most once per interval" $
        all (> interval) (timeDeltas (map fst (actions st)))
    assertNamed "action runs whenever interval has passed" $
        length diffTimes <= 1 || actions st == expected
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
    { _fixtureDBFresh :: DBFresh m s ShelleyKey
    , _fixtureDBLayer :: DBLayer m s ShelleyKey
    , _fixtureWalletLayer :: WalletLayer m s ShelleyKey 'CredFromKeyK
    , _fixtureWallet :: WalletId
    }

createFixtureWallet
    :: ( MonadUnliftIO m
       , MonadTime m
       , IsOurs s Address
       , IsOurs s RewardAccount
       )
    => DBFresh m s k
    -> (WalletId, WalletName, s)
    -> ExceptT W.ErrWalletAlreadyExists m (DBLayer m s k)
createFixtureWallet dbf (wid, wname, wstate) =
    W.createWallet (block0, dummyNetworkParameters) dbf wid wname wstate

setupFixture
    :: forall s m
     . ( MonadUnliftIO m
       , IsOurs s Address
       , IsOurs s RewardAccount
       , Sqlite.PersistAddressBook s
       )
    => (WalletId, WalletName, s)
    -> m (WalletLayerFixture s m)
setupFixture wd@(wid,  _wname, _wstate) = do
    (_kill, dbf) <-
        liftIO $ newDBFreshInMemory nullTracer dummyTimeInterpreter wid
    db <- liftIO $ unsafeRunExceptT $ createFixtureWallet dbf wd
    let db' = hoistDBLayer liftIO db
        wl =
            WalletLayer
                nullTracer
                (block0, dummyNetworkParameters)
                mockNetworkLayer
                dummyTransactionLayer
                db'
        wal = walletId_ db
    pure $ WalletLayerFixture (hoistDBFresh liftIO dbf) db' wl wal

slotNoTime :: SlotNo -> UTCTime
slotNoTime = posixSecondsToUTCTime . fromIntegral . unSlotNo

-- | A dummy transaction layer to see the effect of a root private key. It
-- implements a fake signer that still produces sort of witnesses
dummyTransactionLayer :: TransactionLayer ShelleyKey 'CredFromKeyK SealedTx
dummyTransactionLayer = TransactionLayer
    { mkTransaction = \_era _stakeCredentials keystore _pp _ctx cs -> do
        let inps' = NE.toList $ second Just <$> view #inputs cs
        -- TODO: (ADP-957)
        let cinps' = []
        let txId = mkTxId inps' (view #outputs cs) mempty Nothing
        let tx = Tx
                { txId
                , txCBOR = Nothing
                , fee = Nothing
                , resolvedInputs = inps'
                , resolvedCollateralInputs = cinps'
                , outputs = view #outputs cs
                , collateralOutput = Nothing
                , withdrawals = mempty
                , metadata = Nothing
                , scriptValidity = Nothing
                }
        let wit = forMaybe (NE.toList $ view #inputs cs) $ \(_, TxOut addr _) -> do
                (xprv, Passphrase pwd) <- keystore addr
                let sigData = tx ^. #txId . #getHash
                let sig = CC.unXSignature $ CC.sign pwd (getKey xprv) sigData
                return $ xpubToBytes (getKey $ publicKey xprv) <> sig

        -- (tx1, wit1) == (tx2, wit2) <==> fakebinary1 == fakebinary2
        let fakeBinary = fakeSealedTx (tx ^. #txId, wit)
        return (tx, fakeBinary)

    , addVkWitnesses =
        error "dummyTransactionLayer: addVkWitnesses not implemented"
    , mkUnsignedTransaction =
        error "dummyTransactionLayer: mkUnsignedTransaction not implemented"
    , computeSelectionLimit =
        error "dummyTransactionLayer: computeSelectionLimit not implemented"
    , tokenBundleSizeAssessor =
        error "dummyTransactionLayer: tokenBundleSizeAssessor not implemented"
    , constraints =
        error "dummyTransactionLayer: constraints not implemented"
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
    }
  where
    forMaybe :: [a] -> (a -> Maybe b) -> [b]
    forMaybe = flip mapMaybe

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

newtype DummyState
    = DummyState (Map Address (Index 'Soft 'CredFromKeyK))
    deriving (Generic, Show, Eq)

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
    arbitrary = return (DummyState mempty)

instance IsOurs DummyState Address where
    isOurs _ s = (Just (DerivationIndex 0 :| []), s)

instance IsOurs DummyState RewardAccount where
    isOurs _ s = (Nothing, s)

instance IsOwned DummyState ShelleyKey 'CredFromKeyK where
    isOwned (DummyState m) (rootK, pwd) addr = do
        ix <- Map.lookup addr m
        let accXPrv = deriveAccountPrivateKey pwd rootK minBound
        let addrXPrv = deriveAddressPrivateKey pwd accXPrv UtxoExternal ix
        return (addrXPrv, pwd)

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
