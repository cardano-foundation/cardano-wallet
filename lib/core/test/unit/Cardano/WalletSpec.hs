{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Cardano.Wallet
    ( ErrSignPayment (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , LocalTxSubmissionConfig (..)
    , SelectionResultWithoutChange
    , WalletLayer (..)
    , migrationPlanToSelectionWithdrawals
    , runLocalTxSubmissionPool
    , throttle
    )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), putTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0
    , dummyNetworkLayer
    , dummyNetworkParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , mkTxId
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    , genNestedTxMetadata
    , genSlotNo
    , shrinkSlotNo
    , shrinkTxMetadata
    )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , HardDerivation (..)
    , Index
    , Passphrase (..)
    , Role (..)
    , deriveRewardAccount
    , getRawKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( SelectionError (..) )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionResult (..) )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockTxConstraints (..), genTokenBundleMixed, unMockTxConstraints )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block
    , BlockHeader (BlockHeader)
    , EpochNo (..)
    , NetworkParameters (..)
    , PoolId (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletDelegationNext (WalletDelegationNext)
    , WalletDelegationStatus (..)
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
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , LocalTxSubmissionStatus (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxMetadata
    , TxOut (..)
    , TxStatus (..)
    , isPending
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx, genTxInLargeRange, shrinkTx )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..)
    , TransactionLayer (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM, forM_, replicateM, void )
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
    ( Tracer (..), nullTracer )
import Crypto.Hash
    ( hash )
import Data.Bifunctor
    ( second )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft, isRight )
import Data.Function
    ( on )
import Data.Generics.Internal.VL.Lens
    ( set, view, (^.) )
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
import Data.Word.Odd
    ( Word31 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , InfiniteList (..)
    , NonEmptyList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , arbitrarySizedFractional
    , checkCoverage
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAllBlind
    , label
    , liftArbitrary
    , liftShrink
    , liftShrink2
    , listOf1
    , oneof
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , sized
    , suchThat
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( report )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
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
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Migration as Migration
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = describe "Cardano.WalletSpec" $ parallel $ do

    parallel $ describe "Pointless mockEventSource to cover 'Show' instances for errors" $ do
        let wid = WalletId (hash @ByteString "arbitrary")
        it (show $ ErrSignPaymentNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrSubmitTxNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrUpdatePassphraseNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase) True

    parallel $ describe "WalletLayer works as expected" $ do
        it "Wallet upon creation is written down in db"
            (property walletCreationProp)
        it "Wallet cannot be created more than once"
            (property walletDoubleCreationProp)
        it "Wallet after being created can be got using valid wallet Id"
            (property walletGetProp)
        it "Wallet with wrong wallet Id cannot be got"
            (property walletGetWrongIdProp)
        it "Two wallets with same mnemonic have a same public id"
            (property walletIdDeterministic)
        it "Two wallets with different mnemonic have a different public id"
            (property walletIdInjective)
        it "Wallet has name corresponding to its last update"
            (property walletUpdateName)
        it "Can't change name if wallet doesn't exist"
            (property walletUpdateNameNoSuchWallet)
        it "Can change passphrase of the last private key attached, if any"
            (property walletUpdatePassphrase)
        it "Can't change passphrase with a wrong old passphrase"
            (property walletUpdatePassphraseWrong)
        it "Can't change passphrase if wallet doesn't exist"
            (property walletUpdatePassphraseNoSuchWallet)
        it "Passphrase info is up-to-date after wallet passphrase update"
            (property walletUpdatePassphraseDate)
        it "Root key is re-encrypted with new passphrase"
            (withMaxSuccess 10 $ property walletKeyIsReencrypted)
        it "Wallet can list transactions"
            (property walletListTransactionsSorted)

    parallel $ describe "Tx fee estimation" $
        it "Fee estimates are sound"
            (property prop_estimateFee)

    describe "LocalTxSubmission" $ do
        it "LocalTxSubmission pool retries pending transactions"
            (property prop_localTxSubmission)
        it "LocalTxSubmission updates are limited in frequency"
            (property prop_throttle)

    parallel $ describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join"
            (property prop_guardJoinQuit)
        it "You can join if you cannot quit"
            (property prop_guardQuitJoin)

    parallel $ describe "Join/Quit Stake pool unit mockEventSource" $ do
        let noRetirementPlanned = Nothing
        it "Cannot join A, when active = A" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            W.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join A, when next = [A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            W.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can join A, when active = A, next = [B]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            W.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let next2 = next (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            W.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join when pool is unknown" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            W.guardJoin knownPools dlg pidUnknown noRetirementPlanned
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
        it "Cannot quit when active: not_delegating, next = []" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            W.guardQuit dlg (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [not_delegating]" $ do
            let next1 = next (EpochNo 1) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            W.guardQuit dlg (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [B, not_delegating]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let next2 = next (EpochNo 2) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            W.guardQuit dlg (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Can quit when active: not_delegating, next = [A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = NotDelegating, next = [next1]}
            W.guardQuit dlg (Coin 0) `shouldBe` Right ()

    parallel $ describe "Migration" $ do
        describe "migrationPlanToSelectionWithdrawals" $ do
            it "Target addresses are cycled correctly." $
                property prop_migrationPlanToSelectionWithdrawals_addresses
            it "Inputs and outputs are preserved in the correct order." $
                property prop_migrationPlanToSelectionWithdrawals_io

  where
    pidA = PoolId "A"
    pidB = PoolId "B"
    pidUnknown = PoolId "unknown"
    knownPools = Set.fromList [pidA, pidB]
    next = WalletDelegationNext

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

prop_guardJoinQuit
    :: [PoolId]
    -> WalletDelegation
    -> PoolId
    -> Maybe W.PoolRetirementEpochInfo
    -> Property
prop_guardJoinQuit knownPoolsList dlg pid mRetirementInfo = checkCoverage
    $ cover 10 retirementNotPlanned
        "retirementNotPlanned"
    $ cover 10 retirementPlanned
        "retirementPlanned"
    $ cover 10 alreadyRetired
        "alreadyRetired"
    $ case W.guardJoin knownPools dlg pid mRetirementInfo of
        Right () ->
            label "I can join" $ property $
                alreadyRetired `shouldBe` False
        Left W.ErrNoSuchPool{} ->
            label "ErrNoSuchPool" $ property True
        Left W.ErrAlreadyDelegating{} ->
            label "ErrAlreadyDelegating"
                (W.guardQuit dlg (Coin 0) === Right ())
  where
    knownPools = Set.fromList knownPoolsList
    retirementNotPlanned =
        isNothing mRetirementInfo
    retirementPlanned =
        (Just True ==) $ do
            info <- mRetirementInfo
            pure $ W.currentEpoch info < W.retirementEpoch info
    alreadyRetired =
        (Just True ==) $ do
            info <- mRetirementInfo
            pure $ W.currentEpoch info >= W.retirementEpoch info

prop_guardQuitJoin
    :: NonEmptyList PoolId
    -> WalletDelegation
    -> Word64
    -> Property
prop_guardQuitJoin (NonEmpty knownPoolsList) dlg rewards =
    let knownPools = Set.fromList knownPoolsList in
    let noRetirementPlanned = Nothing in
    case W.guardQuit dlg (Coin rewards) of
        Right () ->
            label "I can quit" $ property True
        Left W.ErrNotDelegatingOrAboutTo ->
            label "ErrNotDelegatingOrAboutTo" $
                W.guardJoin
                    knownPools dlg (last knownPoolsList) noRetirementPlanned
                    === Right ()
        Left W.ErrNonNullRewards{} ->
            label "ErrNonNullRewards"
                (property $ rewards /= 0)

walletCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletCreationProp newWallet = monadicIO $ do
    WalletLayerFixture DBLayer{..} _wl walletIds _ <- run $ setupFixture newWallet
    resFromDb <- run $ atomically $ readCheckpoint $ L.head walletIds
    assert (isJust resFromDb)

walletDoubleCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletDoubleCreationProp newWallet@(wid, wname, wstate) =
    monadicIO $ do
        WalletLayerFixture _db wl _walletIds _ <- run $ setupFixture newWallet
        secondTrial <- run $ runExceptT $ W.createWallet wl wid wname wstate
        assert (isLeft secondTrial)

walletGetProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletGetProp newWallet = monadicIO $ do
    WalletLayerFixture _db wl walletIds _ <- run $ setupFixture newWallet
    resFromGet <- run $ runExceptT $ W.readWallet wl (L.head walletIds)
    assert (isRight resFromGet)

walletGetWrongIdProp
    :: ((WalletId, WalletName, DummyState), WalletId)
    -> Property
walletGetWrongIdProp (newWallet@(wid, _, _), walletId) = monadicIO $ do
    WalletLayerFixture _db wl _walletIds _ <- run $ setupFixture newWallet
    attempt <- run $ runExceptT $ W.readWallet wl walletId
    assert ((if wid /= walletId then isLeft else isRight) attempt)

walletIdDeterministic
    :: (WalletId, WalletName, DummyState)
    -> Property
walletIdDeterministic newWallet = monadicIO $ do
    WalletLayerFixture _ _ widsA _ <- run $ setupFixture newWallet
    WalletLayerFixture _ _ widsB _ <- run $ setupFixture newWallet
    assert (widsA == widsB)

walletIdInjective
    :: ((WalletId, WalletName, DummyState), (WalletId, WalletName, DummyState))
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ do
    WalletLayerFixture _ _ widsA _ <- run $ setupFixture walletA
    WalletLayerFixture _ _ widsB _ <- run $ setupFixture walletB
    assert (widsA /= widsB)

walletUpdateName
    :: (WalletId, WalletName, DummyState)
    -> [WalletName]
    -> Property
walletUpdateName wallet@(_, wName0, _) names = monadicIO $ do
    wName <- run $ do
        WalletLayerFixture _ wl [wid] _ <- setupFixture wallet
        unsafeRunExceptT $ forM_ names $ \wName ->
            W.updateWallet wl wid (\x -> x { name = wName })
        fmap (name . (\(_, b, _) -> b))
            <$> unsafeRunExceptT $ W.readWallet wl wid
    assert (wName == last (wName0 : names))

walletUpdateNameNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> WalletName
    -> Property
walletUpdateNameNoSuchWallet wallet@(wid', _, _) wid wName =
    wid /= wid' ==> monadicIO $ do
        WalletLayerFixture _ wl _ _ <- run $ setupFixture wallet
        attempt <- run $ runExceptT $
            W.updateWallet wl wid (\x -> x { name = wName })
        assert (attempt == Left (ErrNoSuchWallet wid))

walletUpdatePassphrase
    :: (WalletId, WalletName, DummyState)
    -> Passphrase "raw"
    -> Maybe (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> Property
walletUpdatePassphrase wallet new mxprv = monadicIO $ do
    WalletLayerFixture _ wl [wid] _ <- run $ setupFixture wallet
    case mxprv of
        Nothing -> prop_withoutPrivateKey wl wid
        Just (xprv, pwd) -> prop_withPrivateKey wl wid (xprv, pwd)
  where
    prop_withoutPrivateKey wl wid = do
        attempt <- run $ runExceptT $ W.updateWalletPassphrase wl wid (new, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)

    prop_withPrivateKey wl wid (xprv, pwd) = do
        run $ unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
        attempt <- run $ runExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, new)
        assert (attempt == Right ())

walletUpdatePassphraseWrong
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> (Passphrase "raw", Passphrase "raw")
    -> Property
walletUpdatePassphraseWrong wallet (xprv, pwd) (old, new) =
    pwd /= coerce old ==> monadicIO $ do
        WalletLayerFixture _ wl [wid] _ <- run $ setupFixture wallet
        attempt <- run $ do
            unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
            runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey
                $ ErrWithRootKeyWrongPassphrase wid
                ErrWrongPassphrase
        assert (attempt == Left err)

walletUpdatePassphraseNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> (Passphrase "raw", Passphrase "raw")
    -> Property
walletUpdatePassphraseNoSuchWallet wallet@(wid', _, _) wid (old, new) =
    wid /= wid' ==> monadicIO $ do
        WalletLayerFixture _ wl _ _ <- run $ setupFixture wallet
        attempt <- run $ runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        assert (attempt == Left err)

walletUpdatePassphraseDate
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> Property
walletUpdatePassphraseDate wallet (xprv, pwd) = monadicIO $ liftIO $ do
    (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
    let infoShouldSatisfy predicate = do
            info <- (passphraseInfo . (\(_,b,_) -> b)) <$>
                unsafeRunExceptT (W.readWallet wl wid)
            info `shouldSatisfy` predicate
            return info

    void $ infoShouldSatisfy isNothing
    unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
    info <- infoShouldSatisfy isJust
    pause
    unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, coerce pwd)
    void $ infoShouldSatisfy (\info' -> isJust info' && info' > info)
  where
    pause = threadDelay 500

walletKeyIsReencrypted
    :: (WalletId, WalletName)
    -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> Passphrase "raw"
    -> Property
walletKeyIsReencrypted (wid, wname) (xprv, pwd) newPwd =
    monadicIO $ liftIO $ do
        let st = Map.insert (Address "source") minBound mempty
        let wallet = (wid, wname, DummyState st)
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
        let credentials (rootK, pwdP) =
                (getRawKey $ deriveRewardAccount pwdP rootK, pwdP)
        selection' <- unsafeRunExceptT $
            W.assignChangeAddressesAndUpdateDb wl wid () selection
        (_,_,_,txOld) <- unsafeRunExceptT $ W.buildAndSignTransaction
            @_ @_ wl wid credentials (coerce pwd) ctx selection'
        unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, newPwd)
        (_,_,_,txNew) <- unsafeRunExceptT $ W.buildAndSignTransaction
            @_ @_ wl wid credentials newPwd ctx selection'
        txOld `shouldBe` txNew
  where
    selection = SelectionResult
        { inputsSelected = NE.fromList
            [ ( TxIn (Hash "eb4ab6028bd0ac971809d514c92db1") 1
              , TxOut (Address "source") (TokenBundle.fromCoin $ Coin 42)
              )
            ]
        , extraCoinSource =
            Nothing
        , outputsCovered =
            [ TxOut (Address "destination") (TokenBundle.fromCoin $ Coin 14) ]
        , changeGenerated =
            [ (TokenBundle.fromCoin $ Coin 1) ]
        , utxoRemaining =
            UTxOIndex.empty
        }

    ctx = defaultTransactionCtx

walletListTransactionsSorted
    :: (WalletId, WalletName, DummyState)
    -> SortOrder
    -> (Maybe UniformTime, Maybe UniformTime)
    -> [(Tx, TxMeta)]
    -> Property
walletListTransactionsSorted wallet@(wid, _, _) _order (_mstart, _mend) history =
    monadicIO $ liftIO $ do
        WalletLayerFixture DBLayer{..} wl _ slotNoTime <- liftIO $ setupFixture wallet
        atomically $ unsafeRunExceptT $ putTxHistory wid history
        txs <- unsafeRunExceptT $
            W.listTransactions @_ @_ @_ wl wid Nothing Nothing Nothing Descending
        length txs `shouldBe` L.length history
        -- With the 'Down'-wrapper, the sort is descending.
        txs `shouldBe` L.sortOn (Down . slotNo . txInfoMeta) txs
        -- Check transaction time calculation
        let times = Map.fromList [(txInfoId i, txInfoTime i) | i <- txs]
        let expTimes = Map.fromList
                [ (tx ^. #txId, slotNoTime (meta ^. #slotNo))
                | (tx, meta) <- history ]
        times `shouldBe` expTimes

{-------------------------------------------------------------------------------
                        Properties of tx fee estimation
-------------------------------------------------------------------------------}

-- | Properties of 'estimateFeeForCoinSelection':
-- 1. There is no coin selection with a fee above the estimated maximum.
-- 2. The minimum estimated fee is no greater than the maximum estimated fee.
-- 3. Around 10% of fees are below the estimated minimum.
prop_estimateFee
    :: NonEmptyList (Maybe Coin)
    -> Property
prop_estimateFee (NonEmpty coins) =
    case evalState (runExceptT $ W.estimateFee runSelection) 0 of
        Left err ->
            label "errors: all" $ err === genericError

        Right estimation@(W.FeeEstimation minFee maxFee) ->
            label ("errors: " <> if any isNothing coins then "some" else "none") $
            counterexample (show estimation) $ conjoin
                [ property $ maxFee <= unCoin (maximum (catMaybes coins))
                , property $ minFee <= maxFee
                , proportionBelow minFee coins
                    `closeTo` (1/10 :: Double)
                ]
  where
    genericError :: W.ErrSelectAssets
    genericError
        = W.ErrSelectAssetsSelectionError
        $ SelectionBalanceError
        $ Balance.BalanceInsufficient
        $ Balance.BalanceInsufficientError TokenBundle.empty TokenBundle.empty

    runSelection
        :: ExceptT W.ErrSelectAssets (State Int) Coin
    runSelection = do
        i <- lift get
        lift $ put $ (i + 1) `mod` length coins
        case (coins !! i) of
            Nothing -> except $ Left genericError
            Just c  -> except $ Right c

    proportionBelow minFee xs =
        fromIntegral (countBelow minFee xs) / fromIntegral (count isJust xs)
      where
        count p = length . filter p

        -- Find the number of results below the "minimum" estimate.
        countBelow sup =
            count ((< sup) . unCoin . fromMaybe maxBound)

    -- Two fractions are close to each other if they are within 20% either way.
    closeTo a b =
        counterexample (show a <> " & " <> show b <> " are not close enough") $
        property $ abs (a - b) < (1/5)


{-------------------------------------------------------------------------------
                               LocalTxSubmission
-------------------------------------------------------------------------------}

data TxRetryTest = TxRetryTest
    { retryTestPool :: [LocalTxSubmissionStatus SealedTx]
    , retryTestTxHistory :: GenTxHistory
    , postTxResults :: [(SealedTx, Bool)]
    , testSlottingParameters :: SlottingParameters
    , retryTestWallet :: (WalletId, WalletName, DummyState)
    } deriving (Generic, Show, Eq)

numSlots :: TxRetryTest -> Word64
numSlots = const 100

newtype GenTxHistory = GenTxHistory { getTxHistory :: [(Tx, TxMeta)] }
    deriving (Generic, Show, Eq)

instance Arbitrary GenTxHistory where
    arbitrary = fmap GenTxHistory (gen `suchThat` hasPending)
      where
        gen = uniq <$> listOf1 ((,) <$> genTx' <*> genTxMeta)
        uniq = L.nubBy ((==) `on` (view #txId . fst))
        genTx' = mkTx <$> genTid
        hasPending = any ((== Pending) . view #status . snd)
        genTid = Hash . B8.pack <$> listOf1 (elements ['A'..'Z'])
        mkTx tid = Tx tid Nothing [] [] [] mempty Nothing Nothing
        genTxMeta = do
            sl <- genSmallSlot
            let bh = Quantity $ fromIntegral $ unSlotNo sl
            st <- elements [Pending, InLedger, Expired]
            dir <- elements [Incoming, Outgoing]
            expry <- oneof [fmap (Just . (+ sl)) genSmallSlot, pure Nothing]
            pure $ TxMeta st dir sl bh (Coin 0) expry
        genSmallSlot = SlotNo . fromIntegral <$> sized (\n -> choose (0, 4 * n))

    shrink = fmap GenTxHistory
        . filter (not . null)
        . shrinkList (liftShrink2 shrinkTx' shrinkMeta)
        . getTxHistory
      where
        shrinkTx' tx = [set #txId tid' tx | tid' <- shrink (view #txId tx)]
        shrinkMeta (TxMeta st dir sl bh amt ex) =
            [ TxMeta st dir sl' bh amt ex'
            | (sl', ex') <- liftShrink2 shrinkSlotNo (liftShrink shrinkSlotNo)
                (sl, ex) ]

instance Arbitrary TxRetryTest where
    arbitrary = do
        txHistory <- arbitrary
        let pool = mkLocalTxSubmissionStatus txHistory
        results <- zip (map (view #submittedTx) pool) . getInfiniteList <$> arbitrary
        TxRetryTest pool txHistory results <$> arbitrary <*> arbitrary

    shrink (TxRetryTest _ txHistory res sp wal) =
        [ TxRetryTest (mkLocalTxSubmissionStatus txHistory') txHistory' res sp' wal'
        | (txHistory', sp', wal') <- shrink (txHistory, sp, wal)
        ]

mkLocalTxSubmissionStatus
    :: GenTxHistory
    -> [LocalTxSubmissionStatus SealedTx]
mkLocalTxSubmissionStatus = mapMaybe getStatus . getTxHistory
  where
    getStatus :: (Tx, TxMeta) -> Maybe (LocalTxSubmissionStatus SealedTx)
    getStatus (tx, txMeta)
        | isPending txMeta = Just st
        | otherwise = Nothing
      where
        i = tx ^. #txId
        sl = txMeta ^. #slotNo
        st = LocalTxSubmissionStatus i (SealedTx (getHash i)) sl sl

instance Arbitrary SlottingParameters where
    arbitrary = mk <$> choose (0.5, 1)
      where
        mk f = dummySlottingParameters
            { getActiveSlotCoefficient = ActiveSlotCoefficient f }

-- | 'WalletLayer' context.
data TxRetryTestCtx = TxRetryTestCtx
    { ctxDbLayer :: DBLayer TxRetryTestM DummyState ShelleyKey
    , ctxNetworkLayer :: NetworkLayer TxRetryTestM Block
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
    res <- run $ runTest st $ \ctx@(TxRetryTestCtx DBLayer{..} _ _ wid) -> do
        -- Test setup
        atomically $ do
            let txHistory = getTxHistory (retryTestTxHistory tc)
            unsafeRunExceptT $ putTxHistory wid txHistory
            forM_ (retryTestPool tc) $ \(LocalTxSubmissionStatus i tx _ sl) ->
                unsafeRunExceptT $ putLocalTxSubmission wid i tx sl

        -- Run test
        let cfg = LocalTxSubmissionConfig (timeStep st) 10
        runLocalTxSubmissionPool @_ @DummyState @ShelleyKey cfg ctx wid

        -- Gather state
        atomically $ readLocalTxSubmissionPending wid

    monitor $ counterexample $ unlines $
        [ "posted txs = " ++ show (resSubmittedTxs res)
        , "final pool state = " ++ show (resAction res)
        , "logs:"
        ] ++ map (T.unpack . toText) (resLogs res)

    -- props:
    --  1. pending transactions in pool are retried
    let inPool = (`elem` (submittedTx <$> retryTestPool tc))
    assert (all inPool (resSubmittedTxs res))

    --  2. non-pending transactions not retried
    let mkSealed = SealedTx . getHash . view #txId
    let nonPending = map (mkSealed . fst)
            . filter ((/= Pending) . view #status . snd)
            . getTxHistory $ retryTestTxHistory tc
    assert (all (`notElem` (resSubmittedTxs res)) nonPending)

    --  3. retries can fail and not break the wallet
    assert (not $ null $ resAction res)

  where
    runTest
        :: TxRetryTestState
        -> (TxRetryTestCtx -> TxRetryTestM a)
        -> IO (TxRetryTestResult a)
    runTest st testAction = do
        submittedVar <- newMVar []
        (msgs, res) <- captureLogging' $ \tr -> do
            flip runReaderT st $ unTxRetryTestM $ do
                WalletLayerFixture db _wl [wid] _slotNoTime <-
                    setupFixture $ retryTestWallet tc
                let ctx = TxRetryTestCtx db (mockNetwork submittedVar) tr wid

                testAction ctx
        TxRetryTestResult msgs res <$> readMVar submittedVar

    mockNetwork :: MVar [SealedTx] -> NetworkLayer TxRetryTestM Block
    mockNetwork var = dummyNetworkLayer
        { currentSlottingParameters = pure (testSlottingParameters tc)
        , postTx = \tx -> ExceptT $ do
                stash var tx
                pure $ case lookup tx (postTxResults tc) of
                    Just True -> Right ()
                    Just False -> Left (W.ErrPostTxBadRequest "intended")
                    Nothing -> Left (W.ErrPostTxProtocolFailure "unexpected")
        , watchNodeTip = mockNodeTip (numSlots tc) 0
        }

    mockNodeTip end sl cb
        | sl < end = do
            let h = Hash ""
            void $ cb $ BlockHeader (SlotNo sl) (Quantity (fromIntegral sl)) h h
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
    genUTxOEntry = (,) <$> genTxIn <*> genTxOut
      where
        genTxIn :: Gen TxIn
        genTxIn = genTxInLargeRange

        genTxOut :: Gen TxOut
        genTxOut = TxOut
            <$> genAddress
            <*> genTokenBundleMixed mockTxConstraints

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
    test :: NonEmpty SelectionResultWithoutChange -> Property
    test selections = makeCoverage $ makeReports $
        cycledTargetAddressesActual ==
        cycledTargetAddressesExpected
      where
        cycledTargetAddressesActual = view #address <$>
            (view #outputsCovered =<< NE.toList selections)
        cycledTargetAddressesExpected = NE.take
            (length cycledTargetAddressesActual)
            (NE.cycle targetAddresses)
        totalOutputCount = F.sum (length . view #outputsCovered <$> selections)
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
    test :: NonEmpty SelectionResultWithoutChange -> Property
    test selections = makeCoverage $ makeReports $ conjoin
        [ inputsActual == inputsExpected
        , outputsActual == outputsExpected
        ]
      where
        inputsActual :: [[(TxIn, TxOut)]]
        inputsActual =
            NE.toList (NE.toList . view #inputsSelected <$> selections)
        inputsExpected :: [[(TxIn, TxOut)]]
        inputsExpected =
            NE.toList . view #inputIds <$> view #selections plan

        outputsActual :: [[TokenBundle]]
        outputsActual = NE.toList
            (fmap (view #tokens) . view #outputsCovered <$> selections)
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

instance Arbitrary WalletDelegation where
    shrink = genericShrink
    arbitrary = WalletDelegation
        <$> arbitrary
        <*> oneof [ pure [], vector 1, vector 2 ]

instance Arbitrary WalletDelegationStatus where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary WalletDelegationNext where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary PoolId where
    arbitrary = pure $ PoolId "a"

instance Arbitrary W.PoolRetirementEpochInfo where
    arbitrary = W.PoolRetirementEpochInfo <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList utxo

data WalletLayerFixture m = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer m DummyState ShelleyKey
    , _fixtureWalletLayer :: WalletLayer m DummyState ShelleyKey
    , _fixtureWallet :: [WalletId]
    , _fixtureSlotNoTime :: SlotNo -> UTCTime
    }

setupFixture
    :: (MonadUnliftIO m, MonadFail m, MonadTime m)
    => (WalletId, WalletName, DummyState)
    -> m (WalletLayerFixture m)
setupFixture (wid, wname, wstate) = do
    let nl = mockNetworkLayer
    let tl = dummyTransactionLayer
    db <- MVar.newDBLayer timeInterpreter
    let wl = WalletLayer nullTracer (block0, np, st) nl tl db
    res <- runExceptT $ W.createWallet wl wid wname wstate
    let wal = case res of
            Left _ -> []
            Right walletId -> [walletId]
    pure $ WalletLayerFixture db wl wal slotNoTime
  where
    timeInterpreter = dummyTimeInterpreter
    slotNoTime = posixSecondsToUTCTime . fromIntegral . unSlotNo
    np = dummyNetworkParameters
    st = SyncTolerance 10

-- | A dummy transaction layer to see the effect of a root private key. It
-- implements a fake signer that still produces sort of witnesses
dummyTransactionLayer :: TransactionLayer ShelleyKey
dummyTransactionLayer = TransactionLayer
    { mkTransaction = \_era _stakeCredentials keystore _pp _ctx cs -> do
        let inps' = NE.toList $ second txOutCoin <$> inputsSelected cs
        -- TODO: (ADP-957)
        let cinps' = []
        let tid = mkTxId inps' (outputsCovered cs) mempty Nothing
        let tx = Tx
                 { txId = tid
                 , fee = Nothing
                 , resolvedInputs = inps'
                 , resolvedCollateral = cinps'
                 , outputs = outputsCovered cs
                 , withdrawals = mempty
                 , metadata = Nothing
                 , scriptValidity = Nothing
                 }
        wit <- forM (inputsSelected cs) $ \(_, TxOut addr _) -> do
            (xprv, Passphrase pwd) <- withEither
                (ErrKeyNotFoundForAddress addr) $ keystore addr
            let sigData = tx ^. #txId . #getHash
            let sig = CC.unXSignature $ CC.sign pwd (getKey xprv) sigData
            return $ xpubToBytes (getKey $ publicKey xprv) <> sig

        -- (tx1, wit1) == (tx2, wit2) <==> fakebinary1 == fakebinary2
        let fakeBinary = SealedTx . B8.pack $ show (tx, wit)
        return (tx, fakeBinary)

    , mkUnsignedTransaction =
        error "dummyTransactionLayer: mkUnsignedTransaction not implemented"
    , calcMinimumCost =
        error "dummyTransactionLayer: calcMinimumCost not implemented"
    , calcScriptExecutionCost =
        error "dummyTransactionLayer: calcScriptExecutionCost not implemented"
    , computeSelectionLimit =
        error "dummyTransactionLayer: computeSelectionLimit not implemented"
    , tokenBundleSizeAssessor =
        error "dummyTransactionLayer: tokenBundleSizeAssessor not implemented"
    , constraints =
        error "dummyTransactionLayer: constraints not implemented"
    , decodeSignedTx =
        error "dummyTransactionLayer: decodeSignedTx not implemented"
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

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
    dummyTip = BlockHeader (SlotNo 0) (Quantity 0) dummyHash dummyHash
    dummyHash = Hash "dummy hash"

newtype DummyState
    = DummyState (Map Address (Index 'Soft 'AddressK))
    deriving (Generic, Show, Eq)

instance Sqlite.PersistState DummyState where
    insertState _ _ = error "DummyState.insertState: not implemented"
    selectState _ = error "DummyState.selectState: not implemented"

instance NFData DummyState

instance Arbitrary DummyState where
    shrink _ = []
    arbitrary = return (DummyState mempty)

instance IsOurs DummyState Address where
    isOurs _ s = (Just (DerivationIndex 0 :| []), s)

instance IsOurs DummyState RewardAccount where
    isOurs _ s = (Nothing, s)

instance IsOwned DummyState ShelleyKey where
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

instance {-# OVERLAPS #-} Arbitrary (ShelleyKey 'RootK XPrv, Passphrase "encryption")
  where
    shrink _ = []
    arbitrary = do
        pwd <- arbitrary
        mw <- arbitrary
        let key = generateKeyFromSeed (mw, Nothing) pwd
        return (key, pwd)

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

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

instance Arbitrary RewardAccount where
    arbitrary = RewardAccount . BS.pack <$> vector 28

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

instance Arbitrary TxMetadata where
    shrink = shrinkTxMetadata
    arbitrary = genNestedTxMetadata
