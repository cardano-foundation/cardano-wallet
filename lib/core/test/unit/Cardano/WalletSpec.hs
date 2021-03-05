{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet
    ( ErrSignPayment (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , ErrWithRootKey (..)
    , WalletLayer (..)
    )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..), putTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( block0, dummyNetworkParameters, dummyTimeInterpreter, mkTxId )
import Cardano.Wallet.Gen
    ( genMnemonic, genSlotNo, genTxMetadata, shrinkTxMetadata )
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
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( BalanceInsufficientError (..)
    , SelectionError (..)
    , SelectionResult (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (BlockHeader)
    , EpochNo (..)
    , NetworkParameters (..)
    , PoolId (..)
    , SlotNo (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinLargePositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , TransactionInfo (txInfoMeta)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxMetadata
    , TxOut (..)
    , TxStatus (..)
    , txId
    , txOutCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), TransactionLayer (..), defaultTransactionCtx )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM, forM_, replicateM, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT, except, runExceptT )
import Control.Monad.Trans.State.Strict
    ( State, evalState, get, put )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( isLeft, isRight )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, isJust, isNothing )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
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
    ( Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , label
    , liftArbitrary
    , oneof
    , property
    , scale
    , shrinkIntegral
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Time
    ( UniformTime )
import UnliftIO.Concurrent
    ( threadDelay )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = parallel $ do
    parallel $ describe "Pointless tests to cover 'Show' instances for errors" $ do
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

    parallel $ describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join"
            (property prop_guardJoinQuit)
        it "You can join if you cannot quit"
            (property prop_guardQuitJoin)

    parallel $ describe "Join/Quit Stake pool unit tests" $ do
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
     where
         pidA = PoolId "A"
         pidB = PoolId "B"
         pidUnknown = PoolId "unknown"
         knownPools = Set.fromList [pidA, pidB]
         next epoch dlgStatus =
             WalletDelegationNext {changesAt = epoch, status = dlgStatus}

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
walletCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture DBLayer{..} _wl walletIds _) <- setupFixture newWallet
    resFromDb <- atomically $ readCheckpoint (PrimaryKey $ L.head walletIds)
    resFromDb `shouldSatisfy` isJust

walletDoubleCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletDoubleCreationProp newWallet@(wid, wname, wstate) =
    monadicIO $ liftIO $ do
        (WalletLayerFixture _db wl _walletIds _) <- setupFixture newWallet
        secondTrial <- runExceptT $ W.createWallet wl wid wname wstate
        secondTrial `shouldSatisfy` isLeft

walletGetProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletGetProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl walletIds _) <- liftIO $ setupFixture newWallet
    resFromGet <- runExceptT $ W.readWallet wl (L.head walletIds)
    resFromGet `shouldSatisfy` isRight

walletGetWrongIdProp
    :: ((WalletId, WalletName, DummyState), WalletId)
    -> Property
walletGetWrongIdProp (newWallet@(wid, _, _), walletId) = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds _) <- liftIO $ setupFixture newWallet
    attempt <- runExceptT $ W.readWallet wl walletId
    attempt `shouldSatisfy` if wid /= walletId then isLeft else isRight

walletIdDeterministic
    :: (WalletId, WalletName, DummyState)
    -> Property
walletIdDeterministic newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA _) <- liftIO $ setupFixture newWallet
    (WalletLayerFixture _ _ widsB _) <- liftIO $ setupFixture newWallet
    widsA `shouldBe` widsB

walletIdInjective
    :: ((WalletId, WalletName, DummyState), (WalletId, WalletName, DummyState))
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA _) <- liftIO $ setupFixture walletA
    (WalletLayerFixture _ _ widsB _) <- liftIO $ setupFixture walletB
    widsA `shouldNotBe` widsB

walletUpdateName
    :: (WalletId, WalletName, DummyState)
    -> [WalletName]
    -> Property
walletUpdateName wallet@(_, wName0, _) names = monadicIO $ liftIO $ do
    (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
    unsafeRunExceptT $ forM_ names $ \wName ->
        W.updateWallet wl wid (\x -> x { name = wName })
    wName <- fmap (name . (\(_, b, _) -> b))
        <$> unsafeRunExceptT $ W.readWallet wl wid
    wName `shouldBe` last (wName0 : names)

walletUpdateNameNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> WalletName
    -> Property
walletUpdateNameNoSuchWallet wallet@(wid', _, _) wid wName =
    wid /= wid' ==> monadicIO $ liftIO $ do
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        attempt <- runExceptT $ W.updateWallet wl wid (\x -> x { name = wName })
        attempt `shouldBe` Left (ErrNoSuchWallet wid)

walletUpdatePassphrase
    :: (WalletId, WalletName, DummyState)
    -> Passphrase "raw"
    -> Maybe (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> Property
walletUpdatePassphrase wallet new mxprv = monadicIO $ liftIO $ do
    (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
    case mxprv of
        Nothing -> prop_withoutPrivateKey wl wid
        Just (xprv, pwd) -> prop_withPrivateKey wl wid (xprv, pwd)
  where
    prop_withoutPrivateKey wl wid = do
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (new, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        attempt `shouldBe` Left err

    prop_withPrivateKey wl wid (xprv, pwd) = do
        unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, new)
        attempt `shouldBe` Right ()

walletUpdatePassphraseWrong
    :: (WalletId, WalletName, DummyState)
    -> (ShelleyKey 'RootK XPrv, Passphrase "encryption")
    -> (Passphrase "raw", Passphrase "raw")
    -> Property
walletUpdatePassphraseWrong wallet (xprv, pwd) (old, new) =
    pwd /= coerce old ==> monadicIO $ liftIO $ do
        (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey
                $ ErrWithRootKeyWrongPassphrase wid
                ErrWrongPassphrase
        attempt `shouldBe` Left err

walletUpdatePassphraseNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> (Passphrase "raw", Passphrase "raw")
    -> Property
walletUpdatePassphraseNoSuchWallet wallet@(wid', _, _) wid (old, new) =
    wid /= wid' ==> monadicIO $ liftIO $ do
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        attempt `shouldBe` Left err

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
        (_,_,_,txOld) <- unsafeRunExceptT $
            W.signTransaction @_ @_ wl wid () credentials (coerce pwd) ctx selection
        unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, newPwd)
        (_,_,_,txNew) <- unsafeRunExceptT $
            W.signTransaction @_ @_ wl wid () credentials newPwd ctx selection
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
        (WalletLayerFixture DBLayer{..} wl _ slotNoTime) <- liftIO $ setupFixture wallet
        atomically $ unsafeRunExceptT $ putTxHistory (PrimaryKey wid) history
        txs <- unsafeRunExceptT $
            W.listTransactions @_ @_ @_ wl wid Nothing Nothing Nothing Descending
        length txs `shouldBe` L.length history
        -- With the 'Down'-wrapper, the sort is descending.
        txs `shouldBe` L.sortOn (Down . slotNo . txInfoMeta) txs
        -- Check transaction time calculation
        let times = Map.fromList [(txInfoId i, txInfoTime i) | i <- txs]
        let expTimes = Map.fromList $
                (\(tx, meta) -> (txId tx, slotNoTime (meta ^. #slotNo))) <$> history
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
        $ BalanceInsufficient
        $ BalanceInsufficientError TokenBundle.empty TokenBundle.empty

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

data WalletLayerFixture = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer IO DummyState ShelleyKey
    , _fixtureWalletLayer :: WalletLayer DummyState ShelleyKey
    , _fixtureWallet :: [WalletId]
    , _fixtureSlotNoTime :: SlotNo -> UTCTime
    }

setupFixture
    :: (WalletId, WalletName, DummyState)
    -> IO WalletLayerFixture
setupFixture (wid, wname, wstate) = do
    let nl = dummyNetworkLayer
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
        let tid = mkTxId inps' (outputsCovered cs) mempty Nothing
        let tx = Tx tid Nothing inps' (outputsCovered cs) mempty Nothing
        wit <- forM (inputsSelected cs) $ \(_, TxOut addr _) -> do
            (xprv, Passphrase pwd) <- withEither
                (ErrKeyNotFoundForAddress addr) $ keystore addr
            let (Hash sigData) = txId tx
            let sig = CC.unXSignature $ CC.sign pwd (getKey xprv) sigData
            return $ xpubToBytes (getKey $ publicKey xprv) <> sig

        -- (tx1, wit1) == (tx2, wit2) <==> fakebinary1 == fakebinary2
        let fakeBinary = SealedTx . B8.pack $ show (tx, wit)
        return (tx, fakeBinary)

    , initSelectionCriteria =
        error "dummyTransactionLayer: initSelectionCriteria not implemented"
    , calcMinimumCost =
        error "dummyTransactionLayer: calcMinimumCost not implemented"
    , calcMinimumCoinValue =
        error "dummyTransactionLayer: calcMinimumCoinValue not implemented"
    , assessTokenBundleSize =
        error "dummyTransactionLayer: assessTokenBundleSize not implemented"
    , decodeSignedTx =
        error "dummyTransactionLayer: decodeSignedTx not implemented"
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

dummyNetworkLayer :: Monad m => NetworkLayer m block
dummyNetworkLayer = NetworkLayer
    { nextBlocks =
        error "dummyNetworkLayer: nextBlocks not implemented"
    , initCursor =
        error "dummyNetworkLayer: initCursor not implemented"
    , destroyCursor =
        error "dummyNetworkLayer: destroyCursor not implemented"
    , cursorSlotNo =
        error "dummyNetworkLayer: cursorSlotNo not implemented"
    , currentNodeTip =
        pure dummyTip
    , currentNodeEra =
        pure (AnyCardanoEra AllegraEra)
    , currentProtocolParameters =
        pure (protocolParameters dummyNetworkParameters)
    , currentSlottingParameters =
        error "dummyNetworkLayer: currentSlottingParameters not implemented"
    , postTx =
        error "dummyNetworkLayer: postTx not implemented"
    , stakeDistribution =
        error "dummyNetworkLayer: stakeDistribution not implemented"
    , getAccountBalance =
        error "dummyNetworkLayer: getAccountBalance not implemented"
    , watchNodeTip =
        error "dummyNetworkLayer: watchNodeTip not implemented"
    , timeInterpreter = dummyTimeInterpreter
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
    arbitrary = genCoinLargePositive

instance Arbitrary Tx where
    shrink (Tx tid fees ins outs wdrls md) = mconcat
        [ [ Tx tid fees ins' outs  wdrls md
          | ins' <- shrinkList' ins
          ]

        , [ Tx tid fees ins  outs' wdrls md
          | outs' <- shrinkList' outs
          ]

        , [ Tx tid fees ins  outs (Map.fromList wdrls') md
          | wdrls' <- shrinkList' (Map.toList wdrls)
          ]

        , [ Tx tid fees ins  outs wdrls md'
          | md' <- shrink md
          ]
        ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]
    arbitrary = Tx
        <$> arbitrary
        <*> arbitrary
        <*> fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        <*> fmap (L.take 5 . getNonEmpty) arbitrary
        <*> fmap (Map.fromList . L.take 5) arbitrary
        <*> arbitrary

instance Arbitrary RewardAccount where
    arbitrary = RewardAccount . BS.pack <$> vector 28

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> (Hash . B8.pack <$> vector 32)
        <*> scale (`mod` 3) arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut (Address "address") . TokenBundle.fromCoin
        <$> genCoinLargePositive

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
    arbitrary = genTxMetadata
