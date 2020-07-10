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
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet
    ( ErrSelectForPayment (..)
    , ErrSignPayment (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , ErrWithRootKey (..)
    , WalletLayer (..)
    )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrNoSuchWallet (..), PrimaryKey (..), putTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, block0, dummyNetworkParameters, mkTxId )
import Cardano.Wallet.Gen
    ( genMnemonic )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , Depth (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , HardDerivation (..)
    , Index
    , Passphrase (..)
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection, feeBalance )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (BlockHeader)
    , ChimericAccount (..)
    , Coin (..)
    , Direction (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , Hash (..)
    , NetworkParameters (..)
    , PoolId (..)
    , SealedTx (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SortOrder (..)
    , TransactionInfo (txInfoMeta)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , WalletDelegation (..)
    , WalletDelegationNext (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , flatSlot
    , txId
    , unsafeEpochNo
    )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM, forM_, replicateM, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Monad.Trans.State.Strict
    ( State, evalState, state )
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
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust, isNothing )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , applyArbitrary2
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , choose
    , counterexample
    , elements
    , label
    , oneof
    , property
    , scale
    , shrinkIntegral
    , vector
    , withMaxSuccess
    , (.&&.)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Time
    ( UniformTime )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        let wid = WalletId (hash @ByteString "arbitrary")
        it (show $ ErrSelectForPaymentNoSuchWallet @() (ErrNoSuchWallet wid)) True
        it (show $ ErrSignPaymentNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrSubmitTxNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrUpdatePassphraseNoSuchWallet (ErrNoSuchWallet wid)) True
        it (show $ ErrWithRootKeyWrongPassphrase wid ErrWrongPassphrase) True

    describe "WalletLayer works as expected" $ do
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

    describe "Tx fee estimation" $
        it "Fee estimates are sound"
            (property prop_estimateFee)

    describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join"
            (property prop_guardJoinQuit)
        it "You can join if you cannot quit"
            (property prop_guardQuitJoin)

    describe "Join/Quit Stake pool unit tests" $ do
        it "Cannot join A, when active = A" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            W.guardJoin knownPools dlg pidA
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join A, when next = [A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            W.guardJoin knownPools dlg pidA
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can join A, when active = A, next = [B]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let dlg = WalletDelegation {active = Delegating pidA, next = [next1]}
            W.guardJoin knownPools dlg pidA `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let next2 = next (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            W.guardJoin knownPools dlg pidA
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join when pool is unknown" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            W.guardJoin knownPools dlg pidUnknown
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
        it "Cannot quit when active: not_delegating, next = []" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            W.guardQuit dlg (Quantity 0) `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [not_delegating]" $ do
            let next1 = next (EpochNo 1) NotDelegating
            let dlg = WalletDelegation {active = Delegating pidA, next = [next1]}
            W.guardQuit dlg (Quantity 0) `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [B, not_delegating]" $ do
            let next1 = next (EpochNo 1) (Delegating pidB)
            let next2 = next (EpochNo 2) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            W.guardQuit dlg (Quantity 0) `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Can quit when active: not_delegating, next = [A]" $ do
            let next1 = next (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = NotDelegating, next = [next1]}
            W.guardQuit dlg (Quantity 0) `shouldBe` Right ()
     where
         pidA = PoolId "A"
         pidB = PoolId "B"
         pidUnknown = PoolId "unknown"
         knownPools = [pidA, pidB]
         next epoch dlgStatus =
             WalletDelegationNext {changesAt = epoch, status = dlgStatus}


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

prop_guardJoinQuit
    :: [PoolId]
    -> WalletDelegation
    -> PoolId
    -> Property
prop_guardJoinQuit knownPools dlg pid =
    case W.guardJoin knownPools dlg pid of
        Right () ->
            label "I can join" $ property True
        Left W.ErrNoSuchPool{}  ->
            label "ErrNoSuchPool" $ property True
        Left W.ErrAlreadyDelegating{} ->
            label "ErrAlreadyDelegating"
                (W.guardQuit dlg (Quantity 0) === Right ())

prop_guardQuitJoin
    :: NonEmptyList PoolId
    -> WalletDelegation
    -> Word64
    -> Property
prop_guardQuitJoin (NonEmpty knownPools) dlg rewards =
    case W.guardQuit dlg (Quantity rewards) of
        Right () ->
            label "I can quit" $ property True
        Left W.ErrNotDelegatingOrAboutTo ->
            label "ErrNotDelegatingOrAboutTo"
                (W.guardJoin knownPools dlg (last knownPools) === Right ())
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
walletGetWrongIdProp (newWallet, corruptedWalletId) = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds _) <- liftIO $ setupFixture newWallet
    attempt <- runExceptT $ W.readWallet wl corruptedWalletId
    attempt `shouldSatisfy` isLeft

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
    -> Maybe (JormungandrKey 'RootK XPrv, Passphrase "encryption")
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
    -> (JormungandrKey 'RootK XPrv, Passphrase "encryption")
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
    -> (JormungandrKey 'RootK XPrv, Passphrase "encryption")
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
    -> (JormungandrKey 'RootK XPrv, Passphrase "encryption")
    -> Passphrase "raw"
    -> Property
walletKeyIsReencrypted (wid, wname) (xprv, pwd) newPwd =
    monadicIO $ liftIO $ do
        let st = Map.insert (Address "source") minBound mempty
        let wallet = (wid, wname, DummyState st)
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ W.attachPrivateKeyFromPwd wl wid (xprv, pwd)
        (_,_,_,txOld) <-
            unsafeRunExceptT $ W.signPayment @_ @_ @DummyTarget wl wid () (coerce pwd) selection
        unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, newPwd)
        (_,_,_,txNew) <-
            unsafeRunExceptT $ W.signPayment @_ @_ @DummyTarget wl wid () newPwd selection
        txOld `shouldBe` txNew
  where
    selection = mempty
        { CS.inputs =
            [ ( TxIn (Hash "eb4ab6028bd0ac971809d514c92db1") 1
              , TxOut (Address "source") (Coin 42)
              )
            ]
        , CS.outputs =
            [ TxOut (Address "destination") (Coin 14) ]
        }

walletListTransactionsSorted
    :: (WalletId, WalletName, DummyState)
    -> SortOrder
    -> (Maybe UniformTime, Maybe UniformTime)
    -> [(Tx, TxMeta)]
    -> Property
walletListTransactionsSorted wallet@(wid, _, _) _order (_mstart, _mend) history =
    monadicIO $ liftIO $ do
        (WalletLayerFixture DBLayer{..} wl _ slotIdTime) <- liftIO $ setupFixture wallet
        atomically $ unsafeRunExceptT $ putTxHistory (PrimaryKey wid) history
        txs <- unsafeRunExceptT $
            W.listTransactions wl wid Nothing Nothing Descending
        length txs `shouldBe` L.length history
        -- With the 'Down'-wrapper, the sort is descending.
        txs `shouldBe` L.sortOn (Down . slotId . txInfoMeta) txs
        -- Check transaction time calculation
        let times = Map.fromList [(txInfoId i, txInfoTime i) | i <- txs]
        let expTimes = Map.fromList $
                (\(tx, meta) -> (txId tx, slotIdTime (meta ^. #slotId))) <$> history
        times `shouldBe` expTimes


{-------------------------------------------------------------------------------
                        Properties of tx fee estimation
-------------------------------------------------------------------------------}

-- | Properties of 'estimateFeeForCoinSelection':
-- 1. There is no coin selection with a fee above the estimated maximum.
-- 2. The minimum estimated fee is no greater than the maximum estimated fee.
-- 3. Around 10% of fees are below the estimated minimum.
prop_estimateFee :: NonEmptyList (Either String FeeGen) -> Property
prop_estimateFee (NonEmpty results) = case actual of
    Left err -> label "errors: all" $
        Left err === head results
    Right estimation@(W.FeeEstimation minFee maxFee) ->
        label ("errors: " <> if any isLeft results then "some" else "none") $
        counterexample (show estimation) $
            maxFee <= maximum (map (getRight 0) results) .&&.
            minFee <= maxFee .&&.
            (proportionBelow minFee results `closeTo` (1/10 :: Double))
  where
    actual :: Either String W.FeeEstimation
    actual = runTest results' (W.estimateFeeForCoinSelection mockCoinSelection)

    -- infinite list of CoinSelections (or errors) matching the given fee
    -- amounts.
    results' = fmap coinSelectionForFee <$> L.cycle results

    -- Pops a pre-canned result off the state and returns it
    mockCoinSelection
        :: ExceptT String (State [Either String CoinSelection]) Fee
    mockCoinSelection = fmap (Fee . feeBalance) $ ExceptT $ state (\(r:rs) -> (r,rs))

    runTest vals action = evalState (runExceptT action) vals

    -- Find the number of results below the "minimum" estimate.
    countBelow minFee =
        count ((< minFee) . getRight maxBound)
    proportionBelow minFee xs = fromIntegral (countBelow minFee xs)
        / fromIntegral (count isRight xs)
    count p = length . filter p

    -- get the coin amount from a Right FeeGen, or a default value otherwise.
    getRight d = either (const d) (getCoin . unFeeGen)

    -- Two fractions are close to each other if they are within 20% either way.
    closeTo a b =
        counterexample (show a <> " & " <> show b <> " are not close enough") $
        property $ abs (a - b) < (1/5)

-- | A fee amount that has a uniform random distribution in the range 1-100.
newtype FeeGen = FeeGen { unFeeGen :: Coin } deriving (Show, Eq)

instance Arbitrary FeeGen where
    arbitrary = FeeGen . Coin <$> choose (1,100)

-- | Manufacture a coin selection that would result in the given fee.
coinSelectionForFee :: FeeGen -> CoinSelection
coinSelectionForFee (FeeGen (Coin fee)) = mempty
    { CS.inputs  = [(TxIn (Hash "") 0, TxOut (Address "") (Coin (1 + fee)))]
    , CS.outputs = [TxOut (Address "") (Coin 1)]
    }
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

data WalletLayerFixture = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer IO DummyState JormungandrKey
    , _fixtureWalletLayer :: WalletLayer DummyState DummyTarget JormungandrKey
    , _fixtureWallet :: [WalletId]
    , _fixtureSlotIdTime :: SlotId -> UTCTime
    }

setupFixture
    :: (WalletId, WalletName, DummyState)
    -> IO WalletLayerFixture
setupFixture (wid, wname, wstate) = do
    let nl = dummyNetworkLayer
    let tl = dummyTransactionLayer
    db <- MVar.newDBLayer
    let wl = WalletLayer nullTracer (block0, np, st) nl tl db
    res <- runExceptT $ W.createWallet wl wid wname wstate
    let wal = case res of
            Left _ -> []
            Right walletId -> [walletId]
    pure $ WalletLayerFixture db wl wal slotIdTime
  where
    slotNo = flatSlot $ getEpochLength $ genesisParameters np
    slotIdTime = posixSecondsToUTCTime . fromIntegral . slotNo
    np = dummyNetworkParameters
    st = SyncTolerance 10

-- | A dummy transaction layer to see the effect of a root private key. It
-- implements a fake signer that still produces sort of witnesses
dummyTransactionLayer :: TransactionLayer DummyTarget JormungandrKey
dummyTransactionLayer = TransactionLayer
    { mkStdTx = \_ keyFrom _slot cs -> do
        let inps' = map (second coin) (CS.inputs cs)
        let tid = mkTxId inps' (CS.outputs cs)
        let tx = Tx tid inps' (CS.outputs cs)
        wit <- forM (CS.inputs cs) $ \(_, TxOut addr _) -> do
            (xprv, Passphrase pwd) <- withEither
                (ErrKeyNotFoundForAddress addr) $ keyFrom addr
            let (Hash sigData) = txId tx
            let sig = CC.unXSignature $ CC.sign pwd (getKey xprv) sigData
            return $ xpubToBytes (getKey $ publicKey xprv) <> sig

        -- (tx1, wit1) == (tx2, wit2) <==> fakebinary1 == fakebinary2
        let fakeBinary = SealedTx . B8.pack $ show (tx, wit)
        return (tx, fakeBinary)
    , initDelegationSelection =
        error "dummyTransactionLayer: initDelegationSelection not implemented"
    , mkDelegationJoinTx =
        error "dummyTransactionLayer: mkDelegationJoinTx not implemented"
    , mkDelegationQuitTx =
        error "dummyTransactionLayer: mkDelegationQuitTx not implemented"
    , minimumFee =
        error "dummyTransactionLayer: minimumFee not implemented"
    , estimateMaxNumberOfInputs =
        error "dummyTransactionLayer: estimateMaxNumberOfInputs not implemented"
    , validateSelection =
        error "dummyTransactionLayer: validateSelection not implemented"
    , decodeSignedTx =
        error "dummyTransactionLayer: decodeSignedTx not implemented"
    , allowUnbalancedTx =
        False
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

dummyNetworkLayer :: Monad m => NetworkLayer m t block
dummyNetworkLayer = NetworkLayer
    { nextBlocks =
        error "dummyNetworkLayer: nextBlocks not implemented"
    , initCursor =
        error "dummyNetworkLayer: initCursor not implemented"
    , destroyCursor =
        error "dummyNetworkLayer: destroyCursor not implemented"
    , cursorSlotId =
        error "dummyNetworkLayer: cursorSlotId not implemented"
    , currentNodeTip =
        pure dummyTip
    , getProtocolParameters =
        error "dummyNetworkLayer: getProtocolParameters not implemented"
    , postTx =
        error "dummyNetworkLayer: postTx not implemented"
    , stakeDistribution =
        error "dummyNetworkLayer: stakeDistribution not implemented"
    , getAccountBalance =
        error "dummyNetworkLayer: getAccountBalance not implemented"
    , watchNodeTip =
        error "dummyNetworkLayer: watchNodeTip not implemented"
    }
  where
    dummyTip = BlockHeader (SlotId 0 0) (Quantity 0) dummyHash dummyHash
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
    isOurs _ s = (True, s)

instance IsOurs DummyState ChimericAccount where
    isOurs _ s = (False, s)

instance IsOwned DummyState JormungandrKey where
    isOwned (DummyState m) (rootK, pwd) addr = do
        ix <- Map.lookup addr m
        let accXPrv = deriveAccountPrivateKey pwd rootK minBound
        let addrXPrv = deriveAddressPrivateKey pwd accXPrv UTxOExternal ix
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

instance {-# OVERLAPS #-} Arbitrary (JormungandrKey 'RootK XPrv, Passphrase "encryption")
  where
    shrink _ = []
    arbitrary = do
        pwd <- arbitrary
        mw <- arbitrary
        let key = generateKeyFromSeed (mw, Nothing) pwd
        return (key, pwd)

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = applyArbitrary2 SlotId

instance Arbitrary SlotInEpoch where
    shrink (SlotInEpoch x) = SlotInEpoch <$> shrink x
    arbitrary = SlotInEpoch <$> arbitrary

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
    arbitrary = Coin <$> arbitrary

instance Arbitrary Tx where
    shrink (Tx tid ins outs) =
        [Tx tid ins' outs | ins' <- shrinkList' ins ] ++
        [Tx tid ins outs' | outs' <- shrinkList' outs ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]
    arbitrary = Tx
        <$> arbitrary
        <*> fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        <*> fmap (L.take 5 . getNonEmpty) arbitrary

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> (Hash . B8.pack <$> vector 32)
        <*> scale (`mod` 3) arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut (Address "address") . Coin <$> choose (1, 100000)

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger]
        <*> elements [Incoming, Outgoing]
        <*> (SlotId
            <$> (unsafeEpochNo <$> choose (0, 1000))
            <*> (SlotInEpoch <$> choose (0, 21599)))
        <*> fmap Quantity arbitrary
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)
