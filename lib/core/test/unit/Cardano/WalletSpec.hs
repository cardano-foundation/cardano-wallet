{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet
    ( ErrCreateUnsignedTx (..)
    , ErrSignTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWithRootKey (..)
    , ErrWithRootKey (..)
    , WalletLayer (..)
    )
import Cardano.Wallet.DB
    ( DBLayer, ErrNoSuchWallet (..), PrimaryKey (..), putTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..), block0, genesisParameters )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , Index
    , Passphrase (..)
    , WalletKey (..)
    , XPrv
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( ChangeChain (..)
    , SeqKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , EpochNo (..)
    , Hash (..)
    , SlotId (..)
    , SlotNo (..)
    , SortOrder (..)
    , TransactionInfo (txInfoMeta)
    , TransactionInfo (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , TxWitness (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , flatSlot
    , txId
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( forM, forM_, replicateM, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
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
    ( Word32 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , Positive (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , elements
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Time
    ( UniformTime )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        let wid = WalletId (hash @ByteString "arbitrary")
        it (show $ ErrCreateUnsignedTxNoSuchWallet @() (ErrNoSuchWallet wid)) True
        it (show $ ErrSignTxNoSuchWallet (ErrNoSuchWallet wid)) True
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

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

walletCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture db _wl walletIds _) <- setupFixture newWallet
    resFromDb <- DB.readCheckpoint db (PrimaryKey $ L.head walletIds)
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
    -> Passphrase "encryption-new"
    -> Maybe (SeqKey 'RootK XPrv, Passphrase "encryption")
    -> Property
walletUpdatePassphrase wallet new mxprv = monadicIO $ liftIO $ do
    (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
    case mxprv of
        Nothing -> prop_withoutPrivateKey wl wid
        Just (xprv, pwd) -> prop_withPrivateKey wl wid (xprv, pwd)
  where
    prop_withoutPrivateKey wl wid = do
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (coerce new, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        attempt `shouldBe` Left err

    prop_withPrivateKey wl wid (xprv, pwd) = do
        unsafeRunExceptT $ W.attachPrivateKey wl wid (xprv, pwd)
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, new)
        attempt `shouldBe` Right ()

walletUpdatePassphraseWrong
    :: (WalletId, WalletName, DummyState)
    -> (SeqKey 'RootK XPrv, Passphrase "encryption")
    -> (Passphrase "encryption-old", Passphrase "encryption-new")
    -> Property
walletUpdatePassphraseWrong wallet (xprv, pwd) (old, new) =
    pwd /= coerce old ==> monadicIO $ liftIO $ do
        (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ W.attachPrivateKey wl wid (xprv, pwd)
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey
                $ ErrWithRootKeyWrongPassphrase wid
                ErrWrongPassphrase
        attempt `shouldBe` Left err

walletUpdatePassphraseNoSuchWallet
    :: (WalletId, WalletName, DummyState)
    -> WalletId
    -> (Passphrase "encryption-old", Passphrase "encryption-new")
    -> Property
walletUpdatePassphraseNoSuchWallet wallet@(wid', _, _) wid (old, new) =
    wid /= wid' ==> monadicIO $ liftIO $ do
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        attempt <- runExceptT $ W.updateWalletPassphrase wl wid (old, new)
        let err = ErrUpdatePassphraseWithRootKey $ ErrWithRootKeyNoRootKey wid
        attempt `shouldBe` Left err

walletUpdatePassphraseDate
    :: (WalletId, WalletName, DummyState)
    -> (SeqKey 'RootK XPrv, Passphrase "encryption")
    -> Property
walletUpdatePassphraseDate wallet (xprv, pwd) = monadicIO $ liftIO $ do
    (WalletLayerFixture _ wl [wid] _) <- liftIO $ setupFixture wallet
    let infoShouldSatisfy predicate = do
            info <- (passphraseInfo . (\(_,b,_) -> b)) <$>
                unsafeRunExceptT (W.readWallet wl wid)
            info `shouldSatisfy` predicate
            return info

    void $ infoShouldSatisfy isNothing
    unsafeRunExceptT $ W.attachPrivateKey wl wid (xprv, pwd)
    info <- infoShouldSatisfy isJust
    pause
    unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, coerce pwd)
    void $ infoShouldSatisfy (\info' -> isJust info' && info' > info)
  where
    pause = threadDelay 500

walletKeyIsReencrypted
    :: (WalletId, WalletName)
    -> (SeqKey 'RootK XPrv, Passphrase "encryption")
    -> Passphrase "encryption-new"
    -> Property
walletKeyIsReencrypted (wid, wname) (xprv, pwd) newPwd =
    monadicIO $ liftIO $ do
        let state = Map.insert (Address "source") minBound mempty
        let wallet = (wid, wname, DummyState state)
        (WalletLayerFixture _ wl _ _) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ W.attachPrivateKey wl wid (xprv, pwd)
        (_,_,[witOld]) <- unsafeRunExceptT $ W.signTx wl wid pwd selection
        unsafeRunExceptT $ W.updateWalletPassphrase wl wid (coerce pwd, newPwd)
        (_,_,[witNew]) <-
            unsafeRunExceptT $ W.signTx wl wid (coerce newPwd) selection
        witOld `shouldBe` witNew
  where
    selection = CoinSelection
        [ ( TxIn (Hash "eb4ab6028bd0ac971809d514c92db1") 1
          , TxOut (Address "source") (Coin 42)
          )
        ]
        [ TxOut (Address "destination") (Coin 14) ]
        []

walletListTransactionsSorted
    :: (WalletId, WalletName, DummyState)
    -> SortOrder
    -> (Maybe UniformTime, Maybe UniformTime)
    -> [(Tx, TxMeta)]
    -> Property
walletListTransactionsSorted wallet@(wid, _, _) _order (_mstart, _mend) history =
    monadicIO $ liftIO $ do
        (WalletLayerFixture db wl _ slotIdTime) <- liftIO $ setupFixture wallet
        unsafeRunExceptT $ putTxHistory db (PrimaryKey wid) history
        txs <- unsafeRunExceptT $
            W.listTransactions wl wid Nothing Nothing Descending
        length txs `shouldBe` L.length history
        -- With the 'Down'-wrapper, the sort is descending.
        txs `shouldBe` L.sortOn (Down . slotId . txInfoMeta) txs
        -- Check transaction time calculation
        let times = Map.fromList [(txInfoId i, txInfoTime i) | i <- txs]
        let expTimes = Map.fromList $
                (\(tx, meta) -> (txId @DummyTarget tx, slotIdTime (meta ^. #slotId))) <$> history
        times `shouldBe` expTimes

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

data WalletLayerFixture = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer IO DummyState DummyTarget SeqKey
    , _fixtureWalletLayer :: WalletLayer DummyState DummyTarget SeqKey
    , _fixtureWallet :: [WalletId]
    , _fixtureSlotIdTime :: SlotId -> UTCTime
    }

setupFixture
    :: (WalletId, WalletName, DummyState)
    -> IO WalletLayerFixture
setupFixture (wid, wname, wstate) = do
    let nl = error "NetworkLayer"
    let tl = dummyTransactionLayer
    db <- MVar.newDBLayer
    let wl = WalletLayer nullTracer (block0, bp) nl tl db
    res <- runExceptT $ W.createWallet wl wid wname wstate
    let wal = case res of
            Left _ -> []
            Right walletId -> [walletId]
    pure $ WalletLayerFixture db wl wal slotIdTime
  where
    slotNo = flatSlot (getEpochLength bp)
    slotIdTime = posixSecondsToUTCTime . fromIntegral . slotNo
    bp = genesisParameters

-- | A dummy transaction layer to see the effect of a root private key. It
-- implements a fake signer that still produces sort of witnesses
dummyTransactionLayer :: TransactionLayer DummyTarget SeqKey
dummyTransactionLayer = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let tx = Tx (fmap fst inps) outs
        wit <- forM inps $ \(_, TxOut addr _) -> do
            (xprv, Passphrase pwd) <- withEither
                (ErrKeyNotFoundForAddress addr) $ keyFrom addr
            let (Hash sigData) = txId @DummyTarget tx
            let sig = CC.unXSignature $ CC.sign pwd (getKey xprv) sigData
            return $ TxWitness
                (CC.unXPub (getKey $ publicKey xprv) <> sig)
        return (tx, wit)
    , estimateSize =
        error "dummyTransactionLayer: estimateSize not implemented"
    , estimateMaxNumberOfInputs =
        error "dummyTransactionLayer: estimateMaxNumberOfInputs not implemented"
    , validateSelection =
        error "dummyTransactionLayer: validateSelection not implemented"
    , decodeSignedTx =
        error "dummyTransactionLayer: decodeSignedTx not implemented"
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

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

instance IsOurs DummyState where
    isOurs _ s = (True, s)

instance IsOwned DummyState SeqKey where
    isOwned (DummyState m) (rootK, pwd) addr = do
        ix <- Map.lookup addr m
        let accXPrv = deriveAccountPrivateKey pwd rootK minBound
        let addrXPrv = deriveAddressPrivateKey pwd accXPrv ExternalChain ix
        return (addrXPrv, pwd)

instance GenChange DummyState where
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

instance {-# OVERLAPS #-} Arbitrary (SeqKey 'RootK XPrv, Passphrase "encryption")
  where
    shrink _ = []
    arbitrary = do
        seed <- Passphrase . BA.convert . BS.pack <$> replicateM 32 arbitrary
        pwd <- arbitrary
        let key = generateKeyFromSeed (seed, mempty) pwd
        return (key, pwd)

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = SlotId <$> arbitrary <*> arbitrary

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary SortOrder where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Show XPrv where
    show = show . CC.unXPrv

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary =
        Hash . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Tx where
    shrink (Tx ins outs) =
        [Tx ins' outs | ins' <- shrinkList' ins ] ++
        [Tx ins outs' | outs' <- shrinkList' outs ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]
    arbitrary = Tx
        <$> fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        <*> fmap (L.take 5 . getNonEmpty) arbitrary

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> (Hash . B8.pack <$> vectorOf 32 arbitrary)
        <*> scale (`mod` 3) arbitrary

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> pure (Address "address")
        <*> (Coin <$> choose (1, 100000))

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> elements [Pending, InLedger, Invalidated]
        <*> elements [Incoming, Outgoing]
        <*> (SlotId
            <$> (EpochNo <$> choose (0, 1000))
            <*> (SlotNo <$> choose (0, 21599)))
        <*> fmap Quantity arbitrary
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)
