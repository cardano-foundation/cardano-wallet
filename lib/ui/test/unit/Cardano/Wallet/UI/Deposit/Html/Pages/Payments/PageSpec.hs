{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Payments.PageSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (WalletBootEnv)
    , networkEnv
    )
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    , mapBlock
    , postTx
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( withResource
    )
import Cardano.Wallet.Deposit.Pure
    ( Credentials
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( InspectTx (..)
    )
import Cardano.Wallet.Deposit.REST
    ( ErrWalletResource (..)
    , WalletResourceM
    , customerAddress
    , initWallet
    , inspectTx
    , resolveCurrentEraTx
    , runWalletResourceM
    )
import Cardano.Wallet.Deposit.Write
    ( addTxOut
    , emptyTxBody
    , mkAda
    , mkTx
    , mkTxOut
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( unsigned
    )
import Cardano.Wallet.UI.Deposit.Handlers.Payments.Transaction
    ( deserializeTransaction
    , mkPayment
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( nullTracer
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldNotBe
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import Control.Monad.Except
    ( runExceptT
    )

fakeBootEnv :: IO (WalletBootEnv IO)
fakeBootEnv = do
    net <- mapBlock Read.EraValue <$> newNetworkEnvMock
    pure $ WalletBootEnv nullTracer Read.mockGenesisDataMainnet net

credentials :: Credentials
credentials =
    credentialsFromMnemonics "random seed for a testing xpub lala" mempty

letItInitialize :: WalletResourceM ()
letItInitialize = liftIO $ threadDelay 100_000

onSuccess :: (Show e, MonadFail m) => Either e a -> (a -> m b) -> m b
onSuccess (Left e) _ = fail $ show e
onSuccess (Right a) f = f a

withWallet :: WalletResourceM a -> IO (Either ErrWalletResource a)
withWallet f = withResource $ runWalletResourceM f

withInitializedWallet
    :: WalletResourceM a
    -> IO (Either ErrWalletResource a)
withInitializedWallet f =
    withSystemTempDirectory "wallet-ui" $ \dir -> do
        bootEnv <- fakeBootEnv
        withWallet $ do
            initWallet nullTracer bootEnv dir credentials 1
            letItInitialize
            fundTheWallet (networkEnv bootEnv)
            f

fundTheWallet :: NetworkEnv IO z -> WalletResourceM ()
fundTheWallet network = do
    Just address <- customerAddress 0
    let tx =
            mkTx
                $ fst
                $ addTxOut (mkTxOut address (mkAda 1_000_000)) emptyTxBody
    Right () <- liftIO $ postTx network tx
    pure ()

spec :: Spec
spec = do
    describe "payment" $ do
        it "can create a transaction with no receivers" $ do
            etx <- withInitializedWallet $ do
                etx <- runExceptT $ unsigned mkPayment mempty
                onSuccess etx $ \tx -> do
                    onSuccess (deserializeTransaction tx) $ \dtx -> do
                        tx' <- resolveCurrentEraTx dtx
                        inspectTx tx'
            onSuccess etx $ \InspectTx{..} -> do
                change `shouldNotBe` []
                ourInputs `shouldNotBe` []
                fee `shouldNotBe` 0
