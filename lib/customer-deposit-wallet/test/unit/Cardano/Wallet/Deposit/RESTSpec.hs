module Cardano.Wallet.Deposit.RESTSpec
    ( spec
    )
where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    , generate
    , toXPub
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (WalletBootEnv)
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ErrResourceMissing (..)
    , withResource
    )
import Cardano.Wallet.Deposit.REST
    ( ErrCreatingDatabase (..)
    , ErrDatabase (..)
    , ErrLoadingDatabase (..)
    , ErrWalletResource (..)
    , WalletResourceM
    , availableBalance
    , initXPubWallet
    , loadWallet
    , runWalletResourceM
    , walletExists
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
    , shouldBe
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.ByteString.Char8 as B8

fakeBootEnv :: WalletBootEnv IO
fakeBootEnv = WalletBootEnv undefined undefined undefined

xpub :: XPub
xpub =
    toXPub
        $ generate (B8.pack "random seed for a testing xpub lala") B8.empty

letItInitialize :: WalletResourceM ()
letItInitialize = liftIO $ threadDelay 100000

onSuccess :: (Show e, MonadFail m) => Either e a -> (a -> m b) -> m b
onSuccess (Left e) _ = fail $ show e
onSuccess (Right a) f = f a

matchEmptyValue :: Show e => Either e Read.Value -> IO ()
matchEmptyValue x = onSuccess x $ \v -> v `shouldBe` mempty

withWallet :: WalletResourceM a -> IO (Either ErrWalletResource a)
withWallet f = withResource $ runWalletResourceM f

withInitializedWallet
    :: FilePath
    -> WalletResourceM a
    -> IO (Either ErrWalletResource a)
withInitializedWallet dir f = withWallet $ do
    initXPubWallet nullTracer fakeBootEnv dir xpub 0
    letItInitialize
    f

withLoadedWallet
    :: FilePath
    -> WalletResourceM a
    -> IO (Either ErrWalletResource a)
withLoadedWallet dir f = withWallet $ do
    loadWallet fakeBootEnv dir
    letItInitialize
    f

doNothing :: WalletResourceM ()
doNothing = pure ()

inADirectory :: (FilePath -> IO a) -> IO a
inADirectory = withSystemTempDirectory "deposit-rest"

spec :: Spec
spec = do
    describe "REST Deposit interface" $ do
        it "can initialize a wallet"
            $ inADirectory
            $ \dir -> do
                val <- withInitializedWallet dir availableBalance
                matchEmptyValue val
        it "can load an existing wallet"
            $ inADirectory
            $ \dir -> do
                val <- withInitializedWallet dir availableBalance
                onSuccess val $ \_ -> do
                    val' <- withLoadedWallet dir availableBalance
                    matchEmptyValue val'
        it "cannot re-initialize a wallet"
            $ inADirectory
            $ \dir -> do
                val <- withInitializedWallet dir doNothing
                onSuccess val $ \_ -> do
                    val' <- withInitializedWallet dir availableBalance
                    case val' of
                        Left
                            ( ErrNoWallet
                                    ( ErrFailedToInitialize
                                            ( ErrCreatingDatabase
                                                    (ErrDatabaseAlreadyExists fp)
                                                )
                                        )
                                )
                                | dir == fp -> pure ()
                        Left e -> fail $ show e
                        Right _ -> fail "Should have failed the query on re-init"
        it "cannot load a non-existing wallet"
            $ inADirectory
            $ \dir -> do
                val <- withLoadedWallet dir availableBalance
                case val of
                    Left
                        ( ErrNoWallet
                                ( ErrFailedToInitialize
                                        ( ErrLoadingDatabase
                                                (ErrDatabaseNotFound fp)
                                            )
                                    )
                            )
                            | dir == fp -> pure ()
                    Left e -> fail $ show e
                    Right _ -> fail "Should have failed the query on load"
        it "can check if a wallet is present"
            $ inADirectory
            $ \dir -> do
                r <- withInitializedWallet dir doNothing
                onSuccess r $ \_ -> do
                    presence <- withWallet $ walletExists dir
                    onSuccess presence $ \p -> p `shouldBe` True
