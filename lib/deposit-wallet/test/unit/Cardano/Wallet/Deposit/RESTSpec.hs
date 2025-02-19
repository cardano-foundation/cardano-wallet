{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Wallet.Deposit.RESTSpec
    ( spec
    )
where

import Prelude

import Cardano.Crypto.Wallet
    ( sign
    , verify
    , xPrvChangePass
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (WalletBootEnv)
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( ErrResourceMissing (..)
    , withResource
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( Credentials
    , accountXPubFromCredentials
    , createMnemonicFromWords
    , credentialsFromMnemonics
    , deriveAccountXPrv
    , rootXPrvFromCredentials
    )
import Cardano.Wallet.Deposit.REST
    ( ErrCreatingDatabase (..)
    , ErrDatabase (..)
    , ErrLoadingDatabase (..)
    , ErrWalletResource (..)
    , WalletResourceM
    , availableBalance
    , initWallet
    , loadWallet
    , runWalletResourceM
    , walletExists
    )
import Codec.Serialise
    ( deserialise
    , serialise
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Monad.Trans.Cont
    ( cont
    , evalCont
    )
import Control.Tracer
    ( nullTracer
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromJust
    )
import Data.Text
    ( Text
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
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , forAll
    , listOf
    , (===)
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

fakeBootEnv :: WalletBootEnv IO
fakeBootEnv = WalletBootEnv nullTracer Read.mockGenesisDataMainnet undefined

seed :: SomeMnemonic
Right seed =
    createMnemonicFromWords
        "vital minimum victory start lunch find city peanut shiver soft hedgehog artwork mushroom loud found"

credentials :: Credentials
credentials = credentialsFromMnemonics seed mempty

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
    initWallet nullTracer nullTracer fakeBootEnv dir credentials 0
    letItInitialize
    f

withLoadedWallet
    :: FilePath
    -> WalletResourceM a
    -> IO (Either ErrWalletResource a)
withLoadedWallet dir f = withWallet $ do
    loadWallet nullTracer fakeBootEnv dir
    letItInitialize
    f

doNothing :: WalletResourceM ()
doNothing = pure ()

inADirectory :: (FilePath -> IO a) -> IO a
inADirectory = withSystemTempDirectory "deposit-rest"

byteStringGen :: Gen ByteString
byteStringGen = B8.pack <$> listOf arbitrary

textGen :: Gen Text
textGen = T.pack <$> listOf arbitrary

words15 :: [Text]
words15 =
    [ "soap retire song hat major steak stuff daughter half scorpion please brisk decade hill song"
    , "sure cannon broom caution artist legend boring reveal scene rubber weapon chest page clog fine"
    , "fruit garden saddle upper huge educate fabric ocean bamboo verb iron apple have deposit trap"
    ]

credentialsGen :: Gen (Credentials, Text)
credentialsGen = do
    mnemonics' <- elements words15
    case createMnemonicFromWords mnemonics' of
        Left e -> error $ "Invalid mnemonics: " <> show e
        Right seed' -> do
            passphrase' <- textGen
            pure (credentialsFromMnemonics seed' passphrase', passphrase')

spec :: Spec
spec = do
    describe "XPub" $ do
        it "can be serialised and deserialised" $ do
            forAll credentialsGen $ \(credentials', _) ->
                deserialise (serialise $ accountXPubFromCredentials credentials')
                    === accountXPubFromCredentials credentials'
    describe "XPrv" $ do
        it "can be serialised and deserialised" $ do
            forAll credentialsGen $ \(credentials', _) ->
                deserialise (serialise $ rootXPrvFromCredentials credentials')
                    === rootXPrvFromCredentials credentials'
    describe "Credentials" $ do
        it "can be serialised and deserialised" $ do
            forAll credentialsGen $ \(credentials', _) ->
                deserialise (serialise credentials') === credentials'
    describe "Credentials with mnemonics" $ do
        it "can sign and verify a message" $ evalCont $ do
            (credentials', passphrase') <- cont $ forAll credentialsGen
            message <- cont $ forAll byteStringGen
            let
                decryptXPrv =
                    xPrvChangePass (T.encodeUtf8 passphrase') B8.empty
                xprv =
                    deriveAccountXPrv
                        $ decryptXPrv
                        $ fromJust
                        $ rootXPrvFromCredentials credentials'
                sig = sign B8.empty xprv message
            pure
                $ verify (accountXPubFromCredentials credentials') message sig
                    === True

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
        it "can check if a wallet is present on disk"
            $ inADirectory
            $ \dir -> do
                r <- withInitializedWallet dir doNothing
                onSuccess r $ \_ -> do
                    presence <- walletExists dir
                    presence `shouldBe` True
