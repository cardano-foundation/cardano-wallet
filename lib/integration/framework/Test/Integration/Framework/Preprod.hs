{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Framework.Preprod
    ( PreprodSetupLog(..)
    , setupPreprodWallets
    , fixturePreprodWallets
    ) where

import Prelude

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Map as Map
import qualified Network.HTTP.Types.Status as HTTP

import Cardano.Mnemonic
    ( SomeMnemonic (..)
    , mnemonicToText
    )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation
    , ApiWallet
    , WalletStyle (..)
    , getApiT
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..)
    )
import Control.Monad
    ( unless
    , void
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO
    )
import Control.Retry
    ( RetryStatus
    , capDelay
    , fibonacciBackoff
    , retrying
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Aeson.QQ
    ( aesonQQ
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Labels
    ()
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import GHC.Stack
    ( HasCallStack
    )
import Test.Hspec.Expectations.Lifted
    ( expectationFailure
    , shouldBe
    , shouldContain
    )
import Test.Integration.Framework.Context
    ( Context (..)
    )
import Test.Integration.Framework.DSL
    ( Headers (Default)
    , Payload (Empty, Json)
    , eventuallyUsingDelay
    , fixturePassphrase
    , getResponse
    , oneSecond
    )
import Test.Integration.Framework.Request
    ( RequestException
    , request
    )

data PreprodSetupLog
    = WaitingForNodeConnection
    | CreatingWallets
    | WaitingForWalletsToSync
    | PreprodSetupReady

setupPreprodWallets :: Tracer IO PreprodSetupLog -> [SomeMnemonic] -> Context -> IO Context
setupPreprodWallets tr mnemonics ctx = do
    traceWith tr WaitingForNodeConnection
    waitForConnection

    traceWith tr CreatingWallets
    createWalletsIfNeeded mnemonics

    wallets <- getResponse <$> request @[ApiWallet] ctx
        (Link.listWallets @'Shelley) Default Empty
    unless (length wallets == length mnemonics) $
        expectationFailure $ unwords
            [ "Setup error: have", show (length mnemonics), "mnemonics but only"
            , show wallets, "have now been created."
            ]

    let walletIds = map (getApiT . view #id) wallets

    traceWith tr WaitingForWalletsToSync
    waitForAllWalletsToSync
    traceWith tr PreprodSetupReady

    pure ctx{ _preprodWallets = walletIds }
  where
    createWalletsIfNeeded :: [SomeMnemonic] -> IO ()
    createWalletsIfNeeded = mapM_ createWalletIfNeeded

    createWalletIfNeeded :: SomeMnemonic -> IO ()
    createWalletIfNeeded (SomeMnemonic mnemonic) = do
        (s, response) <- request @ApiWallet ctx (Link.postWallet @'Shelley) Default $ Json
            [aesonQQ| {
                "name": "Faucet Wallet",
                "mnemonic_sentence": #{mnemonicToText mnemonic},
                "passphrase": #{fixturePassphrase}
            } |]
        case response of
            Right _ | s == HTTP.status201 -> pure ()
            Left e | s == HTTP.status409 -> show e `shouldContain` "wallet_already_exists"
            _ -> expectationFailure $ unwords
                [ "setupPreprodWallets, createWalletsIfNeeded: unexpected response"
                , show (s, response)
                ]

    waitForConnection :: IO ()
    waitForConnection = void $ retrying
        (capDelay (30*oneSecond) $ fibonacciBackoff oneSecond)
        shouldWait
        (\_retryStatus -> request @ApiNetworkInformation ctx
          Link.getNetworkInfo Default Empty)
      where
        shouldWait
            :: RetryStatus
            -> (HTTP.Status, Either RequestException ApiNetworkInformation)
            -> IO Bool
        shouldWait _ (_, Right info) = pure $ info ^. #syncProgress . #getApiT <= NotResponding
        shouldWait _ (_, Left _err) = pure True

    waitForAllWalletsToSync :: IO ()
    waitForAllWalletsToSync = do
        eventuallyUsingDelay (5 * s) (90 * minutes) "setupPreprodWallets: all wallets are synced" $ do
            wallets :: [ApiWallet] <- getResponse <$> request @[ApiWallet] ctx
                (Link.listWallets @'Shelley) Default Empty
            let progress = map (getApiT . view #state) wallets
            all (== Ready) progress `shouldBe` True
      where
        s = 1_000_000
        minutes = 60

fixturePreprodWallets :: (HasCallStack, MonadUnliftIO m) => Context -> m [ApiWallet]
fixturePreprodWallets ctx = do
    wallets <- getResponse <$> request @[ApiWallet] ctx
        (Link.listWallets @'Shelley) Default Empty

    let keyByWalletId :: ApiWallet -> (WalletId, ApiWallet)
        keyByWalletId w = (getApiT $ w ^. #id, w)

        walletsById :: Map WalletId ApiWallet
        walletsById = Map.fromList $ map keyByWalletId wallets

        lookupWallet :: WalletId -> ApiWallet
        lookupWallet wid = fromMaybe err $ Map.lookup wid walletsById
          where
            err = error $ unwords
                [ "fixturePreprodWallets: expected", show wid, "to be an"
                , "existing wallet. The wallets that could be found are:\n\n"
                ] ++ unlines (map show wallets)

    -- Use the information from 'wallets'
    pure $ map lookupWallet $ _preprodWallets ctx
