module Main where

import Prelude

import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar )
import Data.Aeson
    ( Value )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager, Request, Response, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status200, status404, status405 )
import System.Process
    ( proc, withCreateProcess )
import Test.Hspec
    ( beforeAll, describe, hspec )
import Test.Integration.Framework.DSL
    ( Context (..)
    , RequestException (..)
    , Scenarios
    , expectError
    , expectResponseCode
    , request
    , request'
    , request_
    , scenario
    , verify
    )

import qualified Cardano.Wallet.Network.HttpBridgeSpec as HttpBridge
import qualified Cardano.WalletSpec as Wallet
import qualified Data.Text as T

main :: IO ()
main = do
    hspec $ do
        describe "Cardano.WalletSpec" Wallet.spec
        describe "Cardano.Wallet.Network.HttpBridge" HttpBridge.spec

        beforeAll (withWallet (newMVar . Context ())) $ do
            describe "Integration test framework" dummySpec

        beforeAll (dummySetup (newMVar . Context ())) $ do
            describe "Test response codes" respCodesSpec

-- Runs the wallet server only. The API is not implemented yet, so this is
-- basically a placeholder until then.
withWallet :: ((Text, Manager) -> IO a) -> IO a
withWallet action = do
    let launch = proc "cardano-wallet-server" []
        baseURL = T.pack ("http://localhost:8090/")
    manager <- newManager defaultManagerSettings
    withCreateProcess launch $ \_ _ _ _ph -> do
        threadDelay 1000000
        action (baseURL, manager)

-- Exercise the request functions, which just fail at the moment.
dummySpec :: Scenarios Context
dummySpec = do
    scenario "Try the API which isn't implemented yet" $ do
        response <- request ("GET", "api/wallets") Nothing
        verify (response :: Either RequestException Value)
            [ expectError
            ]

    scenario "request_ function is always successful" $ do
        request_ ("GET", "api/xyzzy") Nothing

-- Temporary test setup for testing response codes
dummySetup :: ((Text, Manager) -> IO a) -> IO a
dummySetup action = do
        let baseURL = T.pack ("http://httpbin.org")
        manager <- newManager defaultManagerSettings
        action (baseURL, manager)

-- Exercise response codes
respCodesSpec :: Scenarios Context
respCodesSpec = do
    scenario "GET; Response code 200" $ do
        response <- request' ("GET", "/get") Nothing
        verify (response :: Either RequestException (Request, Response ByteString))
            [ expectResponseCode status200
            ]

    scenario "GET; Response code 404" $ do
        response <- request' ("GET", "/get/nothing") Nothing
        verify (response :: Either RequestException (Request, Response ByteString))
            [ expectResponseCode status404
            ]

    scenario "POST; Response code 200" $ do
        response <- request' ("POST", "/post") Nothing
        verify (response :: Either RequestException (Request, Response ByteString))
            [ expectResponseCode status200
            ]

    scenario "POST; Response code 405" $ do
        response <- request' ("POST", "/get") Nothing
        verify (response :: Either RequestException (Request, Response ByteString))
            [ expectResponseCode status405
            ]
