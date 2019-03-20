module Main where

import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar )

import Data.Aeson
    ( Value )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Prelude
import System.Process
    ( proc, withCreateProcess )
import Test.Hspec
    ( beforeAll, describe, hspec )

import qualified Data.Text as T

import Test.Integration.Framework.DSL
    ( Context (..)
    , Scenarios
    , expectError
    , request
    , request_
    , scenario
    , verify
    )
import Test.Integration.Framework.Request
    ( RequestException (..) )

import qualified Cardano.NetworkLayer.HttpBridgeSpec as HttpBridge

main :: IO ()
main = do
    hspec $ do
        describe "Cardano.NetworkLayer.HttpBridge" HttpBridge.spec

        beforeAll (withWallet (newMVar . Context ())) $ do
            describe "Integration test framework" dummySpec

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
