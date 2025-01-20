{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Main where

import Prelude

import qualified Data.Text as T
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions

import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    )
import Cardano.Wallet.Application.CLI
    ( Port (..)
    )
import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (Testnet)
    )
import Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    , configureTestNet
    )
import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork (ConfiguredNetwork)
    )
import Cardano.Wallet.Spec.Options
    ( withTestOptions
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic
    )
import Control.Monad
    ( (>=>)
    )
import Data.IORef
    ( newIORef
    )
import Data.Maybe
    ( fromMaybe
    )
import Main.Utf8
    ( withUtf8
    )
import Network.URI
    ( parseURI
    )
import System.Environment
    ( lookupEnv
    )
import Test.Hspec
    ( Spec
    , aroundAll
    )
import Test.Hspec.Core.Runner
    ( defaultConfig
    , runSpec
    )
import Test.Integration.Framework.Context
    ( Context (..)
    )
import Test.Integration.Framework.DSL
    ( setupPreprodWallets
    )
import Test.Integration.Framework.Setup
    ( httpManager
    )


main :: IO ()
main = withUtf8 $ withTestOptions $ \testNetwork _traceConfiguration -> do
    _summary <- runSpec (walletSpec testNetwork) defaultConfig
    pure ()

walletSpec :: TestNetworkConfig -> Spec
walletSpec config =
    aroundAll (configureContext config)
        $ Transactions.e2eSpec @('Testnet 1)

configureTracing :: TraceConfiguration -> (TraceConfiguration -> IO ()) -> IO ()
configureTracing config f = f config

configureContext :: TestNetworkConfig -> (Context -> IO ()) -> IO ()
configureContext testNetworkConfig action =
    configureTestNet testNetworkConfig (contextFromNetwork >=> action)
  where
    contextFromNetwork :: ConfiguredNetwork -> IO Context
    contextFromNetwork (ConfiguredNetwork (WalletApi url host port)) = do
        manager <- httpManager
        let mUri = parseURI $ "http://" <> T.unpack host <> ":" <> show port <> "/"
        let baseUri = case mUri of
              Just uri -> uri
              Nothing  -> error $ "Invalid URI: " <> T.unpack url

        ioRef <- newIORef []
        let ctx = Context
                { _manager = (baseUri, manager)
                , _walletPort = Port port
                , _mainEra = ApiConway
                , _faucet = error "not implemented"
                , _networkParameters = error "todo: np"
                , _testnetMagic = error "testnetMagic"
                , _poolGarbageCollectionEvents = ioRef
                , _smashUrl = error "no smash url"
                , _mintSeaHorseAssets = error "no"
                , _preprodWallets = []
                }
        ctx' <- flip setupPreprodWallets ctx =<< e2ePreprodMnemonics
        return ctx'
      where
        e2ePreprodMnemonics :: IO [SomeMnemonic]
        e2ePreprodMnemonics
            = map (SomeMnemonic . unsafeMkMnemonic @15 . T.words)
            . T.lines
            . T.pack
            . fromMaybe (error errMsg)
            <$> lookupEnv envVarName
          where
            envVarName = "HAL_E2E_PREPROD_MNEMONICS"
            errMsg = unlines
                [ envVarName <> " is not set."
                , "Please set it to a '\\n'-separated list of ' '-separated mnemonics."
                , ""
                , "Example:"
                , "fish fish fish fish fish fish fish fish fish fish fish fish fish fish fish"
                , "buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo buffalo"
                ]
