{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2023-2023 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch @cardano-wallet@.
module Cardano.Launcher.Wallet
    ( -- * Startup
      withCardanoWallet
    , CardanoWalletConfig (..)
    , NetworkConfig (..)

      -- * Run
    , CardanoWalletConn
    , getWalletPort
    ) where

import Prelude

import Cardano.Launcher
    ( LauncherLog, ProcessHasExited, withBackendCreateProcess )
import Cardano.Launcher.Node
    ( CardanoNodeConn, nodeSocketFile )
import Control.Tracer
    ( Tracer (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Network.Socket
    ( PortNumber )
import UnliftIO.Process
    ( CreateProcess (..), proc )

{-----------------------------------------------------------------------------
    Launching a `cardano-wallet` process
------------------------------------------------------------------------------}

-- | Parameters for connecting to the running wallet process.
newtype CardanoWalletConn = CardanoWalletConn {getWalletPort :: PortNumber}
    deriving (Show, Eq)

instance ToText CardanoWalletConn where
    toText = toText . fromEnum . getWalletPort

instance FromText CardanoWalletConn where
    fromText = fmap (CardanoWalletConn . toEnum) . fromText

data NetworkConfig
    = Mainnet
    | Testnet {nodeByronGenesis :: FilePath}
    deriving (Show, Eq)

-- | A subset of the @cardano-wallet@ CLI parameters,
-- used for starting the process.
data CardanoWalletConfig = CardanoWalletConfig
    { walletPort :: PortNumber
    -- ^ Port number for HTTP API. Good default: 8090.
    , walletDatabaseDir :: FilePath
    -- ^ Path to the wallet database file.
    , walletNetwork :: NetworkConfig
    -- ^ Network (mainnet or a testnet) that we connect to.
    , extraArgs :: [String]
    -- ^ Extra arguments to be passed to the process
    , executable :: Maybe FilePath
    -- ^ Path to the @cardano-wallet@ executable.
    , workingDir :: Maybe FilePath
    }
    deriving (Show, Eq)

-- | Spawns a @cardano-wallet@ process.
--
-- IMPORTANT: @cardano-wallet@ must be available on the current path.
withCardanoWallet
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> CardanoNodeConn
    -> CardanoWalletConfig
    -> (CardanoWalletConn -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO (Either ProcessHasExited a)
withCardanoWallet tr node cfg@CardanoWalletConfig{..} action =
    withBackendCreateProcess tr (cardanoWallet cfg node)
        $ \_ _ -> action $ CardanoWalletConn walletPort

cardanoWallet :: CardanoWalletConfig -> CardanoNodeConn -> CreateProcess
cardanoWallet CardanoWalletConfig{..} node =

    let cp = proc (fromMaybe "cardano-wallet" executable)
            $ [ "serve"
            , "--node-socket"
            , nodeSocketFile node
            , "--database"
            , walletDatabaseDir
            , "--port"
            , show walletPort
            ]
                <> case walletNetwork of
                    Mainnet -> ["--mainnet"]
                    Testnet path -> ["--testnet", path]
                <> extraArgs
    in cp { cwd = workingDir }
