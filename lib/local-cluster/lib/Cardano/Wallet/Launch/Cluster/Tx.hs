{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
where

import Prelude

import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliConfigNode
    , cliRetry
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , absFilePathOf
    , toFilePath
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , untag
    )
import System.IO.Temp
    ( emptyTempFile
    )
import System.Path
    ( absFile
    )

import qualified Data.Text as T

-- | Sign a transaction with all the necessary signatures.
signTx
    :: FileOf "tx-body"
    -- ^ Tx body file
    -> [FileOf "signing-key"]
    -- ^ Signing keys for witnesses
    -> ClusterM (FileOf "tx-signed")
signTx rawTx keys = do
    Config{..} <- ask
    let DirOf outputDir = cfgClusterDir
    signedTxPath <-
        fmap absFile
            $ liftIO
            $ emptyTempFile (toFilePath outputDir) "tx-signed.json"
    cli
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "sign"
          , "--tx-body-file"
          , absFilePathOf rawTx
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--out-file"
          , toFilePath signedTxPath
          ]
            ++ concatMap
                (\key -> ["--signing-key-file", absFilePathOf key])
                keys
    pure $ FileOf signedTxPath

-- | Submit a transaction through a running node.
submitTx
    :: CardanoNodeConn
    -> Tagged "name" String
    -> FileOf "tx-signed"
    -> ClusterM ()
submitTx conn name signedTx = do
    Config{..} <- ask
    cliRetry ("Submitting transaction for " <> T.pack (untag name))
        =<< cliConfigNode
            conn
            [ clusterEraToString cfgLastHardFork
            , "transaction"
            , "submit"
            , "--tx-file"
            , absFilePathOf signedTx
            , "--testnet-magic"
            , show (testnetMagicToNatural cfgTestnetMagic)
            , "--cardano-mode"
            ]

signAndSubmitTx
    :: CardanoNodeConn
    -> FileOf "tx-body"
    -- ^ Tx body file
    -> [FileOf "signing-key"]
    -- ^ Signing keys for witnesses
    -> Tagged "name" String
    -> ClusterM ()
signAndSubmitTx conn rawTx keys name = do
    signedTx <- signTx rawTx keys
    submitTx conn name signedTx
