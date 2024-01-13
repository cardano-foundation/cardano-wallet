{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , TestnetMagic (testnetMagicToNatural)
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

import qualified Data.Text as T

-- | Sign a transaction with all the necessary signatures.
signTx
    :: Config
    -> Tagged "output" FilePath
    -- ^ Output directory
    -> Tagged "tx-body" FilePath
    -- ^ Tx body file
    -> [Tagged "signing-key" FilePath]
    -- ^ Signing keys for witnesses
    -> IO (Tagged "tx-signed" FilePath)
signTx Config{..} outputDir rawTx keys = do
    file <- emptyTempFile (untag outputDir) "tx-signed.json"
    cli cfgTracer
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "sign"
          , "--tx-body-file"
          , untag rawTx
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--out-file"
          , file
          ]
            ++ concatMap (\key -> ["--signing-key-file", untag key]) keys
    pure $ Tagged @"tx-signed" file

-- | Submit a transaction through a running node.
submitTx
    :: Config
    -> CardanoNodeConn
    -> Tagged "name" String
    -> Tagged "tx-signed" FilePath
    -> IO ()
submitTx Config{..} conn name signedTx =
    cliRetry cfgTracer ("Submitting transaction for " <> T.pack (untag name))
        =<< cliConfigNode
            cfgTracer
            conn
            [ clusterEraToString cfgLastHardFork
            , "transaction"
            , "submit"
            , "--tx-file"
            , untag signedTx
            , "--testnet-magic"
            , show (testnetMagicToNatural cfgTestnetMagic)
            , "--cardano-mode"
            ]

signAndSubmitTx
    :: Config
    -> CardanoNodeConn
    -> Tagged "output" FilePath
    -- ^ Output directory
    -> Tagged "tx-body" FilePath
    -- ^ Tx body file
    -> [Tagged "signing-key" FilePath]
    -- ^ Signing keys for witnesses
    -> Tagged "name" String
    -> IO ()
signAndSubmitTx config conn outputDir rawTx keys name = do
    signedTx <- signTx config outputDir rawTx keys
    submitTx config conn name signedTx
