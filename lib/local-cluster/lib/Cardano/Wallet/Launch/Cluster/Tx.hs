{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Tx
    ( signTx
    , submitTx
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
import Cardano.Wallet.Launch.Cluster.Config
    ( TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog (..)
    )
import Control.Tracer
    ( Tracer (..)
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
    :: Tracer IO ClusterLog
    -> TestnetMagic
    -> Tagged "output" FilePath
    -- ^ Output directory
    -> Tagged "tx-body" FilePath
    -- ^ Tx body file
    -> [Tagged "signing-key" FilePath]
    -- ^ Signing keys for witnesses
    -> IO (Tagged "tx-signed" FilePath)
signTx tr testnetMagic outputDir rawTx keys = do
    file <- emptyTempFile (untag outputDir) "tx-signed.json"
    cli tr
        $ [ "transaction"
          , "sign"
          , "--tx-body-file"
          , untag rawTx
          , "--testnet-magic"
          , show (testnetMagicToNatural testnetMagic)
          , "--out-file"
          , file
          ]
            ++ concatMap (\key -> ["--signing-key-file", untag key]) keys
    pure $ Tagged @"tx-signed" file

-- | Submit a transaction through a running node.
submitTx
    :: Tracer IO ClusterLog
    -> TestnetMagic
    -> CardanoNodeConn
    -> Tagged "name" String
    -> Tagged "tx-signed" FilePath
    -> IO ()
submitTx tr testnetMagic conn name signedTx =
    cliRetry tr ("Submitting transaction for " <> T.pack (untag name))
        =<< cliConfigNode
            tr
            conn
            [ "transaction"
            , "submit"
            , "--tx-file"
            , untag signedTx
            , "--testnet-magic"
            , show (testnetMagicToNatural testnetMagic)
            , "--cardano-mode"
            ]
