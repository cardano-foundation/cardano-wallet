{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.KeyRegistration
    ( prepareKeyRegistration
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( depositAmt
    , faucetAmt
    , preRegisteredStakeKey
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    , changeFileOf
    )
import Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
import Cardano.Wallet.Launch.Cluster.StakeCertificates
    ( issueStakeVkCert
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
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: ClusterM (FileOf "reg-tx" , FileOf "faucet-prv")
prepareKeyRegistration = do
    Config{..} <- ask
    let outputDir = changeFileOf @"cluster" @"output" cfgClusterDir
    let file = pathOf cfgClusterDir </> "tx.raw"
    let stakePub =
            FileOf @"stake-pub"
                $ pathOf cfgClusterDir </> "pre-registered-stake.pub"
    liftIO $ Aeson.encodeFile (pathOf stakePub) preRegisteredStakeKey
    (faucetInput, faucetPrv) <- takeFaucet
    cert <-
        issueStakeVkCert
            outputDir
            (Tagged @"prefix" "pre-registered")
            stakePub
    sink <- genSinkAddress outputDir Nothing
    cli
        [ clusterEraToString cfgLastHardFork
        , "transaction"
        , "build-raw"
        , "--tx-in"
        , untag faucetInput
        , "--tx-out"
        , sink <> "+" <> "1000000"
        , "--ttl"
        , "400"
        , "--fee"
        , show (faucetAmt - depositAmt - 1_000_000)
        , "--certificate-file"
        , pathOf cert
        , "--out-file"
        , file
        ]
    pure (FileOf @"reg-tx" file, faucetPrv)
