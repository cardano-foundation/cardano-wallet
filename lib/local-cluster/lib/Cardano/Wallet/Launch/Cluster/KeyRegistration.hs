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
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( depositAmt
    , faucetAmt
    , preRegisteredStakeKey
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
import Cardano.Wallet.Launch.Cluster.StakeCertificates
    ( issueStakeVkCert
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , retag
    , untag
    )
import System.FilePath
    ( (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a raw transaction. We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareKeyRegistration
    :: Config
    -> IO (Tagged "reg-tx" FilePath, Tagged "faucet-prv" FilePath)
prepareKeyRegistration config@Config{..} = do
    let outputDir = retag @"cluster" @_ @"output" cfgClusterDir
    let file = untag cfgClusterDir </> "tx.raw"
    let stakePub =
            Tagged @"stake-pub"
                $ untag cfgClusterDir </> "pre-registered-stake.pub"
    Aeson.encodeFile (untag stakePub) preRegisteredStakeKey
    (faucetInput, faucetPrv) <- takeFaucet cfgClusterConfigs
    cert <-
        issueStakeVkCert
            cfgTracer
            outputDir
            (Tagged @"prefix" "pre-registered")
            stakePub
    sink <- genSinkAddress config outputDir Nothing
    cli
        cfgTracer
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
        , untag cert
        , "--out-file"
        , file
        ]
    pure (Tagged @"reg-tx" file, faucetPrv)
