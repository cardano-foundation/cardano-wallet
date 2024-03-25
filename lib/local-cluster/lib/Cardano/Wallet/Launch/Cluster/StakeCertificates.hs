{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Launch.Cluster.StakeCertificates
    ( issueStakeVkCert
    , issueStakeScriptCert
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ClusterEra (..)
    , clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Control.Monad.Reader
    ( asks
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

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tagged "prefix" String
    -> FileOf "stake-pub"
    -> ClusterM (FileOf "stake-vk-cert")
issueStakeVkCert prefix stakePub = do
    outputDir <- asks cfgClusterDir
    lastHardFork <- asks cfgLastHardFork
    let file = pathOf outputDir </> untag prefix <> "-stake.cert"
    cli $
        [ clusterEraToString lastHardFork
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , pathOf stakePub
        , "--out-file"
        , file
        ] <> case lastHardFork of
            BabbageHardFork -> []
            ConwayHardFork -> [
                "--key-reg-deposit-amt"
                , "1000000"
                ]
    pure $ FileOf file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tagged "prefix" String
    -> FilePath
    -> ClusterM (FileOf "stake-script-cert")
issueStakeScriptCert prefix stakeScript = do
    outputDir <- asks cfgClusterDir
    let file = pathOf outputDir </> untag prefix <> "-stake.cert"
    cli
        [ "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , stakeScript
        , "--out-file"
        , file
        ]
    pure $ FileOf file
