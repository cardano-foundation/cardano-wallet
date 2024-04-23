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
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
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
import System.Path
    ( relFile
    , (<.>)
    , (</>)
    )

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tagged "prefix" String
    -> FileOf "stake-pub"
    -> ClusterM (FileOf "stake-vk-cert")
issueStakeVkCert prefix (FileOf stakePub) = do
    DirOf outputDir <- asks cfgClusterDir
    lastHardFork <- asks cfgLastHardFork
    let certPath = outputDir </> relFile (untag prefix <> "-stake") <.> "cert"
    cli $
        [ clusterEraToString lastHardFork
        , "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , toFilePath stakePub
        , "--out-file"
        , toFilePath certPath
        ] <> case lastHardFork of
            BabbageHardFork -> []
            ConwayHardFork -> [
                "--key-reg-deposit-amt"
                , "1000000"
                ]
    pure $ FileOf certPath

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tagged "prefix" String
    -> FilePath
    -> ClusterM (FileOf "stake-script-cert")
issueStakeScriptCert prefix stakeScript = do
    DirOf outputDir <- asks cfgClusterDir
    let certPath = outputDir </> relFile (untag prefix <> "-stake") <.> "cert"
    cli
        [ "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , stakeScript
        , "--out-file"
        , toFilePath certPath
        ]
    pure $ FileOf certPath
