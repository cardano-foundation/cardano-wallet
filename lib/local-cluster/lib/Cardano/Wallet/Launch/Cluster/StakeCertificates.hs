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
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
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
    :: FileOf "output"
    -> Tagged "prefix" String
    -> FileOf "stake-pub"
    -> ClusterM (FileOf "stake-vk-cert")
issueStakeVkCert outputDir prefix stakePub = do
    let file = pathOf outputDir </> untag prefix <> "-stake.cert"
    cli
        [ "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , pathOf stakePub
        , "--out-file"
        , file
        ]
    pure $ FileOf file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: FileOf "output"
    -> Tagged "prefix" String
    -> FilePath
    -> ClusterM (FileOf "stake-script-cert")
issueStakeScriptCert outputDir prefix stakeScript = do
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
