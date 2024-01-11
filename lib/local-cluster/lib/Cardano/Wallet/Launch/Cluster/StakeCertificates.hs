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
import System.FilePath
    ( (</>)
    )

-- | Create a stake address registration certificate from a vk
issueStakeVkCert
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -> Tagged "prefix" String
    -> Tagged "stake-pub" FilePath
    -> IO (Tagged "stake-vk-cert" FilePath)
issueStakeVkCert tr outputDir prefix stakePub = do
    let file = untag outputDir </> untag prefix <> "-stake.cert"
    cli
        tr
        [ "stake-address"
        , "registration-certificate"
        , "--staking-verification-key-file"
        , untag stakePub
        , "--out-file"
        , file
        ]
    pure $ Tagged file

-- | Create a stake address registration certificate from a script
issueStakeScriptCert
    :: Tracer IO ClusterLog
    -> Tagged "output" FilePath
    -> Tagged "prefix" String
    -> FilePath
    -> IO (Tagged "stake-script-cert" FilePath)
issueStakeScriptCert tr outputDir prefix stakeScript = do
    let file = untag outputDir </> untag prefix <> "-stake.cert"
    cli
        tr
        [ "stake-address"
        , "registration-certificate"
        , "--stake-script-file"
        , stakeScript
        , "--out-file"
        , file
        ]
    pure $ Tagged file
