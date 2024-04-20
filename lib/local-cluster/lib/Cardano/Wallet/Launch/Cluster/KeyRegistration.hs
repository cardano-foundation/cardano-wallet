{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.KeyRegistration
    ( prepareStakeKeyRegistration
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
    , preRegisteredStakeKeyPair
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf (..)
    , FileOf (..)
    , toFilePath
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
import System.Path
    ( relFile
    , (</>)
    )

import qualified Data.Aeson as Aeson

-- | Generate a raw transaction that registers a staking certificate.
-- We kill two birds one stone here by also
-- automatically delegating 'pledge' amount to the given stake key.
prepareStakeKeyRegistration
    :: ClusterM
        ( FileOf "reg-tx"
        , FileOf "faucet-prv"
        , FileOf "stake-prv"
        )
prepareStakeKeyRegistration = do
    Config{..} <- ask
    let transactionPath = absDirOf cfgClusterDir </> relFile "tx.raw"
    let stakePub =
            absDirOf cfgClusterDir </> relFile "pre-registered-stake.vkey"
        stakePrv =
            absDirOf cfgClusterDir </> relFile "pre-registered-stake.skey"
    liftIO $ do
        let (pub, prv) = preRegisteredStakeKeyPair
        Aeson.encodeFile (toFilePath stakePub) pub
        Aeson.encodeFile (toFilePath stakePrv) prv
    (faucetInput, faucetPrv) <- takeFaucet
    FileOf cert <-
        issueStakeVkCert
            (Tagged @"prefix" "pre-registered")
            (FileOf stakePub)
    sink <- genSinkAddress Nothing
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
        , toFilePath cert
        , "--out-file"
        , toFilePath transactionPath
        ]
    pure
        ( FileOf @"reg-tx" transactionPath
        , faucetPrv
        , FileOf @"stake-prv" $ stakePrv
        )
