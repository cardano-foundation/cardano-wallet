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
    ( FileOf (..)
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
    let file = pathOf cfgClusterDir </> "tx.raw"
    let stakePub =
            FileOf @"stake-pub"
                $ pathOf cfgClusterDir </> "pre-registered-stake.vkey"
        stakePrv =
            FileOf @"stake-prv"
                $ pathOf cfgClusterDir </> "pre-registered-stake.skey"
    liftIO $ do
        let (pub, prv) = preRegisteredStakeKeyPair
        Aeson.encodeFile (pathOf stakePub) pub
        Aeson.encodeFile (pathOf stakePrv) prv
    (faucetInput, faucetPrv) <- takeFaucet
    cert <-
        issueStakeVkCert
            (Tagged @"prefix" "pre-registered")
            stakePub
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
        , pathOf cert
        , "--out-file"
        , file
        ]
    pure (FileOf @"reg-tx" file, faucetPrv, stakePrv)
