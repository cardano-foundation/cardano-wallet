{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
where

import Prelude

import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.ClusterM
    ( ClusterM
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf (..)
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import System.FilePath
    ( (</>)
    )

genSinkAddress
    :: FileOf "output"
    -- ^ Directory to put keys
    -> Maybe (FileOf "stake-pub")
    -- ^ Stake pub
    -> ClusterM String
genSinkAddress outputDir stakePub = do
    Config{..} <- ask
    let sinkPrv = pathOf outputDir </> "sink.prv"
    let sinkPub = pathOf outputDir </> "sink.pub"
    cli
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , sinkPrv
        , "--verification-key-file"
        , sinkPub
        ]
    cliLine
        $ [ "address"
          , "build"
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--payment-verification-key-file"
          , sinkPub
          ]
            ++ case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", pathOf key]
