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
    ( DirOf (..)
    , FileOf (..)
    , absFilePathOf
    , toFilePath
    )
import Control.Monad.Reader
    ( MonadReader (..)
    )
import System.Path
    ( relFile
    , (</>)
    )

genSinkAddress
    :: Maybe (FileOf "stake-pub")
    -- ^ Stake pub
    -> ClusterM String
genSinkAddress stakePub = do
    Config{..} <- ask
    let DirOf outputDir = cfgClusterDir
    let sinkPrv = outputDir </> relFile "sink.prv"
    let sinkPub = outputDir </> relFile "sink.pub"
    cli
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , toFilePath sinkPrv
        , "--verification-key-file"
        , toFilePath sinkPub
        ]
    cliLine
        $ [ "address"
          , "build"
          , "--testnet-magic"
          , show (testnetMagicToNatural cfgTestnetMagic)
          , "--payment-verification-key-file"
          , toFilePath sinkPub
          ]
            ++ case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", absFilePathOf key]
