{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
where

import Prelude

import Cardano.BM.Tracer
    ( Tracer
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.Logging
    ( ClusterLog
    )
import Data.Tagged
    ( Tagged
    , untag
    )
import System.FilePath
    ( (</>)
    )

genSinkAddress
    :: Tracer IO ClusterLog
    -> TestnetMagic
    -> Tagged "output" FilePath
    -- ^ Directory to put keys
    -> Maybe (Tagged "stake-pub" FilePath)
    -- ^ Stake pub
    -> IO String
genSinkAddress tr testnetMagic outputDir stakePub = do
    let sinkPrv = untag outputDir </> "sink.prv"
    let sinkPub = untag outputDir </> "sink.pub"
    cli
        tr
        [ "address"
        , "key-gen"
        , "--signing-key-file"
        , sinkPrv
        , "--verification-key-file"
        , sinkPub
        ]
    cliLine tr
        $ [ "address"
          , "build"
          , "--testnet-magic"
          , show (testnetMagicToNatural testnetMagic)
          , "--payment-verification-key-file"
          , sinkPub
          ]
            ++ case stakePub of
                Nothing -> []
                Just key -> ["--stake-verification-key-file", untag key]
