{-# LANGUAGE DataKinds #-}

import Prelude

import Cardano.Wallet.Primitive.Types
    ( GenesisParameters (..)
    , StartTime (..)
    , getGenesisBlockHash
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Read.Block
    ( ConsensusBlock
    )
import Cardano.Wallet.Read.Block.Gen.Build
    ( exampleBlocks
    )
import Criterion.Main
    ( bench
    , bgroup
    , defaultMain
    , nf
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Read.Primitive.Block as New
import qualified Data.ByteString.Char8 as B8

-- Our benchmark harness.
main :: IO ()
main =
    defaultMain
        [ bgroup
            "fib"
            [ bench "1 new" $ nf (run new) 1
            , bench "10 new" $ nf (run new) 10
            , bench "100 new" $ nf (run new) 100
            , bench "1000 new" $ nf (run new) 1000
            ]
        ]

new :: GenesisParameters -> ConsensusBlock -> W.Block
new gp = fst . New.fromCardanoBlock (getGenesisBlockHash gp)

run :: (GenesisParameters -> ConsensusBlock -> W.Block) -> Int -> [W.Block]
run f n = f dummyGenesisParameters <$> take n exampleBlocks

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters =
    GenesisParameters
        { getGenesisBlockHash = dummyGenesisHash
        , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
        }

dummyGenesisHash :: Hash "Genesis"
dummyGenesisHash = Hash (B8.replicate 32 '1')
