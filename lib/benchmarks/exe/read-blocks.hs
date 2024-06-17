{-# LANGUAGE DataKinds #-}

import Prelude

import Cardano.BM.ToTextTracer
    ( ToTextTracer (..)
    , withToTextTracer
    )
import Cardano.Wallet.Benchmarks.Collect
    ( newReporterFromEnv
    , noSemantic
    , runCriterionBenchmark
    )
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
import Control.Monad.Cont
    ( evalContT
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Criterion.Main
    ( bench
    , bgroup
    , nf
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import System.IO
    ( stdout
    )

import qualified Cardano.Wallet.Primitive.Ledger.Read.Block as New
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = evalContT $ do
    ToTextTracer tr <- withToTextTracer (Left stdout) Nothing
    reporter <- newReporterFromEnv tr noSemantic
    liftIO
        $ runCriterionBenchmark 60 tr reporter
        $ bgroup
            "read blocks"
            [ bench "1 block" $ nf (run new) 1
            , bench "10 blocks" $ nf (run new) 10
            , bench "100 blocks" $ nf (run new) 100
            , bench "1000 blocks" $ nf (run new) 1000
            , bench "10000 blocks" $ nf (run new) 10000
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
