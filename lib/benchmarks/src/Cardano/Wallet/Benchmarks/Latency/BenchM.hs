{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Benchmarks.Latency.BenchM
    ( BenchM
    , BenchCtx (..)
    , partialFromRight
    , clientEnv
    , fixtureMultiAssetWallet
    , fixtureWallet
    , fixtureWalletWith
    , finallyDeleteWallet
    , request
    , requestWithError
    , runDSL
    )
where

import Prelude

import Cardano.Wallet.Api.Clients.Testnet.Shelley
    ( A
    )
import Cardano.Wallet.Api.Types
    ( ApiWallet
    )
import Cardano.Wallet.Benchmarks.Latency.Measure
    ( LogCaptureFunc
    )
import Control.Monad
    ( void
    )
import Control.Monad.Reader
    ( MonadIO (..)
    , MonadReader (..)
    , ReaderT (..)
    , ask
    )
import Control.Monad.Trans.Resource
    ( MonadResource (..)
    , ResourceT
    , allocate
    , runResourceT
    )
import Data.Generics.Internal.VL
    ( (^.)
    )
import Network.Wai.Middleware.Logging
    ( ApiLog
    )
import Numeric.Natural
    ( Natural
    )
import Servant.Client
    ( ClientEnv
    , ClientError
    , ClientM
    , mkClientEnv
    , parseBaseUrl
    , runClientM
    )
import Test.Integration.Framework.DSL
    ( Context
    )

import qualified Cardano.Wallet.Api.Clients.Testnet.Shelley as C
import qualified Test.Integration.Framework.DSL as DSL

data BenchCtx = BenchCtx
    { dslContext :: Context
    , logFun :: LogCaptureFunc ApiLog ()
    }

-- one day we will export the manager from the context
clientEnv :: Context -> ClientEnv
clientEnv ctx = case parseBaseUrl $ show (fst $ ctx ^. #_manager) of
    Left _ -> error "Invalid base URL"
    Right bu -> mkClientEnv (snd $ ctx ^. #_manager) bu

partialFromRight :: Show l => Either l r -> r
partialFromRight = either (error . show) Prelude.id

type BenchM = ResourceT (ReaderT BenchCtx IO)

requestWithError :: ClientM a -> BenchM (Either ClientError a)
requestWithError x = do
    BenchCtx ctx _ <- ask
    liftIO $ runClientM x $ clientEnv ctx

request :: ClientM a -> BenchM a
request x = partialFromRight <$> requestWithError x

finallyDeleteWallet :: ApiWallet -> BenchM ()
finallyDeleteWallet w = do
    env <- ask
    void
        $ allocate (pure ())
        $ const
        $ void
        $ runReaderT (runResourceT (request $ C.deleteWallet (w ^. #id))) env

-- compatibility with DSL functions
runDSL :: (Context -> ResourceT IO a) -> BenchM a
runDSL action = do
    BenchCtx ctx _ <- ask
    liftResourceT $ action ctx

fixtureMultiAssetWallet :: BenchM ApiWallet
fixtureMultiAssetWallet = runDSL DSL.fixtureMultiAssetWallet

fixtureWallet :: BenchM ApiWallet
fixtureWallet = runDSL DSL.fixtureWallet

fixtureWalletWith :: [Natural] -> BenchM ApiWallet
fixtureWalletWith w = runDSL $ \ctx -> DSL.fixtureWalletWith @A ctx w
