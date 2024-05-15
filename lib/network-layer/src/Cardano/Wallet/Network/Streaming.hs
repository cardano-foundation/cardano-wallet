{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Network.Streaming
    ( -- * ChainStream creation
      ChainStream
    , withStreamingFromBlockChain
    , NewBuffer
    , newTMVarBuffer
    , newTBQueueBuffer
    , Buffer (..)

      -- * ChainStream manipulation
    , forConsensusS
    , eraBlockS
    , eraTxS
    , forChainStream
    , scanChainStream
    )
where

import Prelude hiding
    ( take
    )

import Cardano.Wallet.Network
    ( ChainFollowLog
    , ChainFollower (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Network.Rollback.ChainPoints
    ( ChainPoints (..)
    )
import Cardano.Wallet.Read
    ( BHeader
    , Block
    , ChainPoint (..)
    , ChainTip
    , ConsensusBlock
    , EraValue
    , IsEra (..)
    , Tx
    , applyEraFunValue
    , chainPointFromChainTip
    , fromConsensusBlock
    , getEraTransactions
    , sequenceEraValue
    , (:*:) (..)
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Block.BHeader
    ( getEraBHeader
    )
import Control.Monad.Fix
    ( fix
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO
    )
import Control.Monad.Trans.Cont
    ( ContT (..)
    )
import Control.Parallel
    ( par
    )
import Control.Tracer
    ( Tracer
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , StandardCrypto
    )
import Streaming
    ( MonadIO (..)
    , MonadTrans (..)
    , Of (..)
    , Stream
    )
import UnliftIO.Async
    ( withAsync
    )
import UnliftIO.STM
    ( atomically
    , newEmptyTMVarIO
    , newTBQueueIO
    , putTMVar
    , readTBQueue
    , takeTMVar
    , writeTBQueue
    )

import qualified Streaming.Prelude as S

data Message a = Forward ChainTip a | Rollback ChainPoint

type ChainStream a = Stream (Of (Message a))

-- | A Buffer that can be taken from and put into. We instroduce this abstraction
-- to avoid binding the implementation to IO / MVar.
data Buffer m a = Buffer
    { put :: a -> m ()
    , take :: m a
    }

-- | A function that creates a new Buffer.
type NewBuffer m = forall a. m (Buffer m a)

-- | Create a new Buffer using an MVar.
newTMVarBuffer :: MonadIO m => NewBuffer m
newTMVarBuffer = do
    var <- newEmptyTMVarIO
    pure
        $ Buffer
            { put = atomically . putTMVar var
            , take = atomically $ takeTMVar var
            }

-- | Create a new Buffer using a TBQueue.
newTBQueueBuffer
    :: MonadIO m
    => Natural
    -- ^ size of the queue
    -> NewBuffer m
newTBQueueBuffer size = do
    queue <- newTBQueueIO size
    pure
        $ Buffer
            { put = atomically . writeTBQueue queue
            , take = atomically $ readTBQueue queue
            }

-- | Expose a 'ChainFollower' as a 'ChainStream'. A thread is forked to run the
-- 'ChainFollower' in the background.
withStreamingFromBlockChain
    :: MonadUnliftIO m
    => NetworkLayer m (CardanoBlock StandardCrypto)
    -- ^ ChainFollower provider
    -> Tracer IO ChainFollowLog
    -- ^ ChainFollower logger
    -> NewBuffer m
    -- ^ Buffer provider
    -> ContT r m (ChainStream (NonEmpty ConsensusBlock) m x)
withStreamingFromBlockChain network tr newBuffer = do
    messages <- lift newBuffer
    let cf =
            ChainFollower
                { checkpointPolicy = mempty
                , readChainPoints = pure []
                , rollForward = \blocks nodeTip ->
                    put messages $ Forward nodeTip blocks
                , rollBackward = \point -> do
                    put messages $ Rollback point
                    pure point
                }
    _ <- ContT $ withAsync $ chainSync network tr cf
    pure $ fix $ \next -> do
        msg <- lift $ take messages
        S.yield msg
        next

explodeBlock :: IsEra era => Block era -> (BHeader :*: ([] :.: Tx)) era
explodeBlock block =
    let txs = getEraTransactions block
        bh = getEraBHeader block
    in  (bh :*: Comp txs)

forConsensusS
    :: Monad m
    => ChainStream (NonEmpty ConsensusBlock) m r
    -> ChainStream (ConsensusBlock) m r
forConsensusS = forChainStream $ \blocks -> do
    S.each $ foldr par blocks blocks

eraBlockS
    :: Monad m
    => ChainStream (ConsensusBlock) m r
    -> ChainStream (EraValue (BHeader :*: ([] :.: Tx))) m r
eraBlockS = S.map $ \case
    Forward tip block ->
        (Forward tip)
            $ (\y -> let (h :*: txs) = explodeBlock y in h :*: txs)
                `applyEraFunValue` fromConsensusBlock block
    Rollback cp -> Rollback cp

eraTxS
    :: Monad m
    => ChainStream (EraValue (ctx :*: ([] :.: Tx))) m r
    -> ChainStream (EraValue (ctx :*: Tx)) m r
eraTxS s = S.for s $ \case
    Forward tip x -> S.each $ Forward tip <$> q x
    Rollback cp -> S.yield $ Rollback cp
  where
    f :: (ctx :*: ([] :.: Tx)) era -> ([] :.: (ctx :*: Tx)) era
    f (bh :*: Comp txs) = Comp $ fmap (bh :*:) txs
    q v = sequenceEraValue (f `applyEraFunValue` v)

-- | Pure scanning of a 'ChainStream'.
scanChainStream
    :: Monad m
    => (b -> a -> b)
    -- ^ How to react to new elements
    -> ChainPoints b
    -- ^ How to react to rollbacks
    -> ChainStream a m r
    -- ^ The stream to scan
    -> Stream (Of b) m r
    -- ^ The scanned stream
scanChainStream ingest state = S.drop 1 . S.scan ingest' state current
  where
    ingest' s = \case
        Forward tip x ->
            feed s (chainPointFromChainTip tip)
                $ ingest (current s) x
        Rollback cp -> rollback s cp

forChainStream
    :: Monad m
    => (a -> Stream (Of b) m ())
    -> ChainStream a m r
    -> ChainStream b m r
forChainStream f s = S.for s $ \case
    Forward tip x -> S.map (Forward tip) $ f x
    Rollback cp -> S.yield $ Rollback cp
