{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Network.Light
    ( -- * Interface
      LightSyncSource (..)
    , LightBlocks
    , hoistLightSyncSource
    , lightSync
    , Consensual (..)
    , LightLayerLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.Wallet.Network
    ( ChainFollower (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader (..)
    , chainPointFromBlockHeader
    , fromWalletChainPoint
    )
import Cardano.Wallet.Primitive.Types.BlockSummary
    ( BlockSummary (..)
    )
import Control.Monad.Class.MonadTimer
    ( MonadDelay (..)
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Functor
    ( ($>)
    )
import Data.List
    ( maximumBy
    , sortBy
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Void
    ( Void
    )
import Fmt
    ( Buildable (build)
    , pretty
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.Text as T

{-------------------------------------------------------------------------------
    LightLayer
-------------------------------------------------------------------------------}
type BlockHeight = Integer

-- | Blockchain data source suitable for the implementation of 'lightSync'.
data LightSyncSource m block addr txs = LightSyncSource
    { getHeader :: block -> BlockHeader
        -- ^ Get the 'BlockHeader' of a given @block@.
    , getTip :: m BlockHeader
        -- ^ Latest tip of the chain.
    , getBlockHeaderAtHeight :: BlockHeight -> m (Consensual BlockHeader)
        -- ^ Get the 'BlockHeader' at a given block height.
    , getNextBlockHeader :: BlockHeader -> m (Consensual (Maybe BlockHeader))
        -- ^ Get the next block header.
    , getBlockHeaderAt :: Read.ChainPoint -> m (Consensual BlockHeader)
        -- ^ Get the full 'BlockHeader' belonging to a given 'ChainPoint'.
        -- Return 'Nothing' if the point is not consensus anymore.
    , getNextBlocks :: Read.ChainPoint -> m (Consensual [block])
        -- ^ Get several blocks immediately following the given 'Chainpoint'.
    , getAddressTxs :: BlockHeader -> BlockHeader -> addr -> m txs
        -- ^ Transactions for a given address and point range.
    }

hoistLightSyncSource
    :: (forall a. m a -> n a)
    -> LightSyncSource m block addr txs
    -> LightSyncSource n block addr txs
hoistLightSyncSource f x = LightSyncSource
    { getHeader = getHeader x
    , getTip = f $ getTip x
    , getBlockHeaderAtHeight = f . getBlockHeaderAtHeight x
    , getNextBlockHeader = f . getNextBlockHeader x
    , getBlockHeaderAt = f . getBlockHeaderAt x
    , getNextBlocks = f . getNextBlocks x
    , getAddressTxs = \a block c -> f $ getAddressTxs x a block c
    }

type LightBlocks m block addr txs =
    Either (NonEmpty block) (BlockSummary m addr txs)

-- | Retrieve the 'ChainPoint' with the highest 'Slot'.
latest :: [Read.ChainPoint] -> Read.ChainPoint
latest [] = Read.GenesisPoint
latest xs = maximumBy compareSlot xs

-- | Retrieve the 'ChainPoint' with the second-highest 'Slot'.
secondLatest :: [Read.ChainPoint] -> Read.ChainPoint
secondLatest []  = Read.GenesisPoint
secondLatest [_] = Read.GenesisPoint
secondLatest xs  = case sortBy (flip compareSlot) xs of
    (_:y:_) -> y
    _ -> Read.GenesisPoint

-- | Compare the slot numbers of two 'Read.ChainPoint's,
-- but where the 'Read.GenesisPoint' comes before all other slot numbers.
compareSlot :: Read.ChainPoint -> Read.ChainPoint -> Ordering
compareSlot pt1 pt2 = compare (toOrdered pt1) (toOrdered pt2)
  where
    toOrdered :: Read.ChainPoint -> Integer
    toOrdered Read.GenesisPoint = -1
    toOrdered (Read.BlockPoint (Read.SlotNo nat) _) = toInteger nat

-- | Convert a 'BlockHeader' into a 'Read.ChainPoint'.
chainPointFromBlockHeader' :: BlockHeader -> Read.ChainPoint
chainPointFromBlockHeader' = fromWalletChainPoint . chainPointFromBlockHeader

-- | Drive a 'ChainFollower' using a 'LightSyncSource'.
-- Never returns.
lightSync
    :: MonadDelay m
    => Tracer m LightLayerLog
    -> LightSyncSource m block addr txs
    -> ChainFollower m
        Read.ChainPoint
        BlockHeader
        (LightBlocks m block addr txs)
    -> m Void
lightSync tr light follower = readChainPoints follower >>= syncFrom . latest
  where
    syncFrom chainPoint = do
        move <- proceedToNextPoint light chainPoint
        syncFrom =<< case move of
            RollBackward -> do
                prev <- secondLatest <$> readChainPoints follower
                -- NOTE: Rolling back to a result of 'readChainPoints'
                -- should always be possible,
                -- but the code currently does not need this assumption.
                traceWith tr $ MsgLightRollBackward chainPoint prev
                rollBackward follower prev
            RollForward old new tip -> do
                traceWith tr $ MsgLightRollForward chainPoint old new tip
                rollForward follower (Right $ mkBlockSummary light old new) tip
                traceWith tr $ MsgLightRolledForward new
                pure $ chainPointFromBlockHeader' new
            WaitForANewTip tip -> do
                threadDelay 2 -- seconds
                $> chainPointFromBlockHeader' tip

data NextPointMove block
    = RollForward
        BlockHeader
        -- ^ From
        BlockHeader
        -- ^ To
        BlockHeader
        -- ^ Tip
    | RollBackward
    | WaitForANewTip BlockHeader
    deriving (Show)

-- | 'Consensual' represents the result of query on the blockchain.
-- Either the result is a value that is part of the consensus chain,
-- or the result is an indication that the consensus had changed
-- before the entire value could be retrieved.
data Consensual a
    = NotConsensual
    | Consensual a
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Buildable a => Buildable (Consensual a) where
  build = \case
    NotConsensual -> "NotConsensual"
    Consensual a -> "Consensual " <> build a

consensually
    :: Applicative m
    => (a -> m (NextPointMove block))
    -> Consensual a
    -> m (NextPointMove block)
consensually k ca =
    case ca of
        NotConsensual-> pure RollBackward
        Consensual a -> k a

proceedToNextPoint
    :: Monad m
    => LightSyncSource m block addr txs
    -> Read.ChainPoint
    -> m (NextPointMove block)
proceedToNextPoint LightSyncSource{..} chainPoint =
    getBlockHeaderAt chainPoint >>= consensually \currentBlock ->
        getNextBlockHeader currentBlock >>= consensually \case
            Nothing -> pure $ WaitForANewTip currentBlock
            Just fromBlock -> do
                chainTip <- getTip
                -- In some rare cases a rollback happens on the blockchain
                -- in between the getNextBlockHeader and getTip
                -- and the tip is behind the next block. We don't want to
                -- roll forward in this case, so we additionally check that
                -- we only roll forward if the block height is growing.
                --
                -- FIXME later: This criterion is not foolproof —
                -- it is possible for the tip to gain higher block height even
                -- after a rollback. For now, we accept this rare risk.
                -- Note: The invariant that we (eventually) want to preserve
                -- is that all block headers in `RollForward` are consistent
                -- with a potential history of the blockchain.
                -- (The headers do not need to consensus at the time
                -- of the roll forward, but they do need to be consistent
                -- with each other.)
                pure
                    if blockHeight fromBlock <= blockHeight chainTip
                    then RollForward fromBlock chainTip chainTip
                    else RollBackward

-- | Create a 'BlockSummary'
mkBlockSummary
    :: LightSyncSource m block addr txs
    -> BlockHeader
    -> BlockHeader
    -> BlockSummary m addr txs
mkBlockSummary light old new = BlockSummary
    { from = old
    , to = new
    , query = getAddressTxs light old new
    }

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}
data LightLayerLog
    = MsgLightRollForward
        Read.ChainPoint BlockHeader BlockHeader BlockHeader
    | MsgLightRolledForward BlockHeader
    | MsgLightRollBackward
        Read.ChainPoint Read.ChainPoint
    deriving (Show, Eq, Generic)

instance ToText LightLayerLog where
    toText = \case
        MsgLightRollForward cp_ from_ to_ tip ->
            T.unwords
                [ "LightLayer started rolling forward:"
                , "chain_point: ", Read.prettyChainPoint cp_
                , "from: ", pretty from_
                , "to: ", pretty to_
                , "tip: ", pretty tip
                ]
        MsgLightRolledForward bh ->
            T.unwords
                [ "LightLayer finished rolling forward:"
                , "last block: ", pretty bh
                ]
        MsgLightRollBackward from_ to_ ->
            T.unwords
                [ "LightLayer roll backward:"
                , "from: ", Read.prettyChainPoint from_
                , "to: ", Read.prettyChainPoint to_
                ]

instance HasPrivacyAnnotation LightLayerLog

instance HasSeverityAnnotation LightLayerLog where
    getSeverityAnnotation = \case
        MsgLightRollForward{} -> Debug
        MsgLightRolledForward{} -> Debug
        MsgLightRollBackward{} -> Debug
