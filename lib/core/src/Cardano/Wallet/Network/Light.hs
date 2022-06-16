{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Network.Light
    ( -- * Interface
      LightSyncSource (..)
    , LightBlocks
    , hoistLightSyncSource
    , lightSync

    , LightLayerLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Network
    ( ChainFollower (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( BlockSummary (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint (..)
    , chainPointFromBlockHeader
    , compareSlot
    )
import Control.Monad.Class.MonadTimer
    ( DiffTime, MonadDelay (..) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.List
    ( maximumBy, sortBy )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

{-------------------------------------------------------------------------------
    LightLayer
-------------------------------------------------------------------------------}
type BlockHeight = Integer

-- | Blockchain data source suitable for the implementation of 'lightSync'.
data LightSyncSource m block addr txs = LightSyncSource
    { stabilityWindow :: BlockHeight
        -- ^ Stability window.
    , getHeader :: block -> BlockHeader
        -- ^ Get the 'BlockHeader' of a given @block@.
    , getTip :: m BlockHeader
        -- ^ Latest tip of the chain.
    , isConsensus :: ChainPoint -> m Bool
        -- ^ Check whether a 'ChainPoint' still exists in the consensus,
        -- or whether the chain has rolled back already.
    , getBlockHeaderAtHeight :: BlockHeight -> m (Maybe BlockHeader)
        -- ^ Get the 'BlockHeader' at a given block height.
        -- Returns 'Nothing' if there is no block at this height (anymore).
    , getNextBlockHeader :: BlockHeader -> m (Maybe BlockHeader)
        -- ^ Get the next block header.
    , getBlockHeaderAt :: ChainPoint -> m (Maybe BlockHeader)
        -- ^ Get the full 'BlockHeader' belonging to a given 'ChainPoint'.
        -- Return 'Nothing' if the point is not consensus anymore.
    , getNextBlocks :: ChainPoint -> m (Maybe [block])
        -- ^ Get several blocks immediately following the given 'Chainpoint'.
        -- Return 'Nothing' if the point is not consensus anymore.
    , getAddressTxs :: BlockHeader -> BlockHeader -> addr -> m txs
        -- ^ Transactions for a given address and point range.
    }

hoistLightSyncSource
    :: (forall a. m a -> n a)
    -> LightSyncSource m block addr txs
    -> LightSyncSource n block addr txs
hoistLightSyncSource f x = LightSyncSource
    { stabilityWindow = stabilityWindow x
    , getHeader = getHeader x
    , getTip = f $ getTip x
    , isConsensus = f . isConsensus x
    , getBlockHeaderAtHeight = f . getBlockHeaderAtHeight x
    , getNextBlockHeader = f . getNextBlockHeader x
    , getBlockHeaderAt = f . getBlockHeaderAt x
    , getNextBlocks = f . getNextBlocks x
    , getAddressTxs = \a b c -> f $ getAddressTxs x a b c
    }

type LightBlocks m block addr txs =
    Either (NonEmpty block) (BlockSummary m addr txs)

-- | Retrieve the 'ChainPoint' with the highest 'Slot'.
latest :: [ChainPoint] -> ChainPoint
latest [] = ChainPointAtGenesis
latest xs = maximumBy compareSlot xs

-- | Retrieve the 'ChainPoint' with the second-highest 'Slot'.
secondLatest :: [ChainPoint] -> ChainPoint
secondLatest []  = ChainPointAtGenesis
secondLatest [_] = ChainPointAtGenesis
secondLatest xs  = head . tail $ sortBy (flip compareSlot) xs

-- | Drive a 'ChainFollower' using a 'LightSyncSource'.
-- Never returns.
lightSync
    :: (Monad m, MonadDelay m)
    => Tracer m LightLayerLog
    -> LightSyncSource m block addr txs
    -> ChainFollower m ChainPoint BlockHeader (LightBlocks m block addr txs)
    -> m Void
lightSync tr light follower = readChainPoints follower >>= syncFrom . latest
  where
    syncFrom chainPoint = do
        move <- proceedToNextPoint light chainPoint
        syncFrom =<< case move of
            Rollback -> do
                prev <- secondLatest <$> readChainPoints follower
                -- NOTE: Rolling back to a result of 'readChainPoints'
                -- should always be possible,
                -- but the code here does not need this assumption.
                traceWith tr $ MsgLightRollBackward chainPoint prev
                rollBackward follower prev
            Stable old new tip -> do
                traceWith tr $
                    MsgLightRollForward chainPoint new tip
                rollForward follower (Right $ mkBlockSummary light old new) tip
                pure $ chainPointFromBlockHeader new
            Unstable blocks new tip -> do
                case blocks of
                    [] -> threadDelay secondsPerSlot
                    b : bs -> do
                        traceWith tr $ MsgLightRollForward chainPoint new tip
                        rollForward follower (Left $ b :| bs) tip
                pure $ chainPointFromBlockHeader new

data NextPointMove block
    = Rollback
    -- ^ We are forced to roll back.
    | Stable BlockHeader BlockHeader BlockHeader
    -- ^ We are still in the stable region.
    -- @Stable old new tip@.
    | Unstable [block] BlockHeader BlockHeader
    -- ^ We are entering the unstable region.
    -- @Unstable blocks new tip@.

proceedToNextPoint
    :: Monad m
    => LightSyncSource m block addr txs
    -> ChainPoint
    -> m (NextPointMove block)
proceedToNextPoint light chainPoint = do
    tip <- getTip light
    mhere <- getBlockHeaderAt light chainPoint
    maybeRollback mhere $ \here ->
        if isUnstable (stabilityWindow light) here tip
        then do
            mblocks <- getNextBlocks light chainPoint
            maybeRollback mblocks $ \case
                [] -> pure $ Unstable [] here tip
                b : bs -> do
                    let new = getHeader light $ NE.last (b :| bs)
                    continue <- isConsensus light $ chainPointFromBlockHeader new
                    pure $ if continue
                        then Unstable (b:bs) new tip
                        else Rollback
        else do
            mold <- getNextBlockHeader light here
            mnew <- getBlockHeaderAtHeight light $
                blockHeightToInteger (blockHeight tip) - stabilityWindow light
            maybeRollback mold $ \old ->
                maybeRollback mnew $ \new ->
                    pure $ Stable old new tip
  where
    maybeRollback m f = maybe (pure Rollback) f m

-- | Test whether a 'ChainPoint' is in the
-- unstable region close to the tip.
isUnstable :: BlockHeight -> BlockHeader -> BlockHeader -> Bool
isUnstable stabilityWindow_ old tip =
    blockHeightToInteger (blockHeight tip) - stabilityWindow_
  <= blockHeightToInteger (blockHeight old)

secondsPerSlot :: DiffTime
secondsPerSlot = 2

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

blockHeightToInteger :: Quantity "block" Word32 -> Integer
blockHeightToInteger (Quantity n) = fromIntegral n

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}
data LightLayerLog
    = MsgLightRollForward ChainPoint BlockHeader BlockHeader
    | MsgLightRollBackward ChainPoint ChainPoint
    deriving (Show, Eq, Generic)

instance ToText LightLayerLog where
    toText = \case
        MsgLightRollForward from_ to_ tip -> T.unwords
            [ "LightLayer roll forward:"
            , "from: ", toText $ show from_
            , "to: ", toText $ show to_
            , "tip: ", toText $ show tip
            ]
        MsgLightRollBackward from_ to_ -> T.unwords
            [ "LightLayer roll backward:"
            , "from: ", toText $ show from_
            , "to: ", toText $ show to_
            ]

instance HasPrivacyAnnotation LightLayerLog

instance HasSeverityAnnotation LightLayerLog where
    getSeverityAnnotation = \case
        MsgLightRollForward{} -> Debug
        MsgLightRollBackward{} -> Debug
