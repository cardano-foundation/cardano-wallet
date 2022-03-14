{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Errors
    , ErrPostTx (..)

    -- * Chain following
    , ChainFollower (..)
    , mapChainFollower
    , ChainFollowLog (..)
    , ChainSyncLog (..)
    , mapChainSyncLog
    , withFollowStatsMonitoring

    -- * Logging (for testing)
    , FollowStats (..)
    , Rearview (..)
    , emptyStats
    , updateStats
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( LightSummary )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint (..)
    , ProtocolParameters
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolsSummary
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Control.Monad.Class.MonadSTM
    ( atomically )
import Control.Monad.Class.MonadSTM.Strict
    ( StrictTMVar, newTMVarIO, putTMVar, takeTMVar )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, contramapM, traceWith )
import Data.Bifunctor
    ( first )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, diffUTCTime, getCurrentTime )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import NoThunks.Class
    ( AllowThunksIn (..), NoThunks (..) )
import Numeric.Natural
    ( Natural )
import Safe
    ( headMay )
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )

import qualified Cardano.Api.Shelley as Node
import qualified Data.List.NonEmpty as NE

{-------------------------------------------------------------------------------
    ChainSync
-------------------------------------------------------------------------------}
-- | Interface for network capabilities.
data NetworkLayer m block = NetworkLayer
    { chainSync
        :: Tracer IO ChainFollowLog
        -> ChainFollower m ChainPoint BlockHeader (NonEmpty block)
        -> m ()
        -- ^ Connect to the node and run the ChainSync protocol.
        -- The callbacks provided in the 'ChainFollower' argument
        -- are used to handle intersection finding,
        -- the arrival of new blocks, and rollbacks.

    , lightSync
        :: Maybe (
            ChainFollower m ChainPoint BlockHeader (LightBlocks m block)
            -> m ()
          )
        -- ^ Connect to a data source that offers an efficient
        -- query @Address -> Transactions@.

    , currentNodeTip
        :: m BlockHeader
        -- ^ Get the current tip from the chain producer

    , currentNodeEra
        :: m AnyCardanoEra
        -- ^ Get the era the node is currently in.

    , currentProtocolParameters
        :: m ProtocolParameters
        -- ^ Get the last known protocol parameters. In principle, these can
        -- only change once per epoch.

    , currentNodeProtocolParameters
        :: m (Maybe Node.ProtocolParameters)
        -- ^ Get the last known node's protocol parameters. In principle, these can
        -- only change once per epoch.

    , currentSlottingParameters
        :: m SlottingParameters
        -- ^ Get the last known slotting parameters. In principle, these can
        -- only change once per era.

    , watchNodeTip
        :: (BlockHeader -> m ())
        -> m ()
        -- ^ Register a callback for when the node tip changes.
        -- This function should never finish, unless the callback throws an
        -- exception, which will be rethrown by this function.

    , postTx
        :: SealedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , stakeDistribution
        :: Coin -- Stake to consider for rewards
        -> m StakePoolsSummary

    , getCachedRewardAccountBalance
        :: RewardAccount
        -> m Coin
        -- ^ Return the cached reward balance of an account.
        --
        -- If there is no cached value, it will return `Coin 0`, and add the
        -- account to the internal set of observed account, such that it will be
        -- fetched later.

    , fetchRewardAccountBalances
        :: Set RewardAccount
        -> m (Map RewardAccount Coin)
        -- ^ Fetch the reward account balance of a set of accounts without
        -- any caching.

    , timeInterpreter
        :: TimeInterpreter (ExceptT PastHorizonException m)
    , syncProgress
        :: SlotNo -> m (SyncProgress)
    }

-- | In light-mode, we receive either a list of blocks as usual,
-- or a 'LightSummary' of blocks.
type LightBlocks m block = Either (NonEmpty block) (LightSummary m)

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl = nl
        { chainSync = \tr follower ->
            chainSync nl tr $ mapChainFollower id id id (fmap f) follower
        , lightSync =
            (\sync -> sync . mapChainFollower id id id (first $ fmap f))
            <$> lightSync nl
        }

-- | A collection of callbacks to use with the 'chainSync' function.
data ChainFollower m point tip blocks = ChainFollower
    { readLocalTip :: m [point]
        -- ^ Callback for reading the local tip. Used to negotiate the
        -- intersection with the node.
        --
        -- A response of [] is interpreted as `Origin` -- i.e. the chain will be
        -- served from genesis.
        --
        -- TODO: Could be named readCheckpoints?
    , rollForward :: blocks -> tip -> m ()
        -- ^ Callback for rolling forward.
        --
        -- Implementors _may_ delete old checkpoints while rolling forward.

    , rollBackward :: point -> m point
        -- ^ Roll back to the requested slot, or further, and return the point
        -- actually rolled back to.
        --
        -- __Example 1:__
        --
        -- If the follower stores checkpoints for all blocks, we can always roll
        -- back to the requested point exactly.
        --
        -- @
        -- -- If
        -- knownSlots follower `shouldReturn` [0,1,2,3]
        -- let requested = SlotNo 2
        -- -- Then
        -- actual <- rollBackward follower requested
        -- knownSlots follower shouldReturn` [0,1,2]
        -- actual `shouldBe` SlotNo 2
        -- @
        --
        -- Note that the slotNos are unlikely to be consecutive in real life,
        -- but this doesn't matter, as ouroboros-network asks us to rollback to
        -- points, corresponding to blocks.
        --
        -- __Example 2:__
        --
        -- @
        -- -- If
        -- knownSlots follower `shouldReturn` [0,9,10]
        -- let requested = SlotNo 2
        -- -- Then
        -- actual <- rollBackward follower requested
        -- knownSlots follower shouldReturn` [0]
        -- actual `shouldBe` SlotNo 0
        -- @
        --
    }

mapChainFollower
    :: Functor m
    => (point1 -> point2) -- ^ Covariant
    -> (point2 -> point1) -- ^ Contravariant
    -> (tip2 -> tip1) -- ^ Contravariant
    -> (blocks2 -> blocks1) -- ^ Contravariant
    -> ChainFollower m point1 tip1 blocks1
    -> ChainFollower m point2 tip2 blocks2
mapChainFollower fpoint12 fpoint21 ftip fblocks cf =
    ChainFollower
        { readLocalTip = map fpoint12 <$> readLocalTip cf
        , rollForward = \bs tip -> rollForward cf (fblocks bs) (ftip tip)
        , rollBackward = fmap fpoint12 . rollBackward cf . fpoint21
        }

{-------------------------------------------------------------------------------
    Errors
-------------------------------------------------------------------------------}

-- | Error while trying to send a transaction
newtype ErrPostTx = ErrPostTxValidationError Text
    deriving (Generic, Show, Eq)

instance ToText ErrPostTx where
    toText = \case
        ErrPostTxValidationError msg -> msg

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}

-- | Low-level logs of the ChainSync mini-protocol
data ChainSyncLog block point
    = MsgChainFindIntersect [point]
    | MsgChainRollForward (NonEmpty block) point
    | MsgChainRollBackward point Int
    | MsgChainTip point
    | MsgLocalTip point
    | MsgTipDistance Natural
    deriving (Show, Eq, Generic)

mapChainSyncLog
    :: (b1 -> b2)
    -> (p1 -> p2)
    -> ChainSyncLog b1 p1
    -> ChainSyncLog b2 p2
mapChainSyncLog f g = \case
    MsgChainFindIntersect points -> MsgChainFindIntersect (g <$> points)
    MsgChainRollForward blocks tip ->
        MsgChainRollForward (f <$> blocks) (g tip)
    MsgChainRollBackward point n -> MsgChainRollBackward (g point) n
    MsgChainTip point -> MsgChainTip (g point)
    MsgLocalTip point -> MsgLocalTip (g point)
    MsgTipDistance d -> MsgTipDistance d

instance ToText (ChainSyncLog BlockHeader ChainPoint) where
    toText = \case
        MsgChainFindIntersect cps -> mconcat
            [ "Requesting intersection using "
            , toText (length cps)
            , " points"
            , maybe "" ((", the latest being " <>) . pretty) (headMay cps)
            ]
        MsgChainRollForward headers tip ->
            let buildRange (x :| []) = x
                buildRange xs = NE.head xs <> ".." <> NE.last xs
                slots = pretty . slotNo <$> headers
            in mconcat
                [ "ChainSync roll forward: "
                , "applying blocks at slots [", buildRange slots, "]"
                , ", tip is "
                , pretty tip
                ]
        MsgChainRollBackward b 0 ->
            "ChainSync roll backward: " <> pretty b
        MsgChainRollBackward b bufferSize -> mconcat
            [ "ChainSync roll backward: "
            , pretty b
            , ", handled inside pipeline buffer with remaining length "
            , toText bufferSize
            ]
        MsgChainTip tip ->
            "Node tip is " <> pretty tip
        MsgLocalTip point ->
            "Synchronized with point: " <> pretty point
        MsgTipDistance d -> "Distance to chain tip: " <> toText d <> " blocks"

instance HasPrivacyAnnotation (ChainSyncLog block point)

instance HasSeverityAnnotation (ChainSyncLog block point) where
    getSeverityAnnotation = \case
        MsgChainFindIntersect{} -> Debug
        MsgChainRollForward{} -> Debug
        MsgChainRollBackward{} -> Debug
        MsgChainTip{} -> Debug
        MsgLocalTip{} -> Debug
        MsgTipDistance{} -> Debug

-- | Higher level log of a chain follower.
-- Includes computed statistics about synchronization progress.
data ChainFollowLog
    = MsgChainSync (ChainSyncLog BlockHeader ChainPoint)
    | MsgFollowStats (FollowStats Rearview)
    | MsgStartFollowing
    deriving (Show, Eq, Generic)

instance ToText ChainFollowLog where
    toText = \case
        MsgChainSync msg -> toText msg
        MsgFollowStats s -> toText s
        MsgStartFollowing -> "Chain following starting."

instance HasPrivacyAnnotation ChainFollowLog
instance HasSeverityAnnotation ChainFollowLog where
    getSeverityAnnotation = \case
        MsgChainSync msg -> getSeverityAnnotation msg
        MsgFollowStats s -> getSeverityAnnotation s
        MsgStartFollowing -> Info

{-------------------------------------------------------------------------------
    Log aggregation
-------------------------------------------------------------------------------}
-- | Statistics of interest from the follow-function.
--
-- The @f@ allows us to use 'Rearview' to keep track of both current and
-- previously logged stats, and perform operations over it in a nice way.
data FollowStats f = FollowStats
    { blocksApplied :: !(f Int)
    , rollbacks :: !(f Int)
    , localTip :: !(f ChainPoint)
    , time :: !(f UTCTime)
      -- ^ NOTE: Current time is not updated until @flush@ is called.
    , prog :: !(f SyncProgress)
      -- ^ NOTE: prog is not updated until @flush@ is called.
    } deriving (Generic)

-- It seems UTCTime contains thunks internally. This shouldn't matter as we
-- 1. Change it seldom - from @flush@, not from @updateStats@
-- 2. Set to a completely new value when we do change it.
deriving via (AllowThunksIn '["time"] (FollowStats Rearview))
    instance (NoThunks (FollowStats Rearview))

deriving instance Show (FollowStats Rearview)
deriving instance Eq (FollowStats Rearview)

-- | Change the @f@ wrapping each record field.
hoistStats
    :: (forall a. f a -> g a)
    -> FollowStats f
    -> FollowStats g
hoistStats f (FollowStats a b c d e) =
    FollowStats (f a) (f b) (f c) (f d) (f e)

-- | A 'Rearview' consists of a past value and a present value.
-- Useful for keeping track of past logs.
--
-- The idea is to
-- 1. Reconstruct a model of the @current@ @state@ using a @Trace@
-- 2. Sometimes log the difference between the @current@ state and the most
-- recently logged one.
data Rearview a = Rearview
    { past :: !a -- ^ Most previously logged state
    , current :: !a -- ^ Not-yet logged state
    } deriving (Eq, Show, Functor, Generic)

instance NoThunks a => NoThunks (Rearview a)

initRearview :: a -> Rearview a
initRearview a = Rearview a a

-- | Modify the present state of a @Rearview state@
overCurrent :: (a -> a) -> Rearview a -> Rearview a
overCurrent f (Rearview pas cur) = Rearview pas (f cur)

emptyStats :: UTCTime -> FollowStats Rearview
emptyStats t = FollowStats (f 0) (f 0) (f ChainPointAtGenesis) (f t) (f p)
  where
    f = initRearview
    p = NotResponding -- Hijacked as an initial value for simplicity.

-- | Update the current statistics based on a new log message.
updateStats
    :: ChainSyncLog block ChainPoint
    -> FollowStats Rearview -> FollowStats Rearview
updateStats msg s = case msg of
    MsgChainRollForward blocks _tip ->
        s { blocksApplied = overCurrent (+ NE.length blocks) (blocksApplied s) }
    MsgChainRollBackward _ 0 ->
        -- rolled back in a way that could not be handled by the pipeline buffer
        s { rollbacks = overCurrent (1 +) (rollbacks s) }
    MsgLocalTip point ->
        s { localTip = overCurrent (const point) (localTip s) }
    _ -> s

instance ToText (FollowStats Rearview) where
    toText st@(FollowStats b r tip t progress) =
        syncStatus <> " " <> stats <> sevExpl
      where
        syncStatus = case progress of
            Rearview NotResponding Ready ->
                "In sync."
            Rearview Ready Ready ->
                "Still in sync."
            Rearview NotResponding NotResponding ->
                "Still not syncing."
            Rearview (Syncing _p) Ready ->
                "In sync!"
            Rearview Ready (Syncing p) ->
                "Fell out of sync (" <> (pretty p) <> ")"
            Rearview _ (Syncing p) ->
                "Syncing (" <> (pretty p) <> ")"
            Rearview past_ NotResponding ->
                "Not responding. Previously " <> (pretty past_) <> "."
        stats = mconcat
            [ "Applied " <> pretty (using (-) b) <> " blocks, "
            , pretty (using (-) r) <> " rollbacks "
            , "in the last " <> pretty (using diffUTCTime t) <> ". "
            , "Current tip is " <> pretty (current tip) <> "."
            ]
          where
            using f x = f (current x) (past x)

        sevExpl = maybe
            ""
            (\x -> " (" <> x <> ")")
            (snd $ explainedSeverityAnnotation st)

-- NOTE: Here we check if the sync progress is going backwards, which
-- would be a sign the wallet is overloaded (or rollbacks)
--
-- But this check might be in the wrong place. Might be better to
-- produce new logs from inside the updateStats function and immeditely
-- warn there.
explainedSeverityAnnotation :: FollowStats Rearview -> (Severity, Maybe Text)
explainedSeverityAnnotation s
    | progressMovedBackwards = (Warning, Just "progress decreased")
    | noBlocks && notRestored = (Warning, Just "not applying blocks")
    | nowInSync = (Notice, Nothing)
    | otherwise = (Info, Nothing)
  where
    progressMovedBackwards = current (prog s) < past (prog s)
    nowInSync = current (prog s) == Ready && past (prog s) < Ready
    notRestored = current (prog s) /= Ready
    noBlocks = (current (blocksApplied s) - past (blocksApplied s)) <= 0

instance HasSeverityAnnotation (FollowStats Rearview) where
    getSeverityAnnotation = fst . explainedSeverityAnnotation

-- | Update the 'TMVar' holding the 'FollowStats'@ @'Rearview'
-- to forget the 'past' values and replace them with the 'current' ones.
-- Also update the time and sync process.
flushStats
    :: UTCTime
    -> (SlotNo -> IO SyncProgress)
    -> StrictTMVar IO (FollowStats Rearview)
    -> IO (FollowStats Rearview)
flushStats t calcSyncProgress var = do
    s <- atomically $ takeTMVar var
    p <- calcSyncProgress $ pseudoSlotNo $ current $ localTip s
    let s' = s { time = overCurrent (const t) (time s) }
               { prog = overCurrent (const p) (prog s) }
    atomically $ putTMVar var $ hoistStats forgetPast s'
    return s'
  where
    forgetPast (Rearview _past curr) = initRearview curr

-- See NOTE [PointSlotNo]
pseudoSlotNo :: ChainPoint -> SlotNo
pseudoSlotNo ChainPointAtGenesis = SlotNo 0
pseudoSlotNo (ChainPoint slot _) = slot

-- | Monitors health and statistics by inspecting the messages
-- submitted to a 'ChainSyncLog' tracer.
--
-- Statistics are computed in regular time intervals.
-- In order to do that, the monitor runs in separate thread.
-- The results are submitted to the outer 'ChainFollowLog' tracer.
withFollowStatsMonitoring
    :: Tracer IO ChainFollowLog
    -> (SlotNo -> IO SyncProgress)
    -> (Tracer IO (ChainSyncLog BlockHeader ChainPoint) -> IO ())
    -> IO ()
withFollowStatsMonitoring tr calcSyncProgress act = do
    t0  <- getCurrentTime
    var <- newTMVarIO $ emptyStats t0
    let trChainSyncLog = flip contramapM tr $ \msg -> do
            atomically $ do
                s <- takeTMVar var
                putTMVar var $! updateStats msg s
            pure $ MsgChainSync msg
    traceWith trChainSyncLog $ MsgLocalTip ChainPointAtGenesis
    race_
        (act trChainSyncLog)
        (loop var startupDelay)
  where
    loop var delay = do
        threadDelay delay
        t <- getCurrentTime
        s <- flushStats t calcSyncProgress var
        traceWith tr $ MsgFollowStats s
        let delay' =
                if (current (prog s)) == Ready
                then restoredDelay
                else syncingDelay
        loop var delay'

    -- | Delay from launch to the first status update
    startupDelay = 5 * second
    -- | Delay between status updates when restored
    restoredDelay = 5 * minute
    -- | Delay between status updates when not restored
    syncingDelay = 30 * second

    second = 1000*1000
    minute = 60 * second
