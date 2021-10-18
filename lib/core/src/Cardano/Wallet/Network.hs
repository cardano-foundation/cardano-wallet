{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , FollowLog (..)
    , ChainSyncLog (..)
    , mapChainSyncLog
    , withFollowStatsMonitoring
    , addFollowerLogging

    -- * Logging (for testing)
    , FollowStats (..)
    , LogState (..)
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
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint
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
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Quantity
    ( Quantity (..) )
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
    ( lastMay )
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )

import qualified Cardano.Api.Shelley as Node
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

{-------------------------------------------------------------------------------
    ChainSync
-------------------------------------------------------------------------------}
-- | Interface for network capabilities.
data NetworkLayer m block = NetworkLayer
    { chainSync
        :: forall msg. Tracer IO (FollowLog msg)
        -> ChainFollower m
            ChainPoint
            BlockHeader
            block
        -> m ()

    , currentNodeTip
        :: m BlockHeader
        -- ^ Get the current tip from the chain producer
        --
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

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl = nl
        { chainSync = \tr follower ->
            chainSync nl tr (mapChainFollower id id id f follower)
        }

-- | A collection of callbacks to use with the 'chainSync' function.
data ChainFollower m point tip block = ChainFollower
    { readLocalTip :: m [point]
        -- ^ Callback for reading the local tip. Used to negotiate the
        -- intersection with the node.
        --
        -- A response of [] is interpreted as `Origin` -- i.e. the chain will be
        -- served from genesis.
        --
        -- TODO: Could be named readCheckpoints?
    , rollForward :: tip -> NonEmpty block -> m ()
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
    -> (block2 -> block1) -- ^ Contravariant
    -> ChainFollower m point1 tip1 block1
    -> ChainFollower m point2 tip2 block2
mapChainFollower fpoint12 fpoint21 ftip fblock cf =
    ChainFollower
        { readLocalTip = map fpoint12 <$> readLocalTip cf
        , rollForward = \t bs -> rollForward cf (ftip t) (fmap fblock bs)
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


-- | Low-level logs for chain-sync
data ChainSyncLog block point
    = MsgChainRollForward block point
    | MsgChainRollBackward point Int
    | MsgTipDistance Natural
    deriving (Show, Eq, Generic)

mapChainSyncLog
    :: (b1 -> b2)
    -> (p1 -> p2)
    -> ChainSyncLog b1 p1
    -> ChainSyncLog b2 p2
mapChainSyncLog f g = \case
    MsgChainRollForward block point -> MsgChainRollForward (f block) (g point)
    MsgChainRollBackward point n -> MsgChainRollBackward (g point) n
    MsgTipDistance d -> MsgTipDistance d

instance (ToText block, ToText point)
    => ToText (ChainSyncLog block point) where
    toText = \case
        MsgChainRollForward b tip ->
            "ChainSync roll forward: " <> toText b <> " tip is " <> toText tip
        MsgChainRollBackward b 0 ->
            "ChainSync roll backward: " <> toText b
        MsgChainRollBackward b bufferSize -> mconcat
            [ "ChainSync roll backward: "
            , toText b
            , ", handled inside buffer with remaining length "
            , toText bufferSize
            ]
        MsgTipDistance d -> "Tip distance: " <> toText d

instance HasPrivacyAnnotation (ChainSyncLog block point)

instance HasSeverityAnnotation (ChainSyncLog block point) where
    getSeverityAnnotation = \case
        MsgChainRollForward{} -> Debug
        MsgChainRollBackward{} -> Debug
        MsgTipDistance{} -> Debug

data FollowLog msg
    = MsgStartFollowing [BlockHeader]
    | MsgHaltMonitoring
    | MsgUnhandledException Text
    | MsgFollowerTip (Maybe BlockHeader)
    | MsgFollowStats (FollowStats LogState)
    | MsgApplyBlocks BlockHeader (NonEmpty BlockHeader)
    | MsgFollowLog msg -- Inner tracer
    | MsgWillRollback ChainPoint
    | MsgDidRollback ChainPoint ChainPoint
    | MsgFailedRollingBack Text -- Reason
    | MsgWillIgnoreRollback SlotNo Text -- Reason
    | MsgChainSync (ChainSyncLog Text Text)
    deriving (Show, Eq, Generic)

instance ToText msg => ToText (FollowLog msg) where
    toText = \case
        MsgStartFollowing cps -> mconcat
            [ "Chain following starting. Requesting intersection using "
            , T.pack . show $ length cps
            , " checkpoints"
            , maybe "" ((", the latest being " <>) . pretty) (lastMay cps)
            ]
        MsgHaltMonitoring ->
            "Stopping following as requested."
        MsgUnhandledException err ->
            "Unexpected error following the chain: " <> err
        MsgFollowerTip p -> "Tip" <> pretty p
        MsgFollowStats s -> toText s
        MsgApplyBlocks tipHdr hdrs ->
            let slot = pretty . slotNo
                buildRange (x :| []) = x
                buildRange xs = NE.head xs <> ".." <> NE.last xs
                blockHeights = pretty . getQuantity . blockHeight <$> hdrs
            in mconcat
                [ "Applying block numbers [", buildRange blockHeights, "]"
                , "  Wallet/node slots: ", slot (NE.last hdrs)
                , "/", slot tipHdr
                ]
        MsgWillIgnoreRollback sl reason ->
            "Will ignore rollback to " <> pretty sl
                <> " because of " <> pretty reason
        MsgWillRollback sl ->
            "Will rollback to " <> pretty sl
        MsgDidRollback requested actual -> mconcat
            [ "Did rollback to "
            , pretty actual
            , " after request to rollback to "
            , pretty requested
            ]
        MsgFailedRollingBack reason -> "Failed rolling back: " <>
            reason
        MsgFollowLog msg -> toText msg
        MsgChainSync msg -> toText msg

instance HasPrivacyAnnotation (FollowLog msg)
instance HasSeverityAnnotation msg => HasSeverityAnnotation (FollowLog msg) where
    getSeverityAnnotation = \case
        MsgStartFollowing _ -> Info
        MsgHaltMonitoring -> Info
        MsgFollowStats s -> getSeverityAnnotation s
        MsgFollowerTip _ -> Debug
        MsgUnhandledException _ -> Error
        MsgApplyBlocks _ _ -> Debug
        MsgFollowLog msg -> getSeverityAnnotation msg
        MsgWillRollback _ -> Debug
        MsgDidRollback _ _ -> Debug
        MsgFailedRollingBack _ -> Error
        MsgWillIgnoreRollback _ _ -> Debug
        MsgChainSync msg -> getSeverityAnnotation msg


--
-- Log aggregation
--

-- | Statistics of interest from the follow-function.
--
-- The @f@ allows us to use @LogState@ to keep track of both current and
-- previously logged stats, and perform operations over it in a nice way.
data FollowStats f = FollowStats
    { blocksApplied :: !(f Int)
    , rollbacks :: !(f Int)
    , tip :: !(f SlotNo)
    , time :: !(f UTCTime)
      -- ^ NOTE: Current time is not updated until @flush@ is called.
    , prog :: !(f SyncProgress)
      -- ^ NOTE: prog is not updated until @flush@ is called.
    } deriving (Generic)

-- It seems UTCTime contains thunks internally. This shouldn't matter as we
-- 1. Change it seldom - from @flush@, not from @updateStats@
-- 2. Set to a completely new value when we do change it.
deriving via (AllowThunksIn '["time"] (FollowStats LogState))
    instance (NoThunks (FollowStats LogState))

deriving instance Show (FollowStats LogState)
deriving instance Eq (FollowStats LogState)

-- | Change the @f@ wrapping each record field.
hoistStats
    :: (forall a. f a -> g a)
    -> FollowStats f
    -> FollowStats g
hoistStats f FollowStats{blocksApplied,rollbacks,tip,time,prog} = FollowStats
    { blocksApplied = f blocksApplied
    , rollbacks = f rollbacks
    , tip = f tip
    , time = f time
    , prog = f prog
    }

-- | For keeping track of what we have logged and what we have not.
--
-- The idea is to
-- 1. Reconstruct a model of the @current@ @state@ using a @Trace@
-- 2. Sometimes log the difference between the @current@ state and the most
-- recently logged one.
data LogState a = LogState
    { prev :: !a -- ^ Most previously logged state
    , current :: !a -- ^ Not-yet logged state
    } deriving (Eq, Show, Functor, Generic, NoThunks)

initLogState :: a -> LogState a
initLogState a = LogState a a

-- | Modify the current state of a @LogState state@
overCurrent :: (a -> a) -> LogState a -> LogState a
overCurrent f (LogState prev cur) = LogState prev (f cur)

-- | /The way/ to log the current stats.
--
-- Returns the current stats from the TMVar, and sets each @prev@ state to
-- @current@ as new value of the @TMVar@.
flush
    :: UTCTime
    -> (SlotNo -> IO SyncProgress)
    -> StrictTMVar IO (FollowStats LogState)
    -> IO (FollowStats LogState)
flush t calcSyncProgress var = do
    s <- atomically $ takeTMVar var
    p <- calcSyncProgress (current $ tip s)
    -- This is where we need to update the time and sync progress
    let s' = s { time = overCurrent (const t) (time s) }
               { prog = overCurrent (const p) (prog s) }
    atomically $ putTMVar var (hoistStats forgetPrev s')
    return s'
  where
    forgetPrev (LogState _prev cur) = LogState cur cur

emptyStats :: UTCTime -> FollowStats LogState
emptyStats t = FollowStats (f 0) (f 0) (f $ SlotNo 0) (f t) (f prog)
  where
    f = initLogState
    prog = NotResponding -- Hijacked as an initial value for simplicity.


-- | Update the stats based on a new log message.
updateStats :: FollowLog msg -> FollowStats LogState -> FollowStats LogState
updateStats msg s = case msg of
    MsgApplyBlocks _tip blocks ->
        s { blocksApplied = overCurrent (+ NE.length blocks) (blocksApplied s) }
    MsgDidRollback _ _ ->
        s { rollbacks = overCurrent (1 +) (rollbacks s) }
    MsgFollowerTip p ->
        s { tip = overCurrent (const $ slotFromMaybeBh p) (tip s) }
    _ -> s
  where
    slotFromMaybeBh = maybe (SlotNo 0) slotNo

instance ToText (FollowStats LogState) where
    toText st@(FollowStats b r tip t prog) = syncStatus <> " " <> stats <> sevExpl
      where
        syncStatus = case prog of
            LogState NotResponding Ready ->
                "In sync."
            LogState Ready Ready ->
                "Still in sync."
            LogState NotResponding NotResponding ->
                "Still not syncing."
            LogState (Syncing _p) Ready ->
                "In sync!"
            LogState Ready (Syncing p) ->
                "Fell out of sync (" <> (pretty p) <> ")"
            LogState _ (Syncing p) ->
                "Syncing (" <> (pretty p) <> ")"
            LogState prev NotResponding ->
                "Not responding. Previously " <> (pretty prev) <> "."
        stats = mconcat
            [ "Applied " <> pretty (using (-) b) <> " blocks, "
            , pretty (using (-) r) <> " rollbacks "
            , "in the last " <> pretty (using diffUTCTime t) <> ". "
            , "Currently at slot " <> pretty (current tip) <> "."
            ]
          where
            using f x = f (current x) (prev x)

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
explainedSeverityAnnotation :: FollowStats LogState -> (Severity, Maybe Text)
explainedSeverityAnnotation s
        | progressMovedBackwards = (Warning, Just "progress decreased")
        | noBlocks && notRestored = (Warning, Just "not applying blocks")
        | nowInSync = (Notice, Nothing)
        | otherwise = (Info, Nothing)
  where
    progressMovedBackwards = current (prog s) < prev (prog s)
    nowInSync = current (prog s) == Ready && prev (prog s) < Ready
    notRestored = current (prog s) /= Ready
    noBlocks = (current (blocksApplied s) - prev (blocksApplied s)) <= 0


instance HasSeverityAnnotation (FollowStats LogState) where
    getSeverityAnnotation = fst . explainedSeverityAnnotation

addFollowerLogging
    :: Monad m
    => Tracer m (FollowLog msg)
    -> ChainFollower m ChainPoint BlockHeader block
    -> ChainFollower m ChainPoint BlockHeader block
addFollowerLogging tr cf = ChainFollower
    { readLocalTip = do
        readLocalTip cf
    , rollForward = \tip blocks -> do
        traceWith tr $ MsgApplyBlocks tip (fromBlock <$> blocks)
        traceWith tr $ MsgFollowerTip (Just tip)
        rollForward cf tip blocks
    , rollBackward = \point -> do
        point' <- rollBackward cf point
        traceWith tr $ MsgDidRollback point point'
        pure point'
    }

-- | Starts a new thread for monitoring health and statistics from
-- the returned @FollowLog msg@.
withFollowStatsMonitoring
    :: Tracer IO (FollowLog msg)
    -> (SlotNo -> IO SyncProgress)
    -> ((Tracer IO (FollowLog msg)) -> IO ())
    -> IO ()
withFollowStatsMonitoring tr calcSyncProgress act = do
    t0' <- getCurrentTime
    var <- newTMVarIO $ emptyStats t0'
    let tr' = flip contramapM tr $ \msg -> do
            atomically $ do
                s <- takeTMVar var
                putTMVar var $! updateStats msg s
            pure msg
    traceWith tr' $ MsgFollowerTip Nothing
    race_
        (act tr')
        (loop var startupDelay)
  where
    loop var delay = do
        threadDelay delay
        t <- getCurrentTime
        s <- flush t calcSyncProgress var
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
