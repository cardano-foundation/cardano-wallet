{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.Network.Logging.Aggregation
    ( FollowStats (..)
    , Rearview (..)
    , emptyStats
    , flushStats
    , overCurrent
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( ChainPoint (..)
    )
import Control.Concurrent.Class.MonadSTM
    ( atomically
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( StrictTMVar
    , putTMVar
    , takeTMVar
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( UTCTime
    , diffUTCTime
    )
import Fmt
    ( pretty
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( AllowThunksIn (..)
    , NoThunks (..)
    )

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
    }
    deriving (Generic)

-- It seems UTCTime contains thunks internally. This shouldn't matter as we
-- 1. Change it seldom - from @flush@, not from @updateStats@
-- 2. Set to a completely new value when we do change it.
deriving via
    (AllowThunksIn '["time"] (FollowStats Rearview))
    instance
        (NoThunks (FollowStats Rearview))

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
    { past :: !a
    -- ^ Most previously logged state
    , current :: !a
    -- ^ Not-yet logged state
    }
    deriving (Eq, Show, Functor, Generic)

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
        stats =
            mconcat
                [ "Applied " <> pretty (using (-) b) <> " blocks, "
                , pretty (using (-) r) <> " rollbacks "
                , "in the last " <> pretty (using diffUTCTime t) <> ". "
                , "Current tip is " <> pretty (current tip) <> "."
                ]
          where
            using f x = f (current x) (past x)

        sevExpl =
            maybe
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
    let s' =
            s{time = overCurrent (const t) (time s)}
                { prog = overCurrent (const p) (prog s)
                }
    atomically $ putTMVar var $ hoistStats forgetPast s'
    return s'
  where
    forgetPast (Rearview _past curr) = initRearview curr

-- See NOTE [PointSlotNo]
pseudoSlotNo :: ChainPoint -> SlotNo
pseudoSlotNo ChainPointAtGenesis = SlotNo 0
pseudoSlotNo (ChainPoint slot _) = slot

{- NOTE [PointSlotNo]

'SlotNo' cannot represent the genesis point.

Historical hack. The DB layer can't represent 'Origin' in the database,
instead we have mapped it to 'SlotNo 0', which is wrong.

Rolling back to SlotNo 0 instead of Origin is fine for followers starting
from genesis (which should be the majority of cases). Other, non-trivial
rollbacks to genesis cannot occur on mainnet (genesis is years within
stable part, and there were no rollbacks in byron).

Could possibly be problematic in the beginning of a testnet without a
byron era. /Perhaps/ this is what is happening in the
>>> [cardano-wallet.pools-engine:Error:1293] [2020-11-24 10:02:04.00 UTC]
>>> Couldn't store production for given block before it conflicts with
>>> another block. Conflicting block header is:
>>> 5bde7e7b<-[f1b35b98-4290#2008]
errors observed in the integration tests.

The issue has been partially fixed in that 'rollbackTo' now takes
a 'Slot' as argument, which can represent the 'Origin'.
However, the database itself mostly stores slot numbers.

FIXME LATER during ADP-1043: As we move towards in-memory data,
all slot numbers in the DB file will either be replaced by
the 'Slot' type, or handled slightly differently when it
is clear that the data cannot exist at the genesis point
(e.g. for TxHistory).

-}
