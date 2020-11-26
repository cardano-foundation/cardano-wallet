{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Contains tools for converting between @SlotNo@, @EpochNo@, @SlotInEpoch@,
-- @UTCTime@.

module Cardano.Wallet.Primitive.Slotting
    ( -- ** Queries
      Qry
    , currentEpoch
    , epochOf
    , slotToUTCTime
    , slotToRelTime
    , toSlotId
    , slotRangeFromRelativeTimeRange
    , slotRangeFromTimeRange
    , firstSlotInEpoch
    , ongoingSlotAt
    , ceilingSlotAt
    , timeOfEpoch
    , getStartTime
    , querySlotLength
    , queryEpochLength

      -- ** Blockchain-relative times
    , RelativeTime
    , toRelativeTime
    , toRelativeTimeRange
    , fromRelativeTime
    , addRelTime

      -- ** What's the time?
    , currentRelativeTime
    , getCurrentTimeRelativeFromStart

      -- ** Running queries
    , TimeInterpreter
    , mkSingleEraInterpreter
    , mkTimeInterpreter
    , PastHorizonException (..)

      -- ** Helpers
    , unsafeEpochNo
    ) where

import Prelude

import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types
    ( EpochLength (..)
    , EpochNo (..)
    , Range (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , unsafeEpochNo
    )
import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Reader
    ( ReaderT, ask, runReaderT )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( fromMaybe )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime, getCurrentTime )
import Data.Word
    ( Word32, Word64 )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), SystemStart (..), addRelTime )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Expr (..)
    , Interpreter
    , PastHorizonException (..)
    , epochToSize
    , epochToSlot'
    , interpretQuery
    , mkInterpreter
    , qryFromExpr
    , slotToEpoch'
    , slotToWallclock
    , wallclockToSlot
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( neverForksSummary )

import qualified Cardano.Slotting.Slot as Cardano
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Cardano
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF

{-------------------------------------------------------------------------------
                                    Queries
-------------------------------------------------------------------------------}

-- | This is 'Ouroboros.Consensus.HardFork.History.Qry.Qry' wrapped in a reader
-- to provide the blockchain system start time as context.
type Qry = ReaderT StartTime HF.Qry

-- | Query the blockchain start time. This is part of the 'TimeInterpreter'
-- environment.
getStartTime :: Qry StartTime
getStartTime = ask

-- | Query the epoch corresponding to a flat slot number.
epochOf :: SlotNo -> Qry EpochNo
epochOf slot = epochNumber <$> toSlotId slot

-- | Query to convert a flat 'SlotNo' to a 'SlotId', which is the epoch number,
-- and the local slot index.
toSlotId :: SlotNo -> Qry SlotId
toSlotId slot = do
    (e, s) <- lift $ slotToEpoch' slot
    return $ SlotId
        (EpochNo $ fromIntegral $ Cardano.unEpochNo e)
        (SlotInEpoch $ downCast s)
  where
    downCast :: Word64 -> Word32
    downCast = fromIntegral

-- | Query the absolute time at which a slot starts.
slotToUTCTime :: SlotNo -> Qry UTCTime
slotToUTCTime sl = slotToRelTime sl >>= fromRelativeTime

-- | Query the relative time at which a slot starts.
slotToRelTime :: SlotNo -> Qry RelativeTime
slotToRelTime = lift . fmap fst . slotToWallclock

-- | Query the absolute times at which an epoch starts and ends.
--
-- Querying the end time of /this/ epoch is preferable to querying the start
-- time of the /next/ epoch, because the next epoch may be outside the forecast
-- range, and result in 'PastHorizonException'.
timeOfEpoch :: EpochNo -> Qry (UTCTime, UTCTime)
timeOfEpoch epoch = do
    ref <- firstSlotInEpoch epoch
    refTime <- slotToUTCTime ref
    el <- lift $ qryFromExpr $ EEpochSize $ ELit $ toCardanoEpochNo epoch
    sl <- lift $ qryFromExpr $ ESlotLength $ ELit ref

    let convert = fromRational . toRational
    let el' = convert $ Cardano.unEpochSize el
    let sl' = Cardano.getSlotLength sl

    let timeInEpoch = el' * sl'

    return (refTime, timeInEpoch `addUTCTime` refTime)
  where
    toCardanoEpochNo (EpochNo e) = Cardano.EpochNo $ fromIntegral e

-- | Translate 'EpochNo' to the 'SlotNo' of the first slot in that epoch
firstSlotInEpoch :: EpochNo -> Qry SlotNo
firstSlotInEpoch = lift . epochToSlot' . convertEpochNo
  where
    convertEpochNo (EpochNo e) = Cardano.EpochNo $ fromIntegral e

-- @@
--     slot:
--     |1--------|2----------
--
--     result of onGoingSlotAt:
--     ●---------○
--          1
--               ●----------○
--                    2
-- @@
--
--
ongoingSlotAt :: RelativeTime -> Qry SlotNo
ongoingSlotAt = fmap fst . slotAtTimeDetailed

-- @@
--     slot:
--     |1--------|2----------
--
--     result of ceilingSlotAt:
--     ○---------●
--          2
--               ○----------●
--                    3
-- @@
--
ceilingSlotAt :: RelativeTime -> Qry SlotNo
ceilingSlotAt = fmap ceil2 . slotAtTimeDetailed
  where
    ceil2 (s, 0) = s
    ceil2 (s, _) = s + 1

-- | Helper that returns @(slot, elapsedTimeInSlot)@ for a
-- given @UTCTime@.
slotAtTimeDetailed :: RelativeTime -> Qry (SlotNo, NominalDiffTime)
slotAtTimeDetailed = lift . fmap dropThird . wallclockToSlot
  where
    dropThird (a, b, _) = (a, b)

querySlotLength :: SlotNo -> Qry SlotLength
querySlotLength sl =
    lift $ fmap (SlotLength . Cardano.getSlotLength) $
    qryFromExpr $ ESlotLength $ ELit sl

queryEpochLength :: SlotNo -> Qry EpochLength
queryEpochLength sl = lift $ toEpochLength <$> do
    (e, _) <- slotToEpoch' sl
    epochToSize e
  where
    -- converting up from Word32 to Word64
    toEpochLength = EpochLength . fromIntegral . Cardano.unEpochSize

-- | This function returns a chain-relative time range if (and only if) the
-- specified UTC time range intersects with the life of the blockchain.
--
-- If, on the other hand, the specified time range terminates before the start
-- of the blockchain, this function returns 'Nothing'.
toRelativeTimeRange :: Range UTCTime -> StartTime -> Maybe (Range RelativeTime)
toRelativeTimeRange range start = case toRelativeTime start <$> range of
    Range _ (Just Nothing) -> Nothing
    Range a b -> Just (Range (fromMaybe (RelativeTime 0) <$> a) (join b))

-- | Transforms the given inclusive time range into an inclusive slot range.
slotRangeFromRelativeTimeRange :: Range RelativeTime -> Qry (Range SlotNo)
slotRangeFromRelativeTimeRange (Range a b) =
    Range <$> traverse ceilingSlotAt a <*> traverse ongoingSlotAt b

slotRangeFromTimeRange :: Range UTCTime -> Qry (Maybe (Range SlotNo))
slotRangeFromTimeRange range = mapM slotRangeFromRelativeTimeRange
    =<< (toRelativeTimeRange range <$> getStartTime)

{-------------------------------------------------------------------------------
                            Blockchain-relative time
-------------------------------------------------------------------------------}

-- | Same as 'Cardano.toRelativeTime', but has error handling for times before
-- the system start. No other functions in this module will accept UTC times.
toRelativeTime :: StartTime -> UTCTime -> Maybe RelativeTime
toRelativeTime (StartTime start) utc
    | utc < start = Nothing
    | otherwise = Just $ Cardano.toRelativeTime (SystemStart start) utc

-- | Convert an absolute time to a relative time. If the absolute time is before
-- the system start, consider the relative time to be the system start
-- time. This function can never fail.
toRelativeTimeOrZero :: StartTime -> UTCTime -> RelativeTime
toRelativeTimeOrZero start = fromMaybe (RelativeTime 0) . toRelativeTime start

-- | Query the absolute time corresponding to a blockchain-relative time.
fromRelativeTime :: RelativeTime -> Qry UTCTime
fromRelativeTime t = do
    start <- getStartTime
    pure (Cardano.fromRelativeTime (coerce start) t)

{-------------------------------------------------------------------------------
                                What's the time?
-------------------------------------------------------------------------------}

-- | The current system time, compared to the given blockchain start time.
--
-- If the current time is before the system start (this would only happen when
-- launching testnets), let's just say we're in epoch 0.
--
-- TODO: Use io-sim-classes for easier testing.
getCurrentTimeRelativeFromStart :: StartTime -> IO RelativeTime
getCurrentTimeRelativeFromStart start =
    toRelativeTimeOrZero start <$> getCurrentTime

-- | The current system time, compared to the blockchain start time of the given
-- 'TimeInterpreter'.
--
-- If the current time is before the system start (this would only happen when
-- launching testnets), the relative time is reported as 0.
currentRelativeTime :: MonadIO m => TimeInterpreter m -> m RelativeTime
currentRelativeTime ti =
    ti getStartTime >>= liftIO . getCurrentTimeRelativeFromStart

-- | Note: This fails when the node is far enough behind that we in the present
-- are beyond its safe zone.
currentEpoch :: MonadIO m => TimeInterpreter m -> m EpochNo
currentEpoch ti = do
    now <- currentRelativeTime ti
    ti (ongoingSlotAt now >>= epochOf)

{-------------------------------------------------------------------------------
                                Time Interpreter
-------------------------------------------------------------------------------}

-- | A @TimeInterpreter@ is a way for the wallet to run things of type @Qry a@,
-- with a system start time as context.
--
-- NOTE: Do not hold on to 'TimeInterpreter m' references. Always get fresh ones
-- from the network layer.
--
type TimeInterpreter m = forall a. Qry a -> m a

-- | An 'Interpreter' for a single era, where the @SlottingParameters@ cannot
-- change.
--
-- Queries will never fail with @mkSingleEraInterpreter@.
mkSingleEraInterpreter
    :: HasCallStack
    => StartTime
    -> SlottingParameters
    -> TimeInterpreter Identity
mkSingleEraInterpreter start sp = neverFails . mkTimeInterpreter start int
  where
    int = mkInterpreter summary
    summary = neverForksSummary sz len
    sz = Cardano.EpochSize $ fromIntegral $ unEpochLength $ sp ^. #getEpochLength
    len = Cardano.mkSlotLength $ unSlotLength $ sp ^. #getSlotLength

    neverFails = either bomb pure
    bomb x = error $ "mkSingleEraInterpreter: the impossible happened: " <> show x

-- | Set up a 'TimeInterpreter' for a given start time, and an 'Interpreter'
-- queried from the ledger layer.
mkTimeInterpreter
    :: HasCallStack
    => StartTime
    -> Interpreter xs
    -> TimeInterpreter (Either PastHorizonException)
mkTimeInterpreter start int qry = interpretQuery int (runReaderT qry start)
