{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
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
    , interpretQuery
    , TimeInterpreterLog (..)

      -- ** Combinators for running queries
    , unsafeExtendSafeZone
    , neverFails
    , hoistTimeInterpreter
    , expectAndThrowFailures
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..) )
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
    )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( join, (>=>) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Monad.Trans.Reader
    ( ReaderT, ask, runReaderT )
import Control.Tracer
    ( Tracer, contramap, natTracer, nullTracer, traceWith )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( fromMaybe )
import Data.Text.Class
    ( ToText (..) )
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
    , mkInterpreter
    , qryFromExpr
    , slotToEpoch'
    , slotToWallclock
    , wallclockToSlot
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( neverForksSummary )

import qualified Cardano.Slotting.Slot as Cardano
import qualified Data.Text as T
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
currentRelativeTime :: MonadIO m => TimeInterpreter n -> m RelativeTime
currentRelativeTime =
    liftIO . getCurrentTimeRelativeFromStart . blockchainStartTime

-- | Note: This fails when the node is far enough behind that we in the present
-- are beyond its safe zone.
currentEpoch :: MonadIO m => TimeInterpreter m -> m EpochNo
currentEpoch ti = do
    now <- currentRelativeTime ti
    interpretQuery ti (ongoingSlotAt now >>= epochOf)

{-------------------------------------------------------------------------------
                                Time Interpreter
-------------------------------------------------------------------------------}

-- | A @TimeInterpreter@ is a way for the wallet to run things of type @Qry a@,
-- with a system start time as context.
data TimeInterpreter m = forall eras. TimeInterpreter
    { interpreter :: m (Interpreter eras)
    , blockchainStartTime :: StartTime
    , tracer :: Tracer m TimeInterpreterLog
    , handleResult :: forall a. Either PastHorizonException a -> m a
    }

data TimeInterpreterLog
    = MsgInterpreterPastHorizon
        (Maybe String) -- ^ Reason for why the failure should be impossible
        PastHorizonException
    deriving (Eq, Show)

instance HasSeverityAnnotation TimeInterpreterLog where
    getSeverityAnnotation = \case
        MsgInterpreterPastHorizon Nothing _ -> Notice
        MsgInterpreterPastHorizon _ _ -> Error

instance ToText TimeInterpreterLog where
    toText = \case
        MsgInterpreterPastHorizon Nothing e -> mconcat
            [ "Time interpreter queried past the horizon. "
            , "Full error is: "
            , T.pack (show e)
            ]
        MsgInterpreterPastHorizon (Just reason) e -> mconcat
            [ "Time interpreter queried past the horizon. "
            , "This should not happen because "
            , T.pack reason
            , " Full error is: "
            , T.pack (show e)
            ]

-- | Run a query.
interpretQuery
    :: HasCallStack
    => Monad m
    => TimeInterpreter m
    -> Qry a
    -> m a
interpretQuery (TimeInterpreter getI start tr handleRes) qry = do
    i <- getI
    let res = HF.interpretQuery i $ runReaderT qry start
    case res of
        Left e -> traceWith tr $ MsgInterpreterPastHorizon Nothing e
        Right _ -> pure ()
    handleRes res

-- | An 'Interpreter' for a single era, where the @SlottingParameters@ cannot
-- change.
--
-- Queries will never fail with @mkSingleEraInterpreter@.
mkSingleEraInterpreter
    :: HasCallStack
    => StartTime
    -> SlottingParameters
    -> TimeInterpreter Identity
mkSingleEraInterpreter start sp = TimeInterpreter
    { interpreter = pure int
    , blockchainStartTime = start
    , tracer = nullTracer
    , handleResult = either bomb pure
    }
  where
    int = mkInterpreter summary
    summary = neverForksSummary sz len
    sz = Cardano.EpochSize $ fromIntegral $ unEpochLength $ sp ^. #getEpochLength
    len = Cardano.mkSlotLength $ unSlotLength $ sp ^. #getSlotLength

    bomb x = error $ "mkSingleEraInterpreter: the impossible happened: " <> show x

-- | Set up a 'TimeInterpreter' for a given start time, and an 'Interpreter'
-- queried from the ledger layer.
mkTimeInterpreter
    :: Tracer IO TimeInterpreterLog
    -> StartTime
    -> IO (Interpreter eras)
    -> TimeInterpreter (ExceptT PastHorizonException IO)
mkTimeInterpreter tr start int = TimeInterpreter
    { interpreter = liftIO int
    , blockchainStartTime = start
    , tracer = natTracer liftIO tr
    , handleResult = ExceptT . pure
    }

{-------------------------------------------------------------------------------
                        Time Interpreter combinators
-------------------------------------------------------------------------------}

-- | Takes a motivation of why @TimeInterpreter@ shouldn't fail interpreting
-- queries.
--
-- Unexpected @PastHorizonException@s will be thrown in IO, and traced with
-- Error severity along with the provided motivation.
neverFails
    :: String
    -> TimeInterpreter (ExceptT PastHorizonException IO)
    -> TimeInterpreter IO
neverFails reason = f . hoistTimeInterpreter (runExceptT >=> eitherToIO)
  where
    eitherToIO (Right x) = pure x
    eitherToIO (Left e) = throwIO e

    f (TimeInterpreter getI ss tr h) = TimeInterpreter
        { interpreter = getI
        , blockchainStartTime = ss
        , tracer = contramap (setReason reason) tr
        , handleResult = h
        }
    setReason r (MsgInterpreterPastHorizon _ e)
        = MsgInterpreterPastHorizon (Just r) e

-- | Makes @PastHorizonException@ be thrown in @IO@.
--
-- Will /not/ cause @PastHorizonException@ to be tracer with Error severity,
-- unlike @neverFails@.
expectAndThrowFailures
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> TimeInterpreter IO
expectAndThrowFailures = hoistTimeInterpreter (runExceptT >=> eitherToIO)
  where
    eitherToIO (Right x) = pure x
    eitherToIO (Left e) = throwIO e

-- | Change the underlying monad of the TimeInterpreter with a natural
-- transformation.
hoistTimeInterpreter
    :: (forall a. m a -> n a)
    -> TimeInterpreter m
    -> TimeInterpreter n
hoistTimeInterpreter f (TimeInterpreter getI ss tr h) = TimeInterpreter
    { interpreter = f getI
     -- NOTE: interpreter ti cannot throw PastHorizonException, but
     -- this way we don't have to carry around yet another type parameter.
    , blockchainStartTime = ss
    , tracer = natTracer f tr
    , handleResult = f . h
    }

-- | Extend the safe zone to make the TimeInterpreter return predictions where
-- it otherwise would have failed with @PastHorizonException@. This should be
-- used with great caution, and if we can get away from it, that would also be
-- great. Also ADP-575.
--
-- From the underlying ouroboros-consensus function:
--
-- UNSAFE: extend the safe zone of the current era of the given 'Interpreter'
-- to be /unbounded/, ignoring any future hard forks.
--
-- This only has effect when the 'Interpreter' was obtained in an era that was
-- /not the final one/ (in the final era, this is a no-op). The 'Interpreter'
-- will be made to believe that the current era is the final era, making its
-- horizon unbounded, and thus never returning a 'PastHorizonException'.
--
-- Use of this function is /strongly discouraged/, as it will ignore any future
-- hard forks, and the results produced by the 'Interpreter' can thus be
-- incorrect.
unsafeExtendSafeZone
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> TimeInterpreter IO
unsafeExtendSafeZone = f . neverFails r
  where
    f (TimeInterpreter getI ss tr h) = TimeInterpreter
        { interpreter = HF.unsafeExtendSafeZone <$> getI
        , blockchainStartTime = ss
        , tracer = tr
        , handleResult = h
        }
    r = "unsafeExtendSafeZone should make PastHorizonExceptions impossible."
