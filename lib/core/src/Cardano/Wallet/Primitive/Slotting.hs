{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Contains tools for converting between @SlotNo@, @EpochNo@, @SlotInEpoch@,
-- @UTCTime@.

module Cardano.Wallet.Primitive.Slotting
    ( -- * New api using ouroboros-concensus
      -- ** Queries
      currentEpoch
    , epochAt
    , epochOf
    , startTime
    , toSlotId
    , slotRangeFromTimeRange
    , firstSlotInEpoch
    , ongoingSlotAt
    , ceilingSlotAt
    , endTimeOfEpoch
    , querySlotLength
    , queryEpochLength

    -- ** Running queries
    , TimeInterpreter
    , singleEraInterpreter
    , mkTimeInterpreter
    , HF.PastHorizonException (..)
    , Qry

    -- ** Helpers
    , unsafeEpochNo
    , epochPred
    , epochSucc

      -- * Legacy api - Inaccurate with cardano-node, okay with Jörmungandr
    , SlotParameters (..)
    , slotParams
    , epochStartTime
    , flatSlot
    , fromFlatSlot
    , slotStartTime
    , slotCeiling
    , slotFloor
    , slotAt'
    , slotDifference
    , slotPred
    , slotSucc
    , slotMinBound
    , slotRangeFromTimeRange'
    ) where

import Prelude

import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , EpochNo (..)
    , Range (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , unsafeEpochNo
    , wholeRange
    )
import Control.Monad
    ( ap, liftM, (<=<) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime )
import Data.Word
    ( Word32, Word64 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter, mkInterpreter )
import Ouroboros.Consensus.HardFork.History.Summary
    ( neverForksSummary )

import qualified Cardano.Slotting.Slot as Cardano
import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Cardano
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF

-- -----------------------------------------------------------------------------
-- New Api using ouroboros-consensus. With the right interpreter, the
-- calculations don't break on hard-forks.


--
-- Queries
--

currentEpoch :: MonadIO m => TimeInterpreter m -> m (Maybe EpochNo)
currentEpoch ti = ti . epochAt =<< liftIO getCurrentTime

epochAt :: UTCTime -> Qry (Maybe EpochNo)
epochAt = traverse epochOf <=< ongoingSlotAt

epochOf :: SlotNo -> Qry EpochNo
epochOf slot = epochNumber <$> toSlotId slot

toSlotId :: SlotNo -> Qry SlotId
toSlotId slot = HardForkQry $ do
    (e, s, _) <- HF.slotToEpoch slot
    return $ SlotId
        (EpochNo $ fromIntegral $ Cardano.unEpochNo e)
        (SlotInEpoch $ unsafeConvert s)
  where
    unsafeConvert :: Word64 -> Word32
    unsafeConvert = fromIntegral

startTime :: SlotNo -> Qry UTCTime
startTime s = do
    rel <- HardForkQry (fst <$> HF.slotToWallclock s)
    RelToUTCTime rel

-- | Can be used to know when the next epoch starts.
--
-- This is preferable to asking for the start time of the /next/ epoch, because
-- the next epoch may be outside the forecast range, and result in
-- @PastHorizonException@.
endTimeOfEpoch :: EpochNo -> Qry UTCTime
endTimeOfEpoch epoch = do
    ref <- firstSlotInEpoch epoch
    refTime <- startTime ref
    el <- HardForkQry $ HF.qryFromExpr $ HF.EEpochSize $ HF.ELit $ toCardanoEpochNo epoch
    sl <- HardForkQry $ HF.qryFromExpr $ HF.ESlotLength $ HF.ELit ref

    let convert = fromRational . toRational
    let el' = convert $ Cardano.unEpochSize el
    let sl' = Cardano.getSlotLength sl

    let timeInEpoch = el' * sl'

    return $ timeInEpoch `addUTCTime` refTime
  where
    toCardanoEpochNo (EpochNo e) = Cardano.EpochNo $ fromIntegral e

-- | Translate 'EpochNo' to the 'SlotNo' of the first slot in that epoch
firstSlotInEpoch :: EpochNo -> Qry SlotNo
firstSlotInEpoch = fmap fst . HardForkQry . HF.epochToSlot . convertEpochNo
  where
    convertEpochNo (EpochNo e) = Cardano.EpochNo $ fromIntegral e

-- | Transforms the given inclusive time range into an inclusive slot range.
--
-- This function returns a slot range if (and only if) the specified time range
-- intersects with the life of the blockchain.
--
-- If, on the other hand, the specified time range terminates before the start
-- of the blockchain, this function returns 'Nothing'.
slotRangeFromTimeRange
    :: Range UTCTime
    -> Qry (Maybe (Range SlotNo))
slotRangeFromTimeRange = \case
    Range Nothing Nothing -> do
        pure $ Just wholeRange

    Range (Just inf) Nothing -> do
        inf' <- Just <$> ceilingSlotAt inf
        pure $ Just $ Range inf' Nothing

    Range Nothing (Just sup) -> do
        sup' <- ongoingSlotAt sup
        pure $ (Range Nothing . Just) <$> sup'

    Range (Just inf) (Just sup) -> do
        inf' <- Just <$> ceilingSlotAt inf
        sup' <- ongoingSlotAt sup
        pure $ (Range inf' . Just) <$> sup'

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
ongoingSlotAt :: UTCTime -> Qry (Maybe SlotNo)
ongoingSlotAt x = do
     slotAtTimeDetailed x >>= \case
        Just (slot, _timeInSlot, _timeRemainingInSlot) -> pure $ Just slot
        Nothing -> pure Nothing

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
ceilingSlotAt :: UTCTime -> Qry SlotNo
ceilingSlotAt t = do
     slotAtTimeDetailed t >>= \case
        Just (s, 0, _) -> return s
        Just (s, _, _) -> return (s + 1)
        Nothing -> do
            return $ SlotNo 0

-- | Helper that returns @(slot, elapsedTimeInSlot, remainingTimeInSlot)@ for a
-- given @UTCTime@.
slotAtTimeDetailed
    :: UTCTime
    -> Qry (Maybe (SlotNo, NominalDiffTime, NominalDiffTime))
slotAtTimeDetailed t = do
    UTCTimeToRel t >>= \case
        Just relTime -> fmap Just $ HardForkQry $ HF.wallclockToSlot relTime
        Nothing -> return Nothing

querySlotLength :: SlotNo -> Qry SlotLength
querySlotLength sl =
    fmap (SlotLength . Cardano.getSlotLength) $
    HardForkQry $
    HF.qryFromExpr $
    HF.ESlotLength $
    HF.ELit sl

queryEpochLength :: SlotNo -> Qry EpochLength
queryEpochLength sl = fmap toEpochLength $ HardForkQry $ do
    (e, _, _) <- HF.slotToEpoch sl
    HF.epochToSize e
  where
    -- converting up from Word32 to Word64
    toEpochLength = EpochLength . fromIntegral . Cardano.unEpochSize

-- A @TimeInterpreter@ is a way for the wallet to run things of type @Qry a@.
--
-- NOTE:
-- A @TimeInterpreter@ could in theory decide to update the era summary from the
-- node when running a query.
--
-- We cannot manually specify when the fetching happens.
--
-- This may or may not be what we actually want.
--
type TimeInterpreter m = forall a. Qry a -> m a

-- | An 'Interpreter' for a single era, where the slotting from
-- @GenesisParameters@ cannot change.
--
-- Queries can never fail with @singleEraInterpreter@. This function will throw
-- a 'PastHorizonException' if they do.
singleEraInterpreter
    :: HasCallStack
    => StartTime
    -> SlottingParameters
    -> TimeInterpreter Identity
singleEraInterpreter genesisBlockTime sp = mkTimeInterpreterI (mkInterpreter summary)
  where
    summary = neverForksSummary sz len
    sz = Cardano.EpochSize $ fromIntegral $ unEpochLength $ sp ^. #getEpochLength
    len = Cardano.mkSlotLength $ unSlotLength $ sp ^. #getSlotLength

    mkTimeInterpreterI
        :: HasCallStack
        => Interpreter xs
        -> TimeInterpreter Identity
    mkTimeInterpreterI int q = neverFails $ runQuery start int q
      where
        start = coerce genesisBlockTime

        neverFails = either bomb pure
        bomb x = error $ "singleEraInterpreter: the impossible happened: " <> show x

mkTimeInterpreter
    :: HasCallStack
    => StartTime
    -> Interpreter xs
    -> TimeInterpreter (Either HF.PastHorizonException)
mkTimeInterpreter start =
    runQuery (coerce start)

-- | Wrapper around HF.Qry to allow converting times relative to the genesis
-- block date to absolute ones
data Qry :: * -> * where
    HardForkQry  :: HF.Qry a -> Qry a
    RelToUTCTime :: Cardano.RelativeTime -> Qry UTCTime
    UTCTimeToRel :: UTCTime -> Qry (Maybe Cardano.RelativeTime)
    QPure :: a -> Qry a
    QBind :: Qry a -> (a -> Qry b) -> Qry b

instance Functor Qry where
  fmap = liftM

instance Applicative Qry where
  pure  = QPure
  (<*>) = ap

instance Monad Qry where
  return = pure
  (>>=)  = QBind

runQuery
    :: HasCallStack
    => SystemStart
    -> Interpreter xs
    -> Qry a
    -> Either HF.PastHorizonException a
runQuery systemStart int = go
  where
    go :: Qry a -> Either HF.PastHorizonException a
    go (HardForkQry q) = HF.interpretQuery int q
    go (QPure a) =
        return a
    go (QBind x f) = do
        go x >>= go . f
    go (RelToUTCTime rel) =
        pure $ Cardano.fromRelativeTime systemStart rel
    go (UTCTimeToRel utc)
        -- Cardano.toRelativeTime may throw, so we need this guard:
        | utc < getSystemStart systemStart = pure Nothing
        | otherwise = pure $ Just $ Cardano.toRelativeTime systemStart utc

-- -----------------------------------------------------------------------------
-- Legacy functions
-- These only work for a single era. We need to stop using them

-- | The essential parameters necessary for performing slot arithmetic.
data SlotParameters = SlotParameters
    { getEpochLength
        :: EpochLength
    , getSlotLength
        :: SlotLength
    , getGenesisBlockDate
        :: StartTime
    , getActiveSlotCoefficient
        :: ActiveSlotCoefficient
    } deriving (Eq, Generic, Show)

slotParams :: StartTime -> SlottingParameters -> SlotParameters
slotParams t0 sp = SlotParameters
    (sp ^. #getEpochLength)
    (sp ^. #getSlotLength)
    t0
    (sp ^. #getActiveSlotCoefficient)

-- | Calculate the time at which an epoch begins.
epochStartTime :: SlotParameters -> EpochNo -> UTCTime
epochStartTime sps e = slotStartTime sps $ SlotId e 0

-- | Return the epoch immediately before the given epoch, or 'Nothing' if there
--   is no representable epoch before the given epoch.
epochPred :: EpochNo -> Maybe EpochNo
epochPred (EpochNo e)
    | e == minBound = Nothing
    | otherwise = Just $ EpochNo $ pred e

-- | Return the epoch immediately after the given epoch, or 'Nothing' if there
--   is no representable epoch after the given epoch.
epochSucc :: EpochNo -> Maybe EpochNo
epochSucc (EpochNo e)
    | e == maxBound = Nothing
    | otherwise = Just $ EpochNo $ succ e

-- | Convert a 'SlotId' to the number of slots since genesis.
flatSlot :: EpochLength -> SlotId -> Word64
flatSlot (EpochLength epochLength) (SlotId (EpochNo e) (SlotInEpoch s)) =
    fromIntegral epochLength * fromIntegral e + fromIntegral s

-- | Convert a 'flatSlot' index to 'SlotId'.
--
-- This function will fail if applied to a value that is higher than the maximum
-- value of 'flatSlot' for the specified 'EpochLength'.
--
fromFlatSlot :: EpochLength -> Word64 -> SlotId
fromFlatSlot el@(EpochLength epochLength) n
    | n <= maxFlatSlot =
        SlotId (EpochNo $ fromIntegral e) (fromIntegral s)
    | otherwise =
        error $ mconcat
            [ "fromFlatSlot: The specified flat slot number ("
            , show n
            , ") is higher than the maximum flat slot number ("
            , show maxFlatSlot
            , ") for the specified epoch length ("
            , show epochLength
            , ")."
            ]
  where
    e = n `div` fromIntegral epochLength
    s = n `mod` fromIntegral epochLength
    maxFlatSlot =
        flatSlot el (SlotId (EpochNo maxBound) (SlotInEpoch $ epochLength - 1))

-- | @slotDifference a b@ is how many slots @a@ is after @b@. The result is
-- non-negative, and if @b > a@ then this function returns zero.
slotDifference :: SlotParameters -> SlotId -> SlotId -> Quantity "slot" Natural
slotDifference (SlotParameters el _ _ _) a b
    | a' > b' = Quantity $ fromIntegral $ a' - b'
    | otherwise = Quantity 0
  where
    a' = flatSlot el a
    b' = flatSlot el b

-- | Return the slot immediately before the given slot.
slotPred :: SlotParameters -> SlotId -> Maybe SlotId
slotPred (SlotParameters (EpochLength el) _ _ _) (SlotId en sn)
    | en == 0 && sn == 0 = Nothing
    | sn > 0 = Just $ SlotId en (sn - 1)
    | otherwise = Just $ SlotId (en - 1) (SlotInEpoch $ el - 1)

-- | Return the slot immediately after the given slot.
slotSucc :: SlotParameters -> SlotId -> SlotId
slotSucc (SlotParameters (EpochLength el) _ _ _) (SlotId en (SlotInEpoch sn))
    | sn < el - 1 = SlotId en (SlotInEpoch $ sn + 1)
    | otherwise = SlotId (en + 1) 0

-- | The time when a slot begins.
slotStartTime :: SlotParameters -> SlotId -> UTCTime
slotStartTime (SlotParameters el (SlotLength sl) (StartTime st) _) slot =
    addUTCTime offset st
  where
    offset = sl * fromIntegral (flatSlot el slot)

-- | For the given time 't', determine the ID of the earliest slot with start
--   time 's' such that 't ≤ s'.
slotCeiling :: SlotParameters -> UTCTime -> SlotId
slotCeiling sp@(SlotParameters _ (SlotLength sl) _ _) t =
    fromMaybe slotMinBound $ slotAt' sp (addUTCTime (pred sl) t)

-- | For the given time 't', determine the ID of the latest slot with start
--   time 's' such that 's ≤ t'.
slotFloor :: SlotParameters -> UTCTime -> Maybe SlotId
slotFloor = slotAt'

-- | Returns the earliest slot.
slotMinBound :: SlotId
slotMinBound = SlotId 0 0

-- | For the given time 't', determine the ID of the unique slot with start
--   time 's' and end time 'e' such that 's ≤ t ≤ e'.
slotAt' :: SlotParameters -> UTCTime -> Maybe SlotId
slotAt' (SlotParameters (EpochLength el) (SlotLength sl) (StartTime st) _) t
    | t < st = Nothing
    | otherwise = Just $ SlotId {epochNumber, slotNumber}
  where
    diff :: NominalDiffTime
    diff = t `diffUTCTime` st

    epochLength :: NominalDiffTime
    epochLength = fromIntegral el * sl

    epochNumber = EpochNo $
        floor (diff / epochLength)

    slotNumber = SlotInEpoch $
        floor ((diff - fromIntegral (unEpochNo epochNumber) * epochLength) / sl)

-- | Transforms the given inclusive time range into an inclusive slot range.
--
-- This function returns a slot range if (and only if) the specified time range
-- intersects with the life of the blockchain.
--
-- If, on the other hand, the specified time range terminates before the start
-- of the blockchain, this function returns 'Nothing'.
--
slotRangeFromTimeRange'
    :: SlotParameters
    -> Range UTCTime
    -> Maybe (Range SlotId)
slotRangeFromTimeRange' sps (Range mStart mEnd) =
    Range slotStart <$> slotEnd
  where
    slotStart =
        slotCeiling sps <$> mStart
    slotEnd =
        maybe (Just Nothing) (fmap Just . slotFloor sps) mEnd
