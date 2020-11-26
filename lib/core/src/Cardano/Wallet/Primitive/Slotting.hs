{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, addUTCTime, getCurrentTime )
import Data.Word
    ( Word32, Word64 )
import GHC.Stack
    ( HasCallStack )
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
