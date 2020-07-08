{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Functionality for calculating @SyncProgress@ of wallets.
module Cardano.Wallet.Primitive.SyncProgress
    ( -- * Types
      SyncProgress (..)
    , SyncTolerance (..)
    , mkSyncTolerance

      -- * Implementations
    , syncProgress
    , syncProgressRelativeToTime
    , )
    where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( SlotParameters (..), flatSlot, slotAt )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , SlotId (..)
    , SlotLength (..)
    , distance
    )
import Control.DeepSeq
    ( NFData (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime )
import Fmt
    ( Buildable, build )
import GHC.Generics
    ( Generic )
import Safe
    ( readMay )

import qualified Data.Text as T

data SyncProgress
    = Ready
    | Syncing !(Quantity "percent" Percentage)
    | NotResponding
    deriving (Generic, Eq, Show)

instance NFData SyncProgress

instance Ord SyncProgress where
    NotResponding <= _ = True
    _ <= NotResponding = False
    Ready <= Ready = True
    Ready <= Syncing _ = False
    Syncing _ <= Ready = True
    Syncing a <= Syncing b = a <= b

instance Buildable SyncProgress where
    build = \case
        Ready ->
            "restored"
        Syncing (Quantity p) ->
            "still restoring (" <> build (toText p) <> ")"
        NotResponding ->
            "not responding"


newtype SyncTolerance = SyncTolerance NominalDiffTime
    deriving stock (Generic, Eq, Show)

-- | Construct a 'SyncTolerance' from a number of __seconds__
mkSyncTolerance :: Int -> SyncTolerance
mkSyncTolerance =
    SyncTolerance . toEnum . (* pico)
  where
    pico = 1_000_000_000_000

instance ToText SyncTolerance where
    toText (SyncTolerance t) = T.pack (show t)

instance FromText SyncTolerance where
    fromText t = case T.splitOn "s" t of
        [v,""] ->
            maybe
                (Left errSyncTolerance)
                (Right . mkSyncTolerance)
                (readMay $ T.unpack v)
        _ ->
            Left errSyncTolerance
      where
        errSyncTolerance = TextDecodingError $ unwords
            [ "Cannot parse given time duration. Here are a few examples of"
            , "valid text representing a sync tolerance: '3s', '3600s', '42s'."
            ]

-- | Estimate restoration progress based on:
--
-- - The current local tip
-- - The last slot
--
-- For the sake of this calculation, we are somewhat conflating the definitions
-- of slots and block height. Because we can't reliably _trust_ that the current
-- node is actually itself synced with the network. So, we compute the progress
-- as:
--
-- @
-- p = h / (h + X)
-- @
--
-- Where:
--
-- - @h@: the number of blocks we have ingested so far.
-- - @X@: the estimatd remaining slots to reach the network tip.
--
-- Initially, `X` gives a relatively poor estimation of the network height, as
-- it assumes that every next slot will be a block. But, as we ingest blocks,
-- `h` becomes bigger and `X` becomes smaller making the progress estimation
-- better and better. At some point, `X` is null, and we have `p = h / h`
syncProgress
    :: SyncTolerance
        -- ^ A time tolerance inside which we consider ourselves synced
    -> SlotParameters
        -- ^ Parameters for slot arithmetic which are assumed to be static.
        --
        -- NOTE: This is no longer the case with the Hard Fork Combinator and
        -- Byron-to-Shelley era transition.
    -> BlockHeader
        -- ^ Local tip
    -> SlotId
        -- ^ Last slot that could have been produced
    -> SyncProgress
syncProgress (SyncTolerance timeTolerance) sp tip slotNow =
    let
        bhTip = fromIntegral . getQuantity $ blockHeight tip

        epochLength = getEpochLength sp
        (SlotLength slotLength)  = getSlotLength sp

        n0 = flatSlot epochLength (slotId tip)
        n1 = flatSlot epochLength slotNow

        tolerance = floor (timeTolerance / slotLength)

        remainingSlots = fromIntegral $ n1 - n0

        ActiveSlotCoefficient f = getActiveSlotCoefficient sp
        remainingBlocks = round (remainingSlots * f)

        -- Using (max 1) to avoid division by 0.
        progress = bhTip % (max 1 (bhTip + remainingBlocks))
    in if distance n1 n0 < tolerance || n0 >= n1 || progress >= 1 then
        Ready
    else
        Syncing
        . Quantity
        . either (const . error $ errMsg progress) id
        . mkPercentage
        $ progress
  where
    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"

-- | Helper to compare the /local tip/ with the slot corresponding to a
-- @UTCTime@, and calculate progress based on that.
syncProgressRelativeToTime
    :: SyncTolerance
        -- ^ A time tolerance inside which we consider ourselves synced
    -> SlotParameters
        -- ^ Parameters for slot arithmetic which are assumed to be static.
        --
        -- NOTE: This is no longer the case with the Hard Fork Combinator and
        -- Byron-to-Shelley era transition.
    -> BlockHeader
    -- ^ Local tip
    -> UTCTime
    -- ^ Where we believe the network tip is (e.g. @getCurrentTime@).
    -> SyncProgress
syncProgressRelativeToTime tolerance sp tip time =
    maybe
        (Syncing minBound)
        (syncProgress tolerance sp tip)
        (slotAt sp time)
