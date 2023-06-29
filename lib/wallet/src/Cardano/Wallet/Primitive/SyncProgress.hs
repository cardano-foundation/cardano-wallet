{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}

-- | Functionality for calculating @SyncProgress@ of wallets.
module Cardano.Wallet.Primitive.SyncProgress
    ( -- * Types
      SyncProgress (..)
    , SyncTolerance (..)
    , mkSyncTolerance

      -- * Implementations
    , syncProgress
    ) where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , interpretQuery
    , slotToRelTime
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Bifunctor
    ( bimap
    )
import Data.Either
    ( fromRight
    )
import Data.Quantity
    ( Percentage (..)
    , Quantity (..)
    , mkPercentage
    )
import Data.Ratio
    ( (%)
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Data.Time.Clock
    ( NominalDiffTime
    )
import Fmt
    ( Buildable
    , build
    )
import GHC.Generics
    ( Generic
    )
import GHC.Stack
    ( HasCallStack
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    , diffRelTime
    )

data SyncProgress
    = Ready
    | Syncing !(Quantity "percent" Percentage)
    | NotResponding
    deriving (Generic, Eq, Show)

instance NoThunks SyncProgress

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
mkSyncTolerance = SyncTolerance . toEnum . (* pico)
  where
    pico = 1_000_000_000_000

instance ToText SyncTolerance where
    toText (SyncTolerance t) = toText t

instance FromText SyncTolerance where
    fromText = bimap (const errSyncTolerance) SyncTolerance . fromText
      where
        errSyncTolerance =
            TextDecodingError
                $ unwords
                    [ "Cannot parse given time duration. Here are a few examples of"
                    , "valid text representing a sync tolerance: '3s', '3600s', '42s'."
                    ]

-- | Estimate restoration progress based on:
--
-- - The slot of the latest block consumed (our progress)
-- - The slot corresponding to the latest wall-clock time (our target)
--
-- The estimated progress is the quotient of these two quantities.
--
-- In the Cardano consensus protocol, only a fraction of slots contains blocks.
-- Hence, the progress percentage will often be < 100%,
-- as the slot corresponding to the current wall-clock time
-- may not be filled with a block.
-- The sync tolerance should be large enough to accommodate this issue.
syncProgress
    :: (HasCallStack, Monad m)
    => SyncTolerance
    -- ^ A time tolerance inside which we consider ourselves synced
    -> TimeInterpreter m
    -- ^ Converts slots to actual time.
    -> SlotNo
    -- ^ Slot of latest block consumed
    -> RelativeTime
    -- ^ Current wall clock time
    -> m SyncProgress
syncProgress (SyncTolerance tolerance) ti slot now = do
    timeCovered <- interpretQuery ti $ slotToRelTime slot
    let progress
            | now == start = 0
            | otherwise = convert timeCovered % convert now

    pure
        $ if withinTolerance timeCovered now
            then Ready
            else
                Syncing
                    . Quantity
                    . fromRight (error $ errMsg progress)
                    . mkPercentage
                    $ toRational progress
  where
    start = RelativeTime 0

    convert :: RelativeTime -> Int
    convert = round . (* 1_000) . getRelativeTime

    withinTolerance a b = b `diffRelTime` a <= tolerance

    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"
