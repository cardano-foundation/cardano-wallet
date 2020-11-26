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
    ( TimeInterpreter, slotToRelTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bifunctor
    ( bimap )
import Data.Quantity
    ( Percentage (..), Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime )
import Fmt
    ( Buildable, build )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), diffRelTime )

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
    toText (SyncTolerance t) = toText t

instance FromText SyncTolerance where
    fromText = bimap (const errSyncTolerance) SyncTolerance . fromText
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
    :: (HasCallStack, Monad m)
    => SyncTolerance
        -- ^ A time tolerance inside which we consider ourselves synced
    -> TimeInterpreter m
        -- ^ Converts slots to actual time.
    -> BlockHeader
        -- ^ Local tip
    -> RelativeTime
        -- ^ Current Time
    -> m SyncProgress
syncProgress (SyncTolerance tolerance) ti tip now = do
    timeCovered <- ti $ slotToRelTime $ slotNo tip
    let progress
            | now == start = 0
            | otherwise = convert timeCovered % convert now

    if withinTolerance timeCovered now then
        return Ready
    else
        return
        . Syncing
        . Quantity
        . either (const . error $ errMsg progress) id
        . mkPercentage
        . toRational
        $ progress
  where
    start = RelativeTime 0

    convert :: RelativeTime -> Int
    convert = round . (* 1000) . getRelativeTime

    withinTolerance a b =  b `diffRelTime` a <= tolerance

    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"
