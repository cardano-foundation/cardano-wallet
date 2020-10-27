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
    , )
    where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, startTime )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotNo (..) )
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
    ( NominalDiffTime, UTCTime, diffUTCTime )
import Fmt
    ( Buildable, build )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

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
        -- ^ Parameters for slot arithmetic which are assumed to be static.
        --
        -- NOTE: This is no longer the case with the Hard Fork Combinator and
        -- Byron-to-Shelley era transition.
    -> BlockHeader
        -- ^ Local tip
    -> UTCTime
        -- ^ Current Time
    -> m SyncProgress
syncProgress (SyncTolerance tolerance) ti tip now = do
    tipTime <- ti $ startTime $ slotNo tip
    let timeRemaining = now `diffUTCTime` tipTime
    genesisDate <- ti $ startTime $ SlotNo 0
    let timeCovered = tipTime `diffUTCTime` genesisDate

    -- Using (max 1) to avoid division by 0.
    let progress = (convert timeCovered)
            % max 1 (convert $ timeCovered + timeRemaining)
    if timeRemaining < tolerance || timeRemaining < 0 || progress >= 1 then
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
    convert :: NominalDiffTime -> Integer
    convert = round

    errMsg x = "syncProgress: " ++ show x ++ " is out of bounds"
