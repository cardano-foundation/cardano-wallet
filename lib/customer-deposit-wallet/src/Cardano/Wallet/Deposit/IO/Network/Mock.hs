{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Mock implementation of a 'NetworkEnv'.
module Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    , unsafeUTCTimeOfSlot
    , unsafeSlotsToUTCTimes
    , unsafeSlotOfUTCTime
    , originTime
    , shelleyTime
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Network
    ( ChainFollower (..)
    )
import Cardano.Wallet.Read
    ( Slot
    , SlotNo (..)
    , WithOrigin (..)
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    , atomically
    , modifyTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Monad
    ( forever
    )
import Control.Monad.Class.MonadTime
    ( UTCTime
    )
import Control.Monad.Class.MonadTimer
    ( MonadDelay
    , threadDelay
    )
import Data.Foldable
    ( for_
    )
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( maybeToList
    )
import Data.Set
    ( Set
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Mock implementation of 'NetworkEnv'
------------------------------------------------------------------------------}
newNetworkEnvMock
    :: (MonadDelay m, MonadSTM m)
    => m (NetworkEnv m (Read.Block Read.Conway))
newNetworkEnvMock = do
    mchain <- newTVarIO []
    mtip <- newTVarIO Read.GenesisPoint
    mfollowers <- newTVarIO []

    let registerAndUpdate follower = do
            _ <- rollBackward follower Read.GenesisPoint
            (chain, tip) <- atomically $ do
                modifyTVar mfollowers (follower :)
                (,) <$> readTVar mchain <*> readTVar mtip
            case reverse chain of
                [] -> pure ()
                (b : bs) -> rollForward follower (b :| bs) tip

    let forgeBlock tx = atomically $ do
            tipOld <- readTVar mtip
            let txRead = Write.toConwayTx tx
                blockNew = Read.mockNextBlock tipOld [txRead]
                tipNew = Read.getChainPoint blockNew
            writeTVar mtip tipNew
            modifyTVar mchain (blockNew :)
            pure (blockNew, tipNew)

    let broadcast block tip = do
            followers <- readTVarIO mfollowers
            for_ followers $ \follower ->
                rollForward follower (block :| []) tip

    pure
        NetworkEnv
            { chainSync = \_ follower -> do
                registerAndUpdate follower
                forever $ threadDelay 1000000
            , postTx = \tx -> do
                (block, tip) <- forgeBlock tx
                broadcast block tip
                -- brief delay to account for asynchronous chain followers
                threadDelay 100
                pure $ Right ()
            , slotsToUTCTimes = pure . unsafeSlotsToUTCTimes
            , utcTimeToSlot = pure . Just . unsafeSlotOfUTCTime
            }

unsafeSlotsToUTCTimes :: Set Slot -> Map Slot (WithOrigin UTCTime)
unsafeSlotsToUTCTimes slots =
    Map.fromList $ do
        slot <- Set.toList slots
        time <- maybeToList $ unsafeUTCTimeOfSlot slot
        pure (slot, time)

unsafeUTCTimeOfSlot :: Slot -> Maybe (WithOrigin UTCTime)
unsafeUTCTimeOfSlot Origin = Just Origin
unsafeUTCTimeOfSlot (At (SlotNo n)) =
    Just . At
        $ posixSecondsToUTCTime
        $ fromIntegral pt
  where
    pts = fromIntegral n - byronSlots
    pt =
        if pts >= 0
            then shelleyTime + pts
            else shelleyTime + pts * 20

unsafeSlotOfUTCTime :: UTCTime -> Read.Slot
unsafeSlotOfUTCTime t
    | origin = Origin
    | byron = At $ SlotNo $ fromIntegral $ (pt - originTime) `div` 20
    | otherwise = At $ SlotNo $ fromIntegral $ pt - shelleyTime + byronSlots
  where
    pt = floor $ utcTimeToPOSIXSeconds t
    origin = pt < originTime
    byron = pt < shelleyTime

byronSlots :: Integer
byronSlots = 4924800

shelleyTime :: Integer
shelleyTime = 1596491091

originTime :: Integer
originTime = shelleyTime - byronSlots * 20
