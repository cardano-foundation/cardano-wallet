{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Mock implementation of a 'NetworkEnv'.
module Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    ) where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    )
import Cardano.Wallet.Network
    ( ChainFollower (..)
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

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Time as Time
import qualified Cardano.Wallet.Deposit.Write as Write

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
            , currentPParams =
                pure $ Read.EraValue Read.mockPParamsConway
            , getTimeTranslation =
                pure $ Time.toTimeTranslationPure Time.mockTimeInterpreter
            , slotToUTCTime = pure Time.unsafeUTCTimeOfSlot
            }
