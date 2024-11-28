{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Real implementation of a 'NetworkEnv'.
module Cardano.Wallet.Deposit.IO.Network.NodeToClient
    ( fromNetworkLayer
    , NetworkLayer
    , CardanoBlock
    , StandardCrypto
    ) where

import Prelude

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( ErrPostTx (..)
    , NetworkEnv (..)
    , mapBlock
    )
import Cardano.Wallet.Deposit.Time
    ( toTimeTranslation
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    , mapChainFollower
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( CardanoBlock
    )
import Cardano.Wallet.Primitive.Slotting
    ( snapshot
    )
import Cardano.Wallet.Read
    ( chainPointFromChainTip
    )
import Control.Monad.Trans.Except
    ( runExceptT
    , withExceptT
    )
import Control.Tracer
    ( nullTracer
    )

import qualified Cardano.Read.Ledger.Block.Block as Read
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Time as Time
import qualified Cardano.Wallet.Network as NetworkLayer

{-----------------------------------------------------------------------------
    NodeToClient 'NetworkEnv'
------------------------------------------------------------------------------}

-- | Translate the old NetworkLayer to the new NetworkEnv interface
fromNetworkLayer
    :: NetworkLayer.NetworkLayer IO Read.ConsensusBlock
    -> NetworkEnv IO (Read.EraValue Read.Block)
fromNetworkLayer nl = mapBlock Read.fromConsensusBlock $
    NetworkEnv
        { chainSync = \_tr follower -> do
            -- TODO: Connect tracer
            let follower' = mapChainFollower id id chainPointFromChainTip id follower
            NetworkLayer.chainSync nl nullTracer follower'
            return $ error "impossible: chainSync returned"
            -- TODO: We can change the error type of 'NetworkLayer.postTx' it
            -- doesn't need the ErrPostTxEraUnsupported case
        , postTx = runExceptT . withExceptT translateErrPostTx . NetworkLayer.postTx nl
        , currentPParams =
            NetworkLayer.currentPParams nl
        , getTimeTranslation = toTimeTranslation (NetworkLayer.timeInterpreter nl)
        , slotToUTCTime = Time.slotToUTCTime <$> snapshot ti
        }

  where
    ti = NetworkLayer.timeInterpreter nl

    translateErrPostTx :: NetworkLayer.ErrPostTx -> ErrPostTx
    translateErrPostTx = \case
        NetworkLayer.ErrPostTxValidationError errorText -> ErrPostTxValidationError errorText
        NetworkLayer.ErrPostTxMempoolFull -> ErrPostTxMempoolFull
        NetworkLayer.ErrPostTxEraUnsupported _era ->
            error "translateErrPostTx: ErrPostTxEraUnsupported should be impossible"
