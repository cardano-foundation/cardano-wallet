{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.UI.Signal where

import Prelude

import Cardano.Wallet
    ( WalletWorkerLog (..)
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( WalletEngineLog (..)
    )
import Cardano.Wallet.Network.Logging
    ( ChainFollowLog (..)
    , ChainSyncLog (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types
    ( ChainPoint (..)
    , WalletId
    )
import Cardano.Wallet.Registry
    ( WorkerLog (..)
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )

data Signal
    = NewTip WalletId SlotNo
    deriving Show

signalsFromWorker :: WalletEngineLog -> [Signal]
signalsFromWorker = \case
    MsgWalletWorker m -> case m of
        MsgFromWorker wid m' -> case m' of
            MsgChainFollow m'' -> case m'' of
                MsgChainSync m''' -> case m''' of
                    MsgChainTip (ChainPoint slotNo _) -> [NewTip wid slotNo]
                    _ -> []
                _ -> []
            _ -> []
        _ -> []
    _ -> []

filterSignals :: Tracer IO Signal -> Tracer IO WalletEngineLog
filterSignals tr = Tracer $ \case
    m -> case signalsFromWorker m of
        [] -> pure ()
        ss -> mapM_ (traceWith tr) ss
