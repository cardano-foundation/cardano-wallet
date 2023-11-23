{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Primitive.Types.WitnessCount
    ( WitnessCount (..)
    , WitnessCountCtx (..)
    , emptyWitnessCount
    , toKeyRole
    )
where

import Prelude

import Cardano.Address.Script
    ( KeyHash (KeyHash)
    , KeyRole (..)
    )
import Cardano.Wallet.Primitive.Types.AnyExplicitScripts
    ( AnyExplicitScript
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (Hash)
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Word
    ( Word8
    )
import GHC.Generics
    ( Generic
    )

data WitnessCount = WitnessCount
    { verificationKey :: Word8
    , scripts :: [AnyExplicitScript]
    , bootstrap :: Word8
    }
    deriving (Eq, Generic, Show)
    deriving anyclass (NFData)

emptyWitnessCount :: WitnessCount
emptyWitnessCount =
    WitnessCount
        { verificationKey = 0
        , scripts = []
        , bootstrap = 0
        }

-- WitnessCount context is needed to differentiate verification keys present
-- in native scripts.
-- In shelley wallets they could be present due to only policy verification key.
-- In multisig wallet they could stem from payment, policy and delegation roles,
-- and as minting/burning and delegation support comes will be extended in additional
-- data attached in SharedWalletCtx to differentiate that.
-- WitnessCount is needed only during or after signing, in other phases it is not used.
data WitnessCountCtx =
      ShelleyWalletCtx KeyHash -- Policy
    | SharedWalletCtx [KeyHash] -- Delegation key hashes of all cosigners
    | AnyWitnessCountCtx
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

toKeyRole :: WitnessCountCtx -> Hash "VerificationKey" -> KeyRole
toKeyRole witCtx (Hash key) = case witCtx of
    ShelleyWalletCtx (KeyHash _ mypolicykey) ->
        if key == mypolicykey then
            Policy
        else
            Unknown
    SharedWalletCtx stakingKeyHashes ->
        let toStakeKey (KeyHash _ k) = k
            isStakeKey = elem key (map toStakeKey stakingKeyHashes)
        in if isStakeKey then
               Delegation
           else
               Payment
    AnyWitnessCountCtx -> Unknown
