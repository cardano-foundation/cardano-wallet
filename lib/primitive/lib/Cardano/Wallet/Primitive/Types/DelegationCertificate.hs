{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Primitive.Types.DelegationCertificate
    ( DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount
    )
import Control.DeepSeq
    ( NFData
    )
import GHC.Generics
    ( Generic
    )

data DelegationCertificate
    = CertDelegateNone RewardAccount
    | CertDelegateFull RewardAccount PoolId
    | CertRegisterKey RewardAccount
    deriving (Generic, Show, Eq, Ord)

instance NFData DelegationCertificate

dlgCertAccount :: DelegationCertificate -> RewardAccount
dlgCertAccount = \case
    CertDelegateNone acc -> acc
    CertDelegateFull acc _ -> acc
    CertRegisterKey acc -> acc

dlgCertPoolId :: DelegationCertificate -> Maybe PoolId
dlgCertPoolId = \case
    CertDelegateNone{} -> Nothing
    CertDelegateFull _ poolId -> Just poolId
    CertRegisterKey _ -> Nothing
