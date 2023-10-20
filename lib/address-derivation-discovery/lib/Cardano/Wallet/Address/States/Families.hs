{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Address.States.Families where

import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState
    )
import Cardano.Wallet.Address.States.Test.State
    ( TestState
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant
    )
import Data.Kind
    ( Type
    )

type family CredFromOf s :: Depth
type instance CredFromOf (SharedState n key) = 'CredFromScriptK
type instance CredFromOf (SeqState n key) = 'CredFromKeyK
type instance CredFromOf (RndState n) = 'CredFromKeyK
type instance CredFromOf (TestState s n key kt) = kt

-- | A type family to get the key type from a state.
type family KeyOf (s :: Type) :: (Depth -> Type -> Type)
type instance KeyOf (SeqState n k) = k
type instance KeyOf (RndState n) = ByronKey
type instance KeyOf (SharedState n k) = k
type instance KeyOf (TestState s n k kt) = k

type family NetworkOf (s :: Type) :: NetworkDiscriminant
type instance NetworkOf (SeqState n k) = n
type instance NetworkOf (RndState n) = n
type instance NetworkOf (SharedState n k) = n
type instance NetworkOf (TestState s n k kt) = n
