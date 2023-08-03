{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Address.States.Families where

import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.RandomAny
    ( RndAnyState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.SequentialAny
    ( SeqAnyState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState )
import Cardano.Wallet.Address.States.Test.State
    ( TestState )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant )
import Data.Kind
    ( Type )

type family CredFromOf s where
    CredFromOf (SharedState n key) = 'CredFromScriptK
    CredFromOf (SeqState n key) = 'CredFromKeyK
    CredFromOf (RndState n) = 'CredFromKeyK
    CredFromOf (TestState s n key kt) = kt
    CredFromOf (RndAnyState n p) = 'CredFromKeyK
    CredFromOf (SeqAnyState n key p) = 'CredFromKeyK

-- | A type family to get the key type from a state.
type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s n k kt) = k

type family NetworkOf (s :: Type) :: NetworkDiscriminant where
    NetworkOf (SeqState n k) = n
    NetworkOf (RndState n) = n
    NetworkOf (SharedState n k) = n
    NetworkOf (SeqAnyState n k p) = n
    NetworkOf (RndAnyState n p) = n
    NetworkOf (TestState s n k kt) = n
