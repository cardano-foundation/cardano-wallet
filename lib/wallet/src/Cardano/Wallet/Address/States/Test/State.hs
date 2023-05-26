{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Wallet.Address.States.Test.State where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant )
import Data.Kind
    ( Type )
import GHC.Generics
    ( Generic )

newtype
    TestState
        s
        (n :: NetworkDiscriminant)
        (k :: Depth -> Type -> Type)
        (ktype :: Depth)
    = TestState s
    deriving (Generic, Show, Eq)
