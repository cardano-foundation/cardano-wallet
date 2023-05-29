{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Primitive.Types.Credentials
    ( RootCredentials (..)
    , HashedCredentials
    , ClearCredentials
    ) where

import Prelude


import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK) )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase, PassphraseHash )

-- | A 'PrivateKey' for a given 'KeyFlavor'.
data RootCredentials k pw = Credentials
         { credentialsKey :: k 'RootK XPrv
         , credentialsPassword :: pw
         }

deriving instance (Eq (k 'RootK XPrv), Eq pw) => Eq (RootCredentials k pw)
deriving instance (Show (k 'RootK XPrv), Show pw) => Show (RootCredentials k pw)

type HashedCredentials k = RootCredentials k PassphraseHash

type ClearCredentials k = RootCredentials k (Passphrase "encryption")
