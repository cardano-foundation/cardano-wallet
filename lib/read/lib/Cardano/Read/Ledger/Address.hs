{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed address types.
-}
module Cardano.Read.Ledger.Address
    ( CompactAddrType
    , CompactAddr (..)
    , fromByronCompactAddr
    )
    where

import Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Control.Lens
    ( view
    )

import qualified Cardano.Chain.Common as BY
import qualified Cardano.Ledger.Address as SH

{-----------------------------------------------------------------------------
    Output
------------------------------------------------------------------------------}

type family CompactAddrType era where
    CompactAddrType Byron = BY.CompactAddress
    CompactAddrType Shelley = SH.CompactAddr StandardCrypto
    CompactAddrType Allegra = SH.CompactAddr StandardCrypto
    CompactAddrType Mary = SH.CompactAddr StandardCrypto
    CompactAddrType Alonzo = SH.CompactAddr StandardCrypto
    CompactAddrType Babbage = SH.CompactAddr StandardCrypto
    CompactAddrType Conway = SH.CompactAddr StandardCrypto

newtype CompactAddr era = CompactAddr (CompactAddrType era)

deriving instance Show (CompactAddrType era) => Show (CompactAddr era)
deriving instance Eq (CompactAddrType era) => Eq (CompactAddr era)

fromByronCompactAddr
    :: CompactAddrType Byron -> CompactAddrType Shelley
fromByronCompactAddr = SH.fromBoostrapCompactAddress
