{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'Addr' — Addresses on the Cardano Blockchain
-}
module Cardano.Wallet.Read.Address
    ( -- * Compact Addr
      CompactAddr
    , toShortByteString
    , fromShortByteString
    , isBootstrapCompactAddr

    -- * Internal
    , fromEraCompactAddr
    ) where

import Prelude

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Read.Ledger.Address
    ( translateCompactAddrShelleyFromByron
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import Control.Monad.Trans.State.Strict
    ( evalStateT
    )

import qualified Cardano.Ledger.Address as SH
import qualified Cardano.Read.Ledger.Address as L
import qualified Data.ByteString.Short as SBS

{-----------------------------------------------------------------------------
    CompactAddr
------------------------------------------------------------------------------}
type CompactAddr = SH.CompactAddr StandardCrypto

toShortByteString :: CompactAddr -> SBS.ShortByteString
toShortByteString = SH.unCompactAddr

fromShortByteString :: SBS.ShortByteString -> Maybe CompactAddr
fromShortByteString sbs =
    SH.compactAddr
        <$> evalStateT (SH.decodeAddrStateLenientT True True sbs) 0

-- | Efficient check whether this is a Bootstrap address
-- (i.e. an address that was valid in the Byron era).
isBootstrapCompactAddr :: CompactAddr -> Bool
isBootstrapCompactAddr = SH.isBootstrapCompactAddr

{-# INLINEABLE fromEraCompactAddr #-}
fromEraCompactAddr
    :: forall era. IsEra era
    => L.CompactAddr era -> CompactAddr
fromEraCompactAddr = case theEra :: Era era of
    Byron -> onAddress translateCompactAddrShelleyFromByron
    Shelley -> onAddress id
    Allegra -> onAddress id
    Mary -> onAddress id
    Alonzo -> onAddress id
    Babbage -> onAddress id
    Conway -> onAddress id

-- Helper function for type inference.
onAddress :: (L.CompactAddrType era -> t) -> L.CompactAddr era -> t
onAddress f (L.CompactAddr x) = f x
