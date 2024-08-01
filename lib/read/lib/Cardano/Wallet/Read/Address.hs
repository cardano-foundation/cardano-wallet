{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    ) where

import Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Control.Monad.Trans.State.Strict
    ( evalStateT
    )

import qualified Cardano.Ledger.Address as SH
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
