-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Primitives for minting and burning tokens.

module Cardano.Wallet.Primitive.MintBurn
    ( -- * Types
      TxMintBurn(..)
    , ToMint
    , ToBurn
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash, Script )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Data.List.NonEmpty
    ( NonEmpty )

-- TokenMap can only hold positive values, so we create wrapper types to
-- represent positive values (ToMint) and negative values (ToBurn).

-- | Tokens to mint.
type ToMint = TokenMap

-- | Tokens to burn.
type ToBurn = TokenMap

-- | Representation of the Cardano.API data for minting and burning. This is the
-- data necessary to submit to the Cardano.API backend.
data TxMintBurn = TxMintBurn
    { toMint  :: ToMint
    , toBurn  :: ToBurn
    , scripts :: NonEmpty (Script KeyHash)
    }
    deriving (Eq, Show)
