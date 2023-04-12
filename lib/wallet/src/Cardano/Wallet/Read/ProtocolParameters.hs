-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Provides protocol parameters.
--
module Cardano.Wallet.Read.ProtocolParameters
    ( ProtocolParameters (..)
    ) where

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Wallet.Primitive.Types as Wallet

data ProtocolParameters era = ProtocolParameters
    { pparamsWallet
        :: !Wallet.ProtocolParameters
    , pparamsNode
        :: !(CardanoApi.BundledProtocolParameters era)
    }
