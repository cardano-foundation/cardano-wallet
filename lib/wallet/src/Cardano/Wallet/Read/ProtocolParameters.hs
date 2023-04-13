{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Provides protocol parameters.
--
module Cardano.Wallet.Read.ProtocolParameters
    ( ProtocolParameters (..)
    , unsafeFromWalletProtocolParameters
    ) where

import Prelude

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Wallet.Primitive.Types as Wallet

data ProtocolParameters era = ProtocolParameters
    { pparamsWallet
        :: !Wallet.ProtocolParameters
    , pparamsNode
        :: !(CardanoApi.BundledProtocolParameters era)
    }

-- TODO: ADP-2459 - replace with something nicer.
unsafeFromWalletProtocolParameters
    :: forall era. CardanoApi.IsShelleyBasedEra era
    => Wallet.ProtocolParameters
    -> ProtocolParameters era
unsafeFromWalletProtocolParameters pp = ProtocolParameters
    { pparamsWallet = pp
    , pparamsNode = maybe
        (error $ unwords
            [ "unsafeFromWalletProtocolParameters: no nodePParams."
            , "This should only be possible in Byron, where IsShelleyBasedEra"
            , "should prevent this from being reached."
            ])
        (CardanoApi.bundleProtocolParams (CardanoApi.cardanoEra @era))
        (Wallet.currentNodeProtocolParameters pp)
    }
