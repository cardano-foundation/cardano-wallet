{-# LANGUAGE NamedFieldPuns #-}
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

-- TODO: Make this data type abstract: don't export the constructor.
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
unsafeFromWalletProtocolParameters pparamsWallet =
    ProtocolParameters {pparamsWallet, pparamsNode}
  where
    pparamsNode = maybe
        (error missingNodeParamsError)
        (CardanoApi.bundleProtocolParams (CardanoApi.cardanoEra @era))
        (Wallet.currentNodeProtocolParameters pparamsWallet)
    missingNodeParamsError = unwords
        [ "unsafeFromWalletProtocolParameters: no nodePParams."
        , "This should only be possible in Byron, where IsShelleyBasedEra"
        , "should prevent this from being reached."
        ]
