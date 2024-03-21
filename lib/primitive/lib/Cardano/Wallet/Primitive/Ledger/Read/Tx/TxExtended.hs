{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2024 Cardanofoundation
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.TxExtended
    ( fromCardanoTx
    , getTxExtended
    )
where

import Prelude

import Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( getCertificates
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint
    ( mint
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity
    ( getValidity
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.WitnessCount
    ( getWitnessCount
    )
import Cardano.Wallet.Primitive.Types.Tx.TxExtended
    ( TxExtended (..)
    )
import Cardano.Wallet.Read
    ( Allegra
    , Alonzo
    , Babbage
    , Conway
    , IsEra
    , Mary
    , Shelley
    , Tx (Tx)
    , (:*:) ((:*:))
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates
    )
import Cardano.Wallet.Read.Tx.Mint
    ( getEraMint
    )
import Cardano.Wallet.Read.Tx.ReferenceInputs
    ( getEraReferenceInputs
    )
import Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity
    )
import Cardano.Wallet.Read.Tx.Witnesses
    ( getEraWitnesses
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano

fromCardanoTx
    :: Cardano.Tx cera
    -> TxExtended
fromCardanoTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley -> getTxExtended @Shelley $ Tx tx
        Cardano.ShelleyBasedEraAllegra -> getTxExtended @Allegra $ Tx tx
        Cardano.ShelleyBasedEraMary -> getTxExtended @Mary $ Tx tx
        Cardano.ShelleyBasedEraAlonzo -> getTxExtended @Alonzo $ Tx tx
        Cardano.ShelleyBasedEraBabbage -> getTxExtended @Babbage $ Tx tx
        Cardano.ShelleyBasedEraConway -> getTxExtended @Conway $ Tx tx

getTxExtended :: forall era. IsEra era => Tx era -> TxExtended
getTxExtended tx =
    TxExtended
        { walletTx = primitiveTx tx
        , certificates = getCertificates $ getEraCertificates tx
        , toMint = assetsToMint
        , toBurn = assetsToBurn
        , validity = getValidity $ getEraValidity tx
        , witnessCount = getWitnessCount tx
        }
  where
    (assetsToMint, assetsToBurn) =
        mint
            $ getEraMint tx
                :*: getEraWitnesses tx
                :*: getEraReferenceInputs tx
