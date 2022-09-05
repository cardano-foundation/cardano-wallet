{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Tx.ExtraFields where

import Prelude

import Cardano.Api
    ( CardanoEra (..), InAnyCardanoEra (..) )
import Cardano.Ledger.Babbage.TxBody
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.SafeHash
    ( extractHash )
import Cardano.Ledger.Shelley.API
    ( Coin, KeyHash, KeyRole (..) )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval )
import Cardano.Wallet.Types.Read.Tx.Certificates
    ( Certs (..), getEraCerts )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( (:*-*) (..), UseTxAllEra, useTxCached, (*-*) )
import Cardano.Wallet.Types.Read.Tx.ExtraSigs
    ( ExtraSigs (..), getEraExtraSigs )
import Cardano.Wallet.Types.Read.Tx.Integrity
    ( Integrity (..), getEraIntegrity )
import Cardano.Wallet.Types.Read.Tx.Mint
    ( Mint (..), getEraMint )
import Cardano.Wallet.Types.Read.Tx.Validity
    ( Validity (..), getEraValidity )
import Data.Aeson.Lens
    ( pattern JSON )
import Data.Aeson.Types
    ( KeyValue ((.=)), ToJSON (toJSON), Value (..), object )
import Data.Maybe.Strict
    ( StrictMaybe )
import Data.Sequence.Strict
    ( StrictSeq )
import Data.Set
    ( Set )
import Data.Text
    ( Text )

import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Wallet.Types.Read.Tx as Read

type ExtraFields
    = Validity
    :*-* Integrity
    :*-* Certs
    :*-* Mint
    :*-* ExtraSigs

getExtraFields :: UseTxAllEra ExtraFields
getExtraFields =
    getEraValidity
    *-* getEraIntegrity
    *-* getEraCerts
    *-* getEraMint
    *-* getEraExtraSigs

extraFields :: Read.Tx -> InAnyCardanoEra ExtraFields
extraFields = useTxCached getExtraFields

newtype ApiExtraFields = ApiExtraFields (InAnyCardanoEra ExtraFields)

instance ToJSON ApiExtraFields where
    toJSON
        ( ApiExtraFields
            ( InAnyCardanoEra era
                (  Validity v
                    :*-* Integrity i
                    :*-* Certs cs
                    :*-* Mint m
                    :*-* ExtraSigs es
                )
            )
        ) =
        case era of
            ByronEra -> object
                    [ "era" .= ("byron" :: Text)
                    ]
            ShelleyEra -> object
                    [ "era" .= ("shelley" :: Text)
                    , "certs" .= renderCerts cs
                    ]
            AllegraEra -> object
                    [ "era" .= ("allegra" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderCoin m
                    ]
            MaryEra -> object
                    [ "era" .= ("mary" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    ]
            AlonzoEra -> object
                    [ "era" .= ("alonzo" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    , "integrity" .= renderIntegrity i
                    , "extra_sigs" .= renderExtraSigs es
                    ]
            BabbageEra -> object
                    [ "era" .= ("babbage" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    , "integrity" .= renderIntegrity i
                    , "extra_sigs" .= renderExtraSigs es
                    ]

renderExtraSigs :: Set (KeyHash 'Witness StandardCrypto) -> Value
renderExtraSigs = JSON

renderIntegrity :: StrictMaybe (ScriptIntegrityHash StandardCrypto) -> Value
renderIntegrity s = JSON $ extractHash <$> s

renderValidity :: ValidityInterval -> Value
renderValidity = error "not implemented"

renderMaryValue :: Mary.Value StandardCrypto -> Value
renderMaryValue = error "not implemented"

renderCoin :: Coin -> Value
renderCoin =  error "not implemented"

renderCerts :: StrictSeq (DCert StandardCrypto) -> Value
renderCerts = error "not implemented"
