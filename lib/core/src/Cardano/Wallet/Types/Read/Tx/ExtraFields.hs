{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Types.Read.Tx.ExtraFields where

import Prelude

import Cardano.Ledger.Babbage.TxBody
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.API
    ( Coin, KeyHash, KeyRole (..) )
import Cardano.Ledger.Shelley.TxBody
    ( DCert )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval )
import Cardano.Wallet.Types.Read.Eras
    ( (:*:) (..), EraValue, EraValueS (..), applyEraFun, toEraValueS, (*-*) )
import Cardano.Wallet.Types.Read.Tx
    ( Tx )
import Cardano.Wallet.Types.Read.Tx.Certificates
    ( Certificates (..), getEraCertificates )
import Cardano.Wallet.Types.Read.Tx.ExtraSigs
    ( ExtraSigs (..), getEraExtraSigs )
import Cardano.Wallet.Types.Read.Tx.Integrity
    ( Integrity (..), getEraIntegrity )
import Cardano.Wallet.Types.Read.Tx.Mint
    ( Mint (..), getEraMint )
import Cardano.Wallet.Types.Read.Tx.Validity
    ( Validity (..), getEraValidity )
import Data.Aeson.Types
    ( KeyValue ((.=)), ToJSON (toJSON), Value, object )
import Data.Maybe.Strict
    ( StrictMaybe )
import Data.Sequence.Strict
    ( StrictSeq )
import Data.Set
    ( Set )
import Data.Text
    ( Text )

import qualified Cardano.Ledger.Mary.Value as Mary

type ExtraFields
    = Validity :*: Integrity :*: Certificates :*: Mint :*: ExtraSigs

newtype ApiExtraFields = ApiExtraFields (EraValue ExtraFields)

extraFields
     :: EraValue Tx
     -> EraValue ExtraFields
extraFields
    = applyEraFun
        $  getEraValidity
        *-* getEraIntegrity
        *-* getEraCertificates
        *-* getEraMint
        *-* getEraExtraSigs

instance ToJSON ApiExtraFields where
    toJSON ( ApiExtraFields x) = case toEraValueS x of
        ByronValue _
                -> object
                    [ "era" .= ("byron" :: Text)
                    ]
        ShelleyValue
            ( _ :*: _ :*: Certificates cs :*: _  :*:  _)
            -> object
                    [ "era" .= ("shelley" :: Text)
                    , "certs" .= renderCerts cs
                    ]
        AllegraValue
            ( _ :*: _ :*: Certificates cs :*: Mint m :*:  _)
            -> object
                    [ "era" .= ("allegra" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderCoin m
                    ]
        MaryValue
            ( Validity v :*: _ :*: Certificates cs :*: Mint m :*:  _)
            -> object
                    [ "era" .= ("mary" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    ]
        AlonzoValue
            ( Validity v :*: Integrity i :*: Certificates cs :*: Mint m
                :*:  ExtraSigs es)
            -> object
                    [ "era" .= ("alonzo" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    , "integrity" .= renderIntegrity i
                    , "extra_sigs" .= renderExtraSigs es
                    ]
        BabbageValue
            ( Validity v :*: Integrity i :*: Certificates cs :*: Mint m
                :*:  ExtraSigs es)
            -> object
                    [ "era" .= ("babbage" :: Text)
                    , "certs" .= renderCerts cs
                    , "mint" .= renderMaryValue m
                    , "validity" .= renderValidity v
                    , "integrity" .= renderIntegrity i
                    , "extra_sigs" .= renderExtraSigs es
                    ]

renderExtraSigs :: Set (KeyHash 'Witness StandardCrypto) -> Value
renderExtraSigs = error "not implemented"

renderIntegrity :: StrictMaybe (ScriptIntegrityHash StandardCrypto) -> Value
renderIntegrity = error "not implemented"

renderValidity :: ValidityInterval -> Value
renderValidity = error "not implemented"

renderMaryValue :: Mary.Value StandardCrypto -> Value
renderMaryValue = error "not implemented"

renderCoin :: Coin -> Value
renderCoin =  error "not implemented"

renderCerts :: StrictSeq (DCert StandardCrypto) -> Value
renderCerts = error "not implemented"
