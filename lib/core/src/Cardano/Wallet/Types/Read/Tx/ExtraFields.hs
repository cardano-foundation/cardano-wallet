{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Types.Read.Tx.ExtraFields where

import Prelude

import Cardano.Ledger.Babbage.TxBody
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.SafeHash
    ( extractHash )
import Cardano.Ledger.Shelley.API
    ( Coin
    , Delegation (..)
    , GenesisDelegCert (..)
    , KeyHash
    , KeyRole (..)
    , MIRCert (..)
    , MIRPot (..)
    , MIRTarget (..)
    )
import Cardano.Ledger.Shelley.TxBody
    ( DCert (..), DelegCert (..), PoolCert (..) )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( ValidityInterval (..) )
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
import Data.Aeson.Lens
    ( pattern JSON )
import Data.Aeson.Types
    ( Key, KeyValue ((.=)), ToJSON (toJSON), Value (..), object )
import Data.Foldable
    ( toList )
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
renderExtraSigs = JSON

renderIntegrity :: StrictMaybe (ScriptIntegrityHash StandardCrypto) -> Value
renderIntegrity s = JSON $ extractHash <$> s

renderValidity :: ValidityInterval -> Value
renderValidity ValidityInterval {..} = object
    [ "invalid_before" .= invalidBefore
    , "invalid_after" .= invalidHereafter
    ]

renderMaryValue :: Mary.Value StandardCrypto -> Value
renderMaryValue (Mary.Value lovelace assets) = object
    [ "lovelace" .= lovelace
    , "assets"  .= assets
    ]

renderCoin :: Coin -> Value
renderCoin lovelace =  object
    [ "lovelace" .= lovelace
    ]

renderCerts :: StrictSeq (DCert StandardCrypto) -> Value
renderCerts s = JSON @[Value] $ do
    x <- toList s
    case x of
      DCertDeleg dc -> pure
        $ certType "delegation"
        $ case dc of
            RegKey cre -> certType "stake key registration" cre
            DeRegKey cre -> certType "stake key deregistration" cre
            Delegate Delegation {..} ->
                certType "stake delegation"
                $ object
                [ "delegator" .= _delegator
                , "delegatee" .= _delegatee
                ]
      DCertPool pc -> pure
        $ certType "pool"
        $ case pc of
            RegPool pp -> certType "stake pool registration" pp
            RetirePool kh en -> certType "stake pool retirement"
                $ object
                [ "pool_hash" .= kh
                , "epoch_no" .= en
                ]
      DCertGenesis gdc -> pure
        $ certType "genesis"
        $ case gdc of
            GenesisDelegCert kh kh' ha -> object
                [ "genesis" .= kh
                , "delegate" .= kh'
                , "verify"  .= ha
                ]
      DCertMir mc -> pure
        $ certType "MIR"
        $ case mc of
            MIRCert mp mt -> object
                [ "pot" .= case mp of
                    ReservesMIR -> "reverse" :: Text
                    TreasuryMIR -> "treasury"
                , "rewards" .= renderMIRTarget mt
                ]

renderMIRTarget :: MIRTarget StandardCrypto -> Value
renderMIRTarget mt = case mt of
  StakeAddressesMIR mp -> objectType "stake address" "staking values" mp
  SendToOppositePotMIR co -> objectType "sent to opposite pot" "lovelace" co

certType :: ToJSON v => Text -> v -> Value
certType t = objectType t "certificate"

objectType :: ToJSON v => Text -> Key -> v -> Value
objectType t k x = object
    [ "type" .= t
    , k .= x
    ]
