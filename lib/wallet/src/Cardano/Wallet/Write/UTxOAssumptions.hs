{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Write.UTxOAssumptions
    (
    -- * UTxOAssumptions
      UTxOAssumptions (..)
    , assumedInputScriptTemplate
    , assumedTxWitnessTag

    -- * Validation
    , validateAddress
    )
    where

import Prelude

import Cardano.Ledger.Shelley.API
    ( Addr (..), Credential (..) )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..) )
import Cardano.Wallet.Write.Tx
    ( Address )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Wallet.Primitive.Types.Address as W

-- | Assumptions about the UTxO which are needed for coin-selection.
data UTxOAssumptions
    = AllKeyPaymentCredentials
    -- ^ Assumes all 'UTxO' entries have addresses with the post-Shelley
    -- key payment credentials.
    | AllByronKeyPaymentCredentials
    -- ^ Assumes all 'UTxO' entries have addresses with the boostrap/byron
    -- key payment credentials.
    | AllScriptPaymentCredentialsFrom
    -- ^ Assumes all 'UTxO' entries have addresses with script
    -- payment credentials, where the scripts are both derived
    -- from the 'ScriptTemplate' and can be looked up using the given function.
        !CA.ScriptTemplate
        !(W.Address -> CA.Script CA.KeyHash)

assumedInputScriptTemplate :: UTxOAssumptions -> Maybe CA.ScriptTemplate
assumedInputScriptTemplate = \case
    AllKeyPaymentCredentials -> Nothing
    AllByronKeyPaymentCredentials -> Nothing
    AllScriptPaymentCredentialsFrom scriptTemplate _ -> Just scriptTemplate

assumedTxWitnessTag :: UTxOAssumptions -> TxWitnessTag
assumedTxWitnessTag = \case
    AllKeyPaymentCredentials -> TxWitnessShelleyUTxO
    AllByronKeyPaymentCredentials -> TxWitnessByronUTxO
    AllScriptPaymentCredentialsFrom {} -> TxWitnessShelleyUTxO

validateAddress :: UTxOAssumptions -> Address -> Bool
validateAddress = valid
  where
    valid AllKeyPaymentCredentials          (Addr _ KeyHashObj{}    _) = True
    valid AllScriptPaymentCredentialsFrom{} (Addr _ ScriptHashObj{} _) = True
    valid AllByronKeyPaymentCredentials     (AddrBootstrap _)          = True
    valid _                                 _                          = False
