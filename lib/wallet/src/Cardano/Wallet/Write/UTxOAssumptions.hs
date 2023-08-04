{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module containing 'UTxOAssumptions' and related functionality.
module Cardano.Wallet.Write.UTxOAssumptions
  ( -- * UTxOAssumptions
    UTxOAssumptions (..)
  , assumedInputScriptTemplate
  , assumedTxWitnessTag

    -- * Validation
  , validateAddress
  )
where

import Cardano.Address.Script qualified as CA
import Cardano.Ledger.Shelley.API
  ( Addr (..)
  , Credential (..)
  )
import Cardano.Wallet.Primitive.Types.Address qualified as W
import Cardano.Wallet.TxWitnessTag
  ( TxWitnessTag (..)
  )
import Cardano.Wallet.Write.Tx
  ( Address
  )
import Prelude

-- | Assumptions about UTxOs that are needed for coin selection.
data UTxOAssumptions
  = -- | Assumes all 'UTxO' entries have addresses with the post-Shelley
    -- key payment credentials.
    AllKeyPaymentCredentials
  | -- | Assumes all 'UTxO' entries have addresses with the boostrap/byron
    -- key payment credentials.
    AllByronKeyPaymentCredentials
  | -- | Assumes all 'UTxO' entries have addresses with script
    -- payment credentials, where the scripts are both derived
    -- from the 'ScriptTemplate' and can be looked up using the given function.
    AllScriptPaymentCredentialsFrom
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
    valid AllKeyPaymentCredentials (Addr _ KeyHashObj {} _) = True
    valid AllScriptPaymentCredentialsFrom {} (Addr _ ScriptHashObj {} _) = True
    valid AllByronKeyPaymentCredentials (AddrBootstrap _) = True
    valid _ _ = False
