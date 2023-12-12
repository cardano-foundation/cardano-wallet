{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module containing 'UTxOAssumptions' and related functionality.
module Internal.Cardano.Write.UTxOAssumptions
    (
    -- * UTxOAssumptions
      UTxOAssumptions (..)
    , assumedInputScriptTemplate
    )
    where

import Prelude

import Internal.Cardano.Write.Tx
    ( Address
    )

import qualified Cardano.Address.Script as CA

-- | Assumptions about UTxOs that are needed for coin selection.
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
        !(Address -> CA.Script CA.KeyHash)

assumedInputScriptTemplate :: UTxOAssumptions -> Maybe CA.ScriptTemplate
assumedInputScriptTemplate = \case
    AllKeyPaymentCredentials -> Nothing
    AllByronKeyPaymentCredentials -> Nothing
    AllScriptPaymentCredentialsFrom scriptTemplate _ -> Just scriptTemplate
