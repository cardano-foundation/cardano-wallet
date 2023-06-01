{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Module containing 'UTxOAssumptions' and related functionality.
module Cardano.Wallet.Write.UTxOAssumptions
    (
    -- * UTxOAssumptions
      UTxOAssumptions (..)
    , assumedInputScriptTemplate
    , assumedTxWitnessTag

    -- * Validation
    , validateAddress
    , validateAddresses
    , UTxOAssumptionViolation (..)
    )
    where

import Prelude

import Cardano.Ledger.Shelley.API
    ( Addr (..), Credential (..) )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..) )
import Cardano.Wallet.Write.Tx
    ( Address )
import Control.Monad
    ( forM_ )
import Data.Text
    ( Text )
import Fmt
    ( Buildable, build )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Wallet.Primitive.Types.Address as W

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

--
-- Validation
--

data UTxOAssumptionViolation
    = UTxOAssumptionViolation
        Address  -- Address violating assumptions
        Text -- ^ Textual description of assumptions
    deriving (Eq, Show)

instance Buildable UTxOAssumptionViolation where
    build (UTxOAssumptionViolation addr assumptions)
        = mconcat
            [ "UTxOAssumption "
            , build assumptions
            , " broken by "
            , build $ show addr
            ]

validateAddresses
    :: UTxOAssumptions
    -> [Address]
    -> Either UTxOAssumptionViolation ()
validateAddresses assumptions = case assumptions of
    AllKeyPaymentCredentials
        -> validateAll "AllKeyPaymentCredentials"
    AllByronKeyPaymentCredentials
        -> validateAll "AllByronKeyPaymentCredentials"
    AllScriptPaymentCredentialsFrom{}
        -> validateAll "AllScriptPaymentCredentials"
        -- NOTE: The script lookup could provide bigger scripts than predicted
        -- by the template. This is not currently validated.
  where
    validateAll assumptionText addrs = forM_ addrs $ \addr ->
        if validateAddress assumptions addr
        then Right ()
        else Left $ UTxOAssumptionViolation addr assumptionText

validateAddress :: UTxOAssumptions -> Address -> Bool
validateAddress = valid
  where
    valid AllKeyPaymentCredentials          (Addr _ KeyHashObj{}    _) = True
    valid AllScriptPaymentCredentialsFrom{} (Addr _ ScriptHashObj{} _) = True
    valid AllByronKeyPaymentCredentials     (AddrBootstrap _)          = True
    valid _                                 _                          = False
