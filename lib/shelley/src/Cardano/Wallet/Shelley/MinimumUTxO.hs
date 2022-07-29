{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Computing minimum UTxO values.
--
module Cardano.Wallet.Shelley.MinimumUTxO
    ( computeMinimumCoinForUTxO
    , maxLengthCoin
    , unsafeLovelaceToWalletCoin
    , unsafeValueToLovelace
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO (..), MinimumUTxOForShelleyBasedEra (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( toCardanoTxOut )
import Data.Function
    ( (&) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Word
    ( Word64 )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Api.Shelley as Cardano

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
computeMinimumCoinForUTxO
    :: HasCallStack
    => MinimumUTxO
    -> Address
    -> TokenMap
    -> Coin
computeMinimumCoinForUTxO = \case
    MinimumUTxONone ->
        \_addr _tokenMap -> Coin 0
    MinimumUTxOConstant c ->
        \_addr _tokenMap -> c
    MinimumUTxOForShelleyBasedEraOf minUTxO ->
        computeMinimumCoinForShelleyBasedEra minUTxO

-- | Computes a minimum 'Coin' value for a 'TokenMap' that is destined for
--   inclusion in a transaction output.
--
-- This function returns a value that is specific to a given Shelley-based era.
-- Importantly, a value that is valid in one era will not necessarily be valid
-- in another era.
--
computeMinimumCoinForShelleyBasedEra
    :: HasCallStack
    => MinimumUTxOForShelleyBasedEra
    -> Address
    -> TokenMap
    -> Coin
computeMinimumCoinForShelleyBasedEra
    (MinimumUTxOForShelleyBasedEra era pp) addr tokenMap =
        extractResult $
        Cardano.calculateMinimumUTxO era
            (embedTokenMapWithinPaddedTxOut era addr tokenMap)
            (Cardano.fromLedgerPParams era pp)
  where
    extractResult :: Either Cardano.MinimumUTxOError Cardano.Value -> Coin
    extractResult = \case
        Right value ->
            -- We assume that the returned value is a non-negative ada quantity
            -- with no other assets. If this assumption is violated, we have no
            -- way to continue, and must raise an error:
            value
                & unsafeValueToLovelace
                & unsafeLovelaceToWalletCoin
        Left e ->
            -- The 'Cardano.calculateMinimumUTxO' function should only return
            -- an error if a required protocol parameter is missing.
            --
            -- However, given that values of 'MinimumUTxOForShelleyBasedEra'
            -- can only be constructed by supplying an era-specific protocol
            -- parameters record, it should be impossible to trigger this
            -- condition.
            --
            -- Any violation of this assumption indicates a programming error.
            -- If this condition is triggered, we have no way to continue, and
            -- must raise an error:
            --
            error $ unwords
                [ "computeMinimumCoinForUTxO:"
                , "unexpected error:"
                , show e
                ]

-- | Embeds a 'TokenMap' within a padded 'Cardano.TxOut' value.
--
-- When computing the minimum UTxO quantity for a given 'TokenMap', we do not
-- have access to an address or to an ada quantity.
--
-- However, in order to compute a minimum UTxO quantity through the Cardano
-- API, we must supply a 'TxOut' value with a valid address and ada quantity.
--
-- It's imperative that we do not underestimate minimum UTxO quantities, as
-- this may result in the creation of transactions that are unacceptable to
-- the ledger. In the case of change generation, this would be particularly
-- problematic, as change outputs are generated automatically, and users do
-- not have direct control over the ada quantities generated.
--
-- However, while we cannot underestimate minimum UTxO quantities, we are at
-- liberty to moderately overestimate them.
--
-- Since the minimum UTxO quantity function is monotonically increasing w.r.t.
-- the size of the address and ada quantity, if we supply a 'TxOut' with an
-- address and ada quantity whose serialized lengths are the maximum possible
-- lengths, we can be confident that the resultant value will not be an
-- underestimate.
--
embedTokenMapWithinPaddedTxOut
    :: Cardano.ShelleyBasedEra era
    -> Address
    -> TokenMap
    -> Cardano.TxOut Cardano.CtxTx era
embedTokenMapWithinPaddedTxOut era addr m =
    toCardanoTxOut era $ TxOut addr $ TokenBundle maxLengthCoin m

-- | A 'Coin' value that is maximal in length when serialized to bytes.
--
-- When serialized to bytes, this 'Coin' value has a length that is greater
-- than or equal to the serialized length of any 'Coin' value that is valid
-- for inclusion in a transaction output.
--
maxLengthCoin :: Coin
maxLengthCoin = Coin $ intCast @Word64 @Natural $ maxBound

-- | Extracts a 'Coin' value from a 'Cardano.Lovelace' value.
--
-- Fails with a run-time error if the value is negative.
--
unsafeLovelaceToWalletCoin :: HasCallStack => Cardano.Lovelace -> Coin
unsafeLovelaceToWalletCoin (Cardano.Lovelace v) =
  case intCastMaybe @Integer @Natural v of
      Nothing -> error $ unwords
          [ "unsafeLovelaceToWalletCoin:"
          , "encountered negative value:"
          , show v
          ]
      Just lovelaceNonNegative ->
          Coin lovelaceNonNegative

-- | Extracts a 'Cardano.Lovelace' value from a 'Cardano.Value'.
--
-- Fails with a run-time error if the 'Cardano.Value' contains any non-ada
-- assets.
--
unsafeValueToLovelace :: HasCallStack => Cardano.Value -> Cardano.Lovelace
unsafeValueToLovelace v =
    case Cardano.valueToLovelace v of
        Nothing -> error $ unwords
            [ "unsafeValueToLovelace:"
            , "encountered value with non-ada assets:"
            , show v
            ]
        Just lovelace -> lovelace
