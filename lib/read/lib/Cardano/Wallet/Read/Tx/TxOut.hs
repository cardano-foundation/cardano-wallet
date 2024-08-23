{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'TxOut' — transaction output.
-}
module Cardano.Wallet.Read.Tx.TxOut
    ( -- * TxOut
      TxOut
    , mkBasicTxOut
    , getCompactAddr
    , getValue
    , utxoFromEraTx
    , upgradeTxOutToBabbageOrLater

    -- * Conversions
    , toBabbageOutput
    , toConwayOutput

    -- * Serialization
    , deserializeTxOut
    , serializeTxOut

    -- * Internal
    , mkEraTxOut
    )
    where

import Prelude

import Cardano.Ledger.Binary
    ( DecoderError (DecoderErrorCustom)
    )
import Cardano.Ledger.Compactible
    ( toCompact
    )
import Cardano.Read.Ledger.Tx.CollateralOutputs
    ( CollateralOutputs (..)
    , getEraCollateralOutputs
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    , OutputType
    , deserializeOutput
    , getEraCompactAddr
    , getEraValue
    , serializeOutput
    , upgradeToOutputBabbage
    , upgradeToOutputConway
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( Outputs (..)
    , getEraOutputs
    )
import Cardano.Wallet.Read.Address
    ( CompactAddr
    , fromEraCompactAddr
    )
import Cardano.Wallet.Read.Eras
    ( Babbage
    , Conway
    , Era (..)
    , EraValue (..)
    , IsEra (theEra)
    , indexOfEra
    , parseEraIndex
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    , pattern TxIn
    , pattern TxIx
    )
import Cardano.Wallet.Read.Value
    ( Value
    , fromEraValue
    , toMaryValue
    )
import Data.Foldable
    ( toList
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Maybe.Strict
    ( StrictMaybe (SNothing)
    )
import Data.Word
    ( Word16
    )

import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Wallet.Read.Tx.ScriptValidity as Read
import qualified Cardano.Wallet.Read.Tx.TxId as Read
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A 'TxOut' is a transaction output from any era — past, present
-- or next one.
newtype TxOut = TxOutC (EraValue Output)

instance Show TxOut where
    show (TxOutC v) = show v

-- | For testing — make a 'TxOut' from an era-indexed transaction output.
mkEraTxOut :: IsEra era => Output era -> TxOut
mkEraTxOut = TxOutC . EraValue

-- | Make a basic 'TxOut' from an address and a value.
mkBasicTxOut :: CompactAddr -> Value -> TxOut
mkBasicTxOut addr value =
    TxOutC (EraValue (Output txout :: Output Conway))
  where
    val = toMaryValue value
    -- The function 'toCompact' returns 'Nothing' when the input
    -- contains quantities that are outside the bounds of a Word64,
    -- x < 0 or x > (2^64 - 1)
    -- Such quantities are valid 'Integer's, but they cannot be
    -- encoded in a 'TxOut', hence the 'Nothing' result.
    -- Cardano.Ledger uses the same error message when converting.
    cVal = fromMaybe (error ("Illegal Value in TxOut: " ++ show val))
        $ toCompact val
    txout = Babbage.TxOutCompact addr cVal

{-# INLINEABLE getCompactAddr #-}
-- | Get the address which controls who can spend the transaction output.
getCompactAddr :: TxOut -> CompactAddr
getCompactAddr (TxOutC (EraValue txout)) =
    fromEraCompactAddr $ getEraCompactAddr txout

{-# INLINEABLE getValue #-}
-- | Get the monetary 'Value' in this transaction output.
getValue :: TxOut -> Value
getValue (TxOutC (EraValue txout)) =
    fromEraValue $ getEraValue txout

-- | Upgrade the internal representation of a 'TxOut'
-- to at least the 'Babbage' era.
--
-- Hardfork: Upgrade this function to a new era.
upgradeTxOutToBabbageOrLater :: TxOut -> TxOut
upgradeTxOutToBabbageOrLater x@(TxOutC (EraValue (txout :: Output era))) =
    case theEra :: Era era of
        Conway -> x
        Babbage -> x
        _ -> case upgradeToOutputBabbage txout of
            Just output -> TxOutC (EraValue output)
            _ -> error "upgradeTxOutToBabbageOrLater: impossible"

-- | Convert to an output in the 'Babbage' output, if possible.
--
-- Hardfork: Update this function to the current era.
toBabbageOutput :: TxOut -> Maybe (Output Babbage)
toBabbageOutput (TxOutC (EraValue txout)) = upgradeToOutputBabbage txout

-- | Convert to an output in the 'Conway' output
--
-- Hardfork: Update this function to the next era.
toConwayOutput :: TxOut -> Output Conway
toConwayOutput (TxOutC (EraValue txout)) = upgradeToOutputConway txout

{-----------------------------------------------------------------------------
    Serialization
------------------------------------------------------------------------------}

-- | Serialize a 'TxOut' in binary format, e.g. for storing in a database.
serializeTxOut :: TxOut -> BL.ByteString
serializeTxOut (TxOutC (EraValue (txout :: Output era))) =
    BL.cons tag (serializeOutput txout)
  where
    tag = toEnum (indexOfEra (theEra :: Era era))

type Dec era = Either DecoderError (Output era)

-- | Deserialize a 'TxOut' from the binary format.
--
-- prop> ∀ o.  deserializeTxOut (serializeTxOut o) == Just o
deserializeTxOut :: BL.ByteString -> Either DecoderError TxOut
deserializeTxOut bytes
    | Just (x,xs) <- BL.uncons bytes = do
        eera <- maybe (Left $ errUnknownEraIndex x) Right
            $ parseEraIndex (fromEnum x)
        case eera of
            EraValue (_ :: Era era) ->
                TxOutC . EraValue <$> (deserializeOutput xs :: Dec era)
    | otherwise =
        Left $ DecoderErrorCustom "Empty input" ""
  where
    errUnknownEraIndex =
        DecoderErrorCustom "Unknown era index" . T.pack . show

{-----------------------------------------------------------------------------
    Transactions
------------------------------------------------------------------------------}

{-# INLINEABLE utxoFromEraTx #-}
-- | Unspent transaction outputs (UTxO) created by the transaction.
utxoFromEraTx :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTx tx =
    case Read.getScriptValidity tx of
        Read.IsValid True -> utxoFromEraTxCollateralOutputs tx
        Read.IsValid False -> utxoFromEraTxOutputs tx

{-# INLINEABLE utxoFromEraTxOutputs #-}
-- | UTxO corresponding to the ordinary outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromEraTxOutputs
    :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTxOutputs tx =
    withFoldableOutputs toMap (getEraOutputs tx)
  where
    txid = Read.getTxId tx
    mkOutputInEra out = Output out :: Output era

    toMap
        :: forall t. Foldable t
        => t (OutputType era) -> Map.Map TxIn TxOut
    toMap =
        Map.fromList
        . zipWith (\ix -> mkTxInTxOutPair txid ix . mkOutputInEra) [0..]
        . toList

{-# INLINEABLE utxoFromEraTxCollateralOutputs #-}
-- | UTxO corresponding to the collateral outputs of a transaction.
--
-- This function ignores the transaction's script validity.
--
utxoFromEraTxCollateralOutputs
    :: forall era. IsEra era => Tx era -> Map.Map TxIn TxOut
utxoFromEraTxCollateralOutputs tx =
    withMaybeCollateralOutputs singleton (getEraCollateralOutputs tx)
  where
    txid = Read.getTxId tx
    mkOutputInEra out = Output out :: Output era

    singleton :: StrictMaybe (OutputType era) -> Map.Map TxIn TxOut
    singleton =
        Map.fromList
        . map (mkTxInTxOutPair txid index . mkOutputInEra)
        . toList

    -- To reference a collateral output within transaction t, we specify an
    -- output index that is equal to the number of ordinary outputs within t.
    --
    -- See definition of function "collOuts" within "Formal Specification of
    -- the Cardano Ledger for the Babbage era".
    --
    -- https://github.com/IntersectMBO/cardano-ledger?tab=readme-ov-file
    --
    index :: Word16
    index = fromIntegral $
        withFoldableOutputs length (getEraOutputs tx)

-- Helper function: Create a pair @(TxIn, TxOut)@.
mkTxInTxOutPair
    :: forall era. IsEra era
    => Read.TxId -> Word16 -> Output era -> (TxIn, TxOut)
mkTxInTxOutPair txid ix out =
    ( TxIn txid (TxIx ix)
    , TxOutC (EraValue out)
    )

-- Helper function: Treat the 'Outputs' as a 'Foldable' container.
withFoldableOutputs
    :: forall era a. IsEra era
    => (forall t. Foldable t => t (OutputType era) -> a)
    -> Outputs era
    -> a
withFoldableOutputs f = case theEra :: Era era of
    Byron -> \(Outputs x) -> f x
    Shelley -> \(Outputs x) -> f x
    Allegra -> \(Outputs x) -> f x
    Mary -> \(Outputs x) -> f x
    Alonzo -> \(Outputs x) -> f x
    Babbage -> \(Outputs x) -> f x
    Conway -> \(Outputs x) -> f x

-- Helper function: Treat the 'CollateralOutputs' as a 'StrictMaybe'.
withMaybeCollateralOutputs
    :: forall era a. IsEra era
    => (StrictMaybe (OutputType era) -> a)
    -> CollateralOutputs era
    -> a
withMaybeCollateralOutputs f = case theEra :: Era era of
    Byron -> \(CollateralOutputs _) -> f SNothing
    Shelley -> \(CollateralOutputs _) -> f SNothing
    Allegra -> \(CollateralOutputs _) -> f SNothing
    Mary -> \(CollateralOutputs _) -> f SNothing
    Alonzo -> \(CollateralOutputs _) -> f SNothing
    Babbage -> \(CollateralOutputs x) -> f x
    Conway -> \(CollateralOutputs x) -> f x
