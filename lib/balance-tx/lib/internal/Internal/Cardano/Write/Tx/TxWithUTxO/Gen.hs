{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Provides generators and shrinkers for the 'TxWithUTxO' data type.
module Internal.Cardano.Write.Tx.TxWithUTxO.Gen
    ( generate
    , generateWithMinimalUTxO
    , generateWithSurplusUTxO
    , shrinkWith
    , shrinkTxWith
    , shrinkUTxOWith
    )
where

import Cardano.Ledger.Api
    ( EraTx (bodyTxL)
    )
import Cardano.Ledger.Api.Tx.Body
    ( allInputsTxBodyF
    )
import Control.Lens
    ( view
    )
import Internal.Cardano.Write.Eras
    ( IsRecentEra
    )
import Internal.Cardano.Write.Tx
    ( Tx
    , TxIn
    , TxOut
    , UTxO (UTxO)
    )
import Internal.Cardano.Write.Tx.TxWithUTxO
    ( pattern TxWithUTxO
    , type TxWithUTxO
    )
import Test.QuickCheck
    ( Gen
    , frequency
    )
import Test.QuickCheck.Extra
    ( genMapFromKeysWith
    , genNonEmptyDisjointMap
    , interleaveRoundRobin
    )
import Prelude

import qualified Internal.Cardano.Write.Tx.TxWithUTxO as TxWithUTxO

-- | Generates a 'TxWithUTxO' object.
--
-- The domain of the UTxO map is a superset of the transaction input set, but
-- it may or may not be a strict superset.
generate
    :: IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxIn)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generate genTx genTxIn genTxOut =
    frequency
        [ (9, generateWithMinimalUTxO genTx genTxOut)
        , (1, generateWithSurplusUTxO genTx genTxIn genTxOut)
        ]

-- | Generates a 'TxWithUTxO' object that has a minimal UTxO set.
--
-- The domain of the UTxO map is exactly equal to the transaction input set.
generateWithMinimalUTxO
    :: IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generateWithMinimalUTxO genTx genTxOut = do
    tx <- genTx
    utxo <- UTxO <$> genMapFromKeysWith genTxOut (txAllInputs tx)
    pure $ TxWithUTxO.constructFiltered tx utxo
  where
    txAllInputs = view (bodyTxL . allInputsTxBodyF)

-- | Generates a 'TxWithUTxO' object that has a surplus UTxO set.
--
-- The domain of the UTxO map is a strict superset of the transaction input set.
generateWithSurplusUTxO
    :: forall era
     . ()
    => IsRecentEra era
    => Gen (Tx era)
    -> Gen (TxIn)
    -> Gen (TxOut era)
    -> Gen (TxWithUTxO era)
generateWithSurplusUTxO genTx genTxIn genTxOut =
    generateWithMinimalUTxO genTx genTxOut >>= \case
        TxWithUTxO tx (UTxO utxo) -> do
            utxoSurplus <- genNonEmptyDisjointMap genTxIn genTxOut utxo
            pure $ TxWithUTxO.constructFiltered tx $ UTxO (utxo <> utxoSurplus)

shrinkWith
    :: IsRecentEra era
    => (Tx era -> [Tx era])
    -> (UTxO era -> [UTxO era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkWith shrinkTx shrinkUTxO txWithUTxO =
    interleaveRoundRobin
        [ shrinkTxWith shrinkTx txWithUTxO
        , shrinkUTxOWith shrinkUTxO txWithUTxO
        ]

shrinkTxWith
    :: IsRecentEra era
    => (Tx era -> [Tx era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkTxWith shrinkTx (TxWithUTxO tx utxo) =
    [TxWithUTxO.constructFiltered tx' utxo | tx' <- shrinkTx tx]

shrinkUTxOWith
    :: IsRecentEra era
    => (UTxO era -> [UTxO era])
    -> (TxWithUTxO era -> [TxWithUTxO era])
shrinkUTxOWith shrinkUTxO (TxWithUTxO tx utxo) =
    [TxWithUTxO.constructFiltered tx utxo' | utxo' <- shrinkUTxO utxo]
