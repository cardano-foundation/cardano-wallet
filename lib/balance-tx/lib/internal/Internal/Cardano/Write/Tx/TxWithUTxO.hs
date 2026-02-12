{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Provides the 'TxWithUTxO' data type.
module Internal.Cardano.Write.Tx.TxWithUTxO
    ( type TxWithUTxO
    , pattern TxWithUTxO
    , construct
    , constructFiltered
    , isValid
    )
where

import Cardano.Ledger.Api
    ( AlonzoEraTxBody (collateralInputsTxBodyL)
    , BabbageEraTxBody (referenceInputsTxBodyL)
    , EraTx (bodyTxL)
    , EraTxBody (TxBody, inputsTxBodyL)
    )
import Cardano.Ledger.Api.Tx.Body
    ( allInputsTxBodyF
    )
import Control.Lens
    ( over
    , view
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Semigroup.Cancellative
    ( LeftReductive (stripPrefix)
    )
import Data.Set.NonEmpty
    ( NESet
    )
import Internal.Cardano.Write.Eras
    ( IsRecentEra
    )
import Internal.Cardano.Write.Tx
    ( Tx
    , TxIn
    , UTxO (UTxO)
    )
import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

-- | A transaction with an associated UTxO set.
--
-- Every input in the transaction is guaranteed to resolve to a UTxO within the
-- associated UTxO set.
--
-- The UTxO set may also contain additional UTxOs that are not referenced by
-- the transaction.
data TxWithUTxO era = UnsafeTxWithUTxO !(Tx era) !(UTxO era)

deriving instance IsRecentEra era => Eq (TxWithUTxO era)

instance IsRecentEra era => Show (TxWithUTxO era) where
    show = fromMaybe "TxWithUTxO" . stripPrefix "Unsafe" . show

{-# COMPLETE TxWithUTxO #-}
pattern TxWithUTxO
    :: IsRecentEra era => Tx era -> UTxO era -> TxWithUTxO era
pattern TxWithUTxO tx utxo <- UnsafeTxWithUTxO tx utxo

-- | Constructs a 'TxWithUTxO' object from an existing transaction and UTxO set.
--
-- Construction succeeds if (and only if) every single input within the given
-- transaction resolves to a UTxO within the accompanying UTxO set.
--
-- Otherwise, if the transaction has any unresolvable inputs, this function
-- returns the non-empty set of those inputs.
construct
    :: IsRecentEra era
    => Tx era
    -> UTxO era
    -> Either (NESet TxIn) (TxWithUTxO era)
construct tx utxo =
    maybe (Right txWithUTxO) Left (unresolvableInputs txWithUTxO)
  where
    txWithUTxO = UnsafeTxWithUTxO tx utxo

-- | Constructs a 'TxWithUTxO' object from an existing transaction and UTxO set,
--   automatically filtering out any unresolvable inputs from the transaction.
--
-- A transaction input is unresolvable if (and only if) it does not resolve to
-- a UTxO within the given UTxO set.
constructFiltered
    :: forall era
     . IsRecentEra era
    => Tx era
    -> UTxO era
    -> TxWithUTxO era
constructFiltered tx utxo@(UTxO utxoMap) = UnsafeTxWithUTxO txFiltered utxo
  where
    txFiltered :: Tx era
    txFiltered = over bodyTxL removeUnresolvableInputs tx

    removeUnresolvableInputs :: TxBody era -> TxBody era
    removeUnresolvableInputs =
        over inputsTxBodyL f
            . over collateralInputsTxBodyL f
            . over referenceInputsTxBodyL f
      where
        f = Set.filter (`Map.member` utxoMap)

-- | Indicates whether or not a given 'TxWithUTxO' object is valid.
--
-- A 'TxWithUTxO' object is valid if (and only if) all inputs within the
-- transaction resolve to a UTxO within the associated UTxO set.
isValid :: IsRecentEra era => TxWithUTxO era -> Bool
isValid = null . unresolvableInputs

-- | Finds the complete set of unresolvable transaction inputs.
--
-- A transaction input is unresolvable if (and only if) it does not resolve
-- to a UTxO within the associated UTxO set.
--
-- For a valid 'TxWithUTxO' object, this function will return 'Nothing'.
unresolvableInputs
    :: forall era
     . IsRecentEra era
    => TxWithUTxO era
    -> Maybe (NESet TxIn)
unresolvableInputs (TxWithUTxO tx (UTxO utxo)) =
    NESet.nonEmptySet
        . Set.filter (`Map.notMember` utxo)
        . view (bodyTxL . allInputsTxBodyF)
        $ tx
