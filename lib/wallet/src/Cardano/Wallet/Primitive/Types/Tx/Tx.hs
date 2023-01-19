{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main transaction data types used by the wallet.
--
module Cardano.Wallet.Primitive.Types.Tx.Tx
    (
    -- * Types
      Tx (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity(..)
    , ScriptWitnessIndex (..)

    -- * Functions
    , inputs
    , collateralInputs
    , txIns
    , txMetadataIsNull
    , txScriptInvalid

    -- * Queries
    , txAssetIds

    -- * Transformations
    , txMapAssetIds
    , txMapTxIds
    , txRemoveAssetId

    ) where

import Prelude

import Cardano.Api
    ( ScriptWitnessIndex (..), TxMetadata (..), TxMetadataValue (..) )
import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), blockListF', nameF, tupleF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Primitive @Tx@-type.
--
-- Currently tailored for jormungandr in that inputs are @(TxIn, Coin)@
-- instead of @TxIn@. We might have to revisit this when supporting another
-- node.
data Tx = Tx
    { txId
        :: Hash "Tx"
        -- ^ Jörmungandr computes transaction id by hashing the full content of
        -- the transaction, which includes witnesses. Therefore, we need either
        -- to keep track of the witnesses to be able to re-compute the tx id

        -- every time, or, simply keep track of the id itself.
    , txCBOR
        :: Maybe TxCBOR
        -- ^ Serialized version of the transaction as received from the Node.
    , fee
        :: !(Maybe Coin)
        -- ^ Explicit fee for that transaction, if available. Fee are available
        -- explicitly in Shelley, but not in Byron although in Byron they can
        -- easily be re-computed from the delta between outputs and inputs.

    , resolvedInputs
        :: ![(TxIn, Maybe TxOut)]
        -- ^ NOTE: Order of inputs matters in the transaction representation.
        -- The transaction id is computed from the binary representation of a
        -- tx, for which inputs are serialized in a specific order.

    , resolvedCollateralInputs
        :: ![(TxIn, Maybe TxOut)]
        -- ^ NOTE: The order of collateral inputs matters in the transaction
        -- representation.  The transaction id is computed from the binary
        -- representation of a tx, for which collateral inputs are serialized
        -- in a specific order.

    , outputs
        :: ![TxOut]
        -- ^ NOTE: Order of outputs matters in the transaction representations.
        -- Outputs are used as inputs for next transactions which refer to them
        -- using their indexes. It matters also for serialization.

    , collateralOutput :: !(Maybe TxOut)
        -- ^ An output that is only created if a transaction script fails.

    , withdrawals
        :: !(Map RewardAccount Coin)
        -- ^ Withdrawals (of funds from a registered reward account) embedded in
        -- a transaction. The order does not matter.

    , metadata
        :: !(Maybe TxMetadata)
        -- ^ Semi-structured application-specific extension data stored in the
        -- transaction on chain.
        --
        -- This is not to be confused with 'TxMeta', which is information about
        -- a transaction derived from the ledger.
        --
        -- See Appendix E of
        -- <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec Shelley Ledger: Delegation/Incentives Design Spec>.

    , scriptValidity
        :: !(Maybe TxScriptValidity)
        -- ^ Tag indicating whether non-native scripts in this transaction
        -- passed validation. This is added by the block creator when
        -- constructing the block. May be 'Nothing' for pre-Alonzo and pending
        -- transactions.
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance Buildable Tx where
    build t = mconcat
        [ build (view #txId t)
        , build ("\n" :: String)
        , blockListF' "inputs"
            build (fst <$> view #resolvedInputs t)
        , blockListF' "collateral inputs"
            build (fst <$> view #resolvedCollateralInputs t)
        , blockListF' "outputs"
            build (view #outputs t)
        , blockListF' "collateral outputs"
            build (view #collateralOutput t)
        , blockListF' "withdrawals"
            tupleF (Map.toList $ view #withdrawals t)
        , nameF "metadata"
            (maybe "" build $ view #metadata t)
        , nameF "scriptValidity" (build $ view #scriptValidity t)
        ]

instance Buildable TxScriptValidity where
    build TxScriptValid = "valid"
    build TxScriptInvalid = "invalid"

txIns :: Set Tx -> Set TxIn
txIns = foldMap (\tx -> Set.fromList (inputs tx <> collateralInputs tx))

inputs :: Tx -> [TxIn]
inputs = map fst . resolvedInputs

collateralInputs :: Tx -> [TxIn]
collateralInputs = map fst . resolvedCollateralInputs

-- | Indicates whether or not a transaction is marked as having an invalid
--   script.
--
-- Pre-Alonzo era, scripts were not supported.
--
data TxScriptValidity
    = TxScriptValid
    -- ^ The transaction is not marked as having an invalid script.
    | TxScriptInvalid
    -- ^ The transaction is marked as having an invalid script.
  deriving (Generic, Show, Eq, Ord)

instance NFData TxScriptValidity

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

txAssetIds :: Tx -> Set AssetId
txAssetIds tx = F.fold
    [ F.foldMap TxOut.assetIds (view #outputs tx)
    , F.foldMap TxOut.assetIds (view #collateralOutput tx)
    ]

-- | Returns 'True' if (and only if) the given transaction is marked as having
--   an invalid script.
--
-- This function does not actually verify the validity of scripts; it merely
-- checks for the presence or absence of the 'TxScriptInvalid' marker.
--
txScriptInvalid :: Tx -> Bool
txScriptInvalid Tx {scriptValidity} = case scriptValidity of
  Just TxScriptInvalid -> True
  Just TxScriptValid -> False
  -- Script validation always passes in eras that don't support scripts
  Nothing -> False

-- | Test whether the given metadata map is empty.
txMetadataIsNull :: TxMetadata -> Bool
txMetadataIsNull (TxMetadata md) = Map.null md

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

txMapAssetIds :: (AssetId -> AssetId) -> Tx -> Tx
txMapAssetIds f tx = tx
    & over #outputs
        (fmap (TxOut.mapAssetIds f))
    & over #collateralOutput
        (fmap (TxOut.mapAssetIds f))

txMapTxIds :: (Hash "Tx" -> Hash "Tx") -> Tx -> Tx
txMapTxIds f tx = tx
    & over #txId
        f
    & over #resolvedInputs
        (fmap (first (over #inputId f)))
    & over #resolvedCollateralInputs
        (fmap (first (over #inputId f)))

txRemoveAssetId :: Tx -> AssetId -> Tx
txRemoveAssetId tx asset = tx
    & over #outputs
        (fmap (`TxOut.removeAssetId` asset))
    & over #collateralOutput
        (fmap (`TxOut.removeAssetId` asset))
