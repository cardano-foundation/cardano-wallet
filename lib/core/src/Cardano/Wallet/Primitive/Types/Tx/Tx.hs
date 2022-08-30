{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    , TxIn (..)
    , TxOut (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity(..)
    , ScriptWitnessIndex (..)

    -- * Functions
    , inputs
    , collateralInputs
    , txIns
    , txMetadataIsNull
    , txOutCoin
    , txOutAddCoin
    , txOutSubtractCoin
    , txScriptInvalid

    -- * Queries
    , txAssetIds
    , txOutAssetIds

    -- * Transformations
    , txMapAssetIds
    , txMapTxIds
    , txRemoveAssetId
    , txOutMapAssetIds
    , txOutRemoveAssetId

    ) where

import Prelude

import Cardano.Api
    ( ScriptWitnessIndex (..), TxMetadata (..), TxMetadataValue (..) )
import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, Lexicographic (..) )
import Cardano.Wallet.Primitive.Types.Tx.CBOR
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
import Data.Ord
    ( comparing )
import Data.Set
    ( Set )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..)
    , blockListF'
    , blockMapF
    , nameF
    , ordinalF
    , prefixF
    , suffixF
    , tupleF
    )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
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
        :: ![(TxIn, Coin)]
        -- ^ NOTE: Order of inputs matters in the transaction representation.
        -- The transaction id is computed from the binary representation of a
        -- tx, for which inputs are serialized in a specific order.

    , resolvedCollateralInputs
        :: ![(TxIn, Coin)]
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

data TxIn = TxIn
    { inputId
        :: !(Hash "Tx")
    , inputIx
        :: !Word32
    } deriving (Read, Show, Generic, Eq, Ord)

instance NFData TxIn

instance Buildable TxIn where
    build txin = mempty
        <> ordinalF (inputIx txin + 1)
        <> " "
        <> build (inputId txin)

data TxOut = TxOut
    { address
        :: !Address
    , tokens
        :: !TokenBundle
    } deriving (Read, Show, Generic, Eq)

-- Gets the current 'Coin' value from a transaction output.
--
-- 'Coin' values correspond to the ada asset.
--
txOutCoin :: TxOut -> Coin
txOutCoin = TokenBundle.getCoin . view #tokens

-- | Increments the 'Coin' value of a 'TxOut'.
--
-- Satisfies the following property for all values of 'c':
--
-- >>> txOutSubtractCoin c . txOutAddCoin c == id
--
txOutAddCoin :: Coin -> TxOut -> TxOut
txOutAddCoin val (TxOut addr tokens) =
    TxOut addr (tokens <> TokenBundle.fromCoin val)

-- | Decrements the 'Coin' value of a 'TxOut'.
--
-- Satisfies the following property for all values of 'c':
--
-- >>> txOutSubtractCoin c . txOutAddCoin c == id
--
-- If the given 'Coin' is greater than the 'Coin' value of the given 'TxOut',
-- the resulting 'TxOut' will have a 'Coin' value of zero.
--
txOutSubtractCoin :: Coin -> TxOut -> TxOut
txOutSubtractCoin toSubtract =
    over (#tokens . #coin) (`Coin.difference` toSubtract)

-- Since the 'TokenBundle' type deliberately does not provide an 'Ord' instance
-- (as that would lead to arithmetically invalid orderings), this means we can't
-- automatically derive an 'Ord' instance for the 'TxOut' type.
--
-- Instead, we define an 'Ord' instance that makes comparisons based on
-- lexicographic ordering of 'TokenBundle' values.
--
instance Ord TxOut where
    compare = comparing projection
      where
        projection (TxOut address bundle) = (address, Lexicographic bundle)

instance NFData TxOut

instance Buildable TxOut where
    build txOut = buildMap
        [ ("address"
          , addressShort)
        , ("coin"
          , build (txOutCoin txOut))
        , ("tokens"
          , build (TokenMap.Nested $ view (#tokens . #tokens) txOut))
        ]
      where
        addressShort = mempty
            <> prefixF 8 addressFull
            <> "..."
            <> suffixF 8 addressFull
        addressFull = build $ view #address txOut
        buildMap = blockMapF . fmap (first $ id @String)

instance Buildable (TxIn, TxOut) where
    build (txin, txout) = build txin <> " ==> " <> build txout

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
    [ F.foldMap txOutAssetIds (view #outputs tx)
    , F.foldMap txOutAssetIds (view #collateralOutput tx)
    ]

txOutAssetIds :: TxOut -> Set AssetId
txOutAssetIds (TxOut _ bundle) = TokenBundle.getAssets bundle

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
        (fmap (txOutMapAssetIds f))
    & over #collateralOutput
        (fmap (txOutMapAssetIds f))

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
        (fmap (`txOutRemoveAssetId` asset))
    & over #collateralOutput
        (fmap (`txOutRemoveAssetId` asset))

txOutMapAssetIds :: (AssetId -> AssetId) -> TxOut -> TxOut
txOutMapAssetIds f (TxOut address bundle) =
    TxOut address (TokenBundle.mapAssetIds f bundle)

txOutRemoveAssetId :: TxOut -> AssetId -> TxOut
txOutRemoveAssetId (TxOut address bundle) asset =
    TxOut address (TokenBundle.setQuantity bundle asset mempty)
