{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
    , TxChange (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , LocalTxSubmissionStatus (..)
    , TokenBundleSizeAssessor (..)
    , TokenBundleSizeAssessment (..)
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

    -- * Constants
    , txOutMinCoin
    , txOutMaxCoin
    , txOutMinTokenQuantity
    , txOutMaxTokenQuantity
    , txMintBurnMaxTokenQuantity

    -- * Constraints
    , TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    , TxSize (..)
    , txSizeDistance

    -- * Queries
    , txAssetIds
    , txOutAssetIds

    -- * Transformations
    , txMapAssetIds
    , txMapTxIds
    , txRemoveAssetId
    , txOutMapAssetIds
    , txOutRemoveAssetId

    -- * Checks
    , coinIsValidForTxOut

    -- * Conversions (Unsafe)
    , unsafeCoinToTxOutCoinValue

    ) where

import Prelude

import Cardano.Api
    ( ScriptWitnessIndex (..), TxMetadata (..), TxMetadataValue (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
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
    ( AssetId, Lexicographic (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Util
    ( HasCallStack )
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
import Data.Int
    ( Int64 )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( comparing )
import Data.Set
    ( Set )
import Data.Word
    ( Word32, Word64 )
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
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

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

data TxChange derivationPath = TxChange
    { address
        :: !Address
    , amount
        :: !Coin
    , assets
        :: !TokenMap
    , derivationPath
        :: derivationPath
    } deriving (Show, Generic, Eq, Ord)

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

-- | Information about when a transaction was submitted to the local node.
-- This is used for scheduling resubmissions.
data LocalTxSubmissionStatus tx = LocalTxSubmissionStatus
    { txId :: !(Hash "Tx")
    , submittedTx :: !tx
    , firstSubmission :: !SlotNo
    -- ^ Time of first successful submission to the local node.
    , latestSubmission :: !SlotNo
    -- ^ Time of most recent resubmission attempt.
    } deriving stock (Generic, Show, Eq, Functor)

-- | A function capable of assessing the size of a token bundle relative to the
--   upper limit of what can be included in a single transaction output.
--
-- In general, a token bundle size assessment function 'f' should satisfy the
-- following properties:
--
--    * Enlarging a bundle that exceeds the limit should also result in a
--      bundle that exceeds the limit:
--      @
--              f  b1           == TokenBundleSizeExceedsLimit
--          ==> f (b1 `add` b2) == TokenBundleSizeExceedsLimit
--      @
--
--    * Shrinking a bundle that's within the limit should also result in a
--      bundle that's within the limit:
--      @
--              f  b1                  == TokenBundleWithinLimit
--          ==> f (b1 `difference` b2) == TokenBundleWithinLimit
--      @
--
newtype TokenBundleSizeAssessor = TokenBundleSizeAssessor
    { assessTokenBundleSize :: TokenBundle -> TokenBundleSizeAssessment
    }
    deriving Generic

-- | Indicates the size of a token bundle relative to the upper limit of what
--   can be included in a single transaction output, defined by the protocol.
--
data TokenBundleSizeAssessment
    = TokenBundleSizeWithinLimit
    -- ^ Indicates that the size of a token bundle does not exceed the maximum
    -- size that can be included in a transaction output.
    | TokenBundleSizeExceedsLimit
    -- ^ Indicates that the size of a token bundle exceeds the maximum size
    -- that can be included in a transaction output.
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The smallest quantity of lovelace that can appear in a transaction output's
--   token bundle.
--
txOutMinCoin :: Coin
txOutMinCoin = Coin 0

-- | The greatest quantity of lovelace that can appear in a transaction output's
--   token bundle.
--
txOutMaxCoin :: Coin
txOutMaxCoin = Coin 45_000_000_000_000_000

-- | The smallest token quantity that can appear in a transaction output's
--   token bundle.
--
txOutMinTokenQuantity :: TokenQuantity
txOutMinTokenQuantity = TokenQuantity 1

-- | The greatest token quantity that can appear in a transaction output's
--   token bundle.
--
-- Although the ledger specification allows token quantities of unlimited
-- sizes, in practice we'll only see transaction outputs where the token
-- quantities are bounded by the size of a 'Word64'.
--
txOutMaxTokenQuantity :: TokenQuantity
txOutMaxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Word64

-- | The greatest quantity of any given token that can be minted or burned in a
--   transaction.
--
txMintBurnMaxTokenQuantity :: TokenQuantity
txMintBurnMaxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Int64

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

-- | Provides an abstract cost and size model for transactions.
--
-- This allows parts of a transaction to be costed (or sized) individually,
-- without having to compute the cost (or size) of an entire transaction.
--
-- Note that the following functions assume one witness is required per input:
--
-- - 'txInputCost'
-- - 'txInputSize'
--
-- This will lead to slight overestimation in the case of UTxOs that share the
-- same payment key.
--
data TxConstraints = TxConstraints
    { txBaseCost :: Coin
      -- ^ The constant cost of an empty transaction.
    , txBaseSize :: TxSize
      -- ^ The constant size of an empty transaction.
    , txInputCost :: Coin
      -- ^ The constant cost of a transaction input, assuming one witness is
      -- required per input.
    , txInputSize :: TxSize
      -- ^ The constant size of a transaction input, assuming one witness is
      -- required per input.
    , txOutputCost :: TokenBundle -> Coin
      -- ^ The variable cost of a transaction output.
    , txOutputSize :: TokenBundle -> TxSize
      -- ^ The variable size of a transaction output.
    , txOutputMaximumSize :: TxSize
      -- ^ The maximum size of a transaction output.
    , txOutputMaximumTokenQuantity :: TokenQuantity
      -- ^ The maximum token quantity that can appear in a transaction output.
    , txOutputMinimumAdaQuantity :: Address -> TokenMap -> Coin
      -- ^ The variable minimum ada quantity of a transaction output.
    , txOutputBelowMinimumAdaQuantity :: Address -> TokenBundle -> Bool
      -- ^ Returns 'True' if the given 'TokenBundle' has a 'Coin' value that is
      -- below the minimum required.
    , txRewardWithdrawalCost :: Coin -> Coin
      -- ^ The variable cost of a reward withdrawal.
    , txRewardWithdrawalSize :: Coin -> TxSize
      -- ^ The variable size of a reward withdrawal.
    , txMaximumSize :: TxSize
      -- ^ The maximum size of a transaction.
    }
    deriving Generic

txOutputCoinCost :: TxConstraints -> Coin -> Coin
txOutputCoinCost constraints = txOutputCost constraints . TokenBundle.fromCoin

txOutputCoinSize :: TxConstraints -> Coin -> TxSize
txOutputCoinSize constraints = txOutputSize constraints . TokenBundle.fromCoin

txOutputHasValidSize :: TxConstraints -> TokenBundle -> Bool
txOutputHasValidSize constraints b =
    txOutputSize constraints b <= txOutputMaximumSize constraints

txOutputHasValidTokenQuantities :: TxConstraints -> TokenMap -> Bool
txOutputHasValidTokenQuantities constraints m =
    TokenMap.maximumQuantity m <= txOutputMaximumTokenQuantity constraints

-- | The size of a transaction, or part of a transaction, in bytes.
--
newtype TxSize = TxSize { unTxSize :: Natural }
    deriving stock (Eq, Ord, Generic)
    deriving Show via (Quiet TxSize)

instance NFData TxSize

instance Semigroup TxSize where
    TxSize a <> TxSize b = TxSize (a + b)

instance Monoid TxSize where
    mempty = TxSize 0

-- | Computes the absolute distance between two transaction size quantities.
--
txSizeDistance :: TxSize -> TxSize -> TxSize
txSizeDistance (TxSize a) (TxSize b)
    | a >= b    = TxSize (a - b)
    | otherwise = TxSize (b - a)

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

{-------------------------------------------------------------------------------
                          Checks
-------------------------------------------------------------------------------}

coinIsValidForTxOut :: Coin -> Bool
coinIsValidForTxOut c = (&&)
    (c >= txOutMinCoin)
    (c <= txOutMaxCoin)

{-------------------------------------------------------------------------------
                          Conversions (Unsafe)
-------------------------------------------------------------------------------}

-- | Converts the given 'Coin' value to a value that can be included in a
--   transaction output.
--
-- Callers of this function must take responsibility for checking that the
-- given value is:
--
--   - not smaller than 'txOutMinCoin'
--   - not greater than 'txOutMaxCoin'
--
-- This function throws a run-time error if the pre-condition is violated.
--
unsafeCoinToTxOutCoinValue :: HasCallStack => Coin -> Word64
unsafeCoinToTxOutCoinValue c
    | c < txOutMinCoin =
        error $ unwords
            [ "unsafeCoinToTxOutCoinValue: coin value"
            , show c
            , "too small for transaction output"
            ]
    | c > txOutMaxCoin =
          error $ unwords
            [ "unsafeCoinToTxOutCoinValue: coin value"
            , show c
            , "too large for transaction output"
            ]
    | otherwise =
        Coin.unsafeToWord64 c
