{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main transaction data types used by the wallet.
--
module Cardano.Wallet.Primitive.Types.Tx
    (
    -- * Types
      Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxChange (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxStatus (..)
    , SealedTx (..)
    , UnsignedTx (..)
    , TransactionInfo (..)
    , Direction (..)
    , TokenBundleSizeAssessment (..)

    -- * Functions
    , fromTransactionInfo
    , inputs
    , isPending
    , toTxHistory
    , txIns
    , txMetadataIsNull
    , txOutCoin
    , txOutIsCoin
    ) where

import Prelude

import Cardano.Api.Typed
    ( TxMetadata (..), TxMetadataValue (..) )
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
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( comparing )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Time.Clock
    ( UTCTime )
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
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder

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

    , outputs
        :: ![TxOut]
        -- ^ NOTE: Order of outputs matters in the transaction representations.
        -- Outputs are used as inputs for next transactions which refer to them
        -- using their indexes. It matters also for serialization.

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
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance Buildable Tx where
    build (Tx tid _fee ins outs ws md) = mempty
        <> build tid
        <> build ("\n" :: String)
        <> blockListF' "inputs" build (fst <$> ins)
        <> blockListF' "outputs" build outs
        <> blockListF' "withdrawals" tupleF (Map.toList ws)
        <> nameF "metadata" (maybe "" build md)

txIns :: Set Tx -> Set TxIn
txIns = foldMap (Set.fromList . inputs)

inputs :: Tx -> [TxIn]
inputs = map fst . resolvedInputs

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

-- | Check whether a given TxOut is "ada-only", that is, it doesn't contain any
-- native assets.
txOutIsCoin :: TxOut -> Bool
txOutIsCoin = TokenBundle.isCoin . view #tokens

-- Since the 'TokenBundle' type deliberately does not provide an 'Ord' instance
-- (as that would lead to arithmetically invalid orderings), this means we can't
-- automatically derive an 'Ord' instance for the 'TxOut' type.
--
-- Instead, we define an 'Ord' instance that makes comparisons based on the list
-- representation of a 'TokenBundle'.
--
instance Ord TxOut where
    compare = comparing projection
      where
        projection :: TxOut ->
            ( Address
            , Coin
            , [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
            )
        projection out =
            ( out & view #address
            , out & view (#tokens . #coin)
            , out & view (#tokens . #tokens) & TokenMap.toNestedList
            )

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

-- | Additional information about a transaction, derived from the transaction
-- and ledger state. This should not be confused with 'TxMetadata' which is
-- application-specific data included with the transaction.
--
-- TODO: TxProperties or TxProps would be a good name for this type.
data TxMeta = TxMeta
    { status :: !TxStatus
    , direction :: !Direction
    , slotNo :: !SlotNo
    , blockHeight :: !(Quantity "block" Word32)
    , amount :: !Coin
    -- ^ Amount seen from the perspective of the wallet. Refers either to a
    -- spent value for outgoing transaction, or a received value on incoming
    -- transaction.
    , expiry :: !(Maybe SlotNo)
      -- ^ The slot at which a pending transaction will no longer be accepted
      -- into mempools.
    } deriving (Show, Eq, Ord, Generic)

instance NFData TxMeta

instance Buildable TxMeta where
    build (TxMeta s d sl (Quantity bh) c mex) = mempty
        <> build (WithDirection d c)
        <> " " <> build s
        <> " since " <> build sl <> "#" <> build bh
        <> maybe mempty (\ex -> " (expires slot " <> build ex <> ")") mex

data TxStatus
    = Pending
        -- ^ Created, but not yet in a block.
    | InLedger
        -- ^ Has been found in a block.
    | Expired
        -- ^ Time to live (TTL) has passed.
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData TxStatus

instance Buildable TxStatus where
    build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText TxStatus where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText TxStatus where
    toText = toTextFromBoundedEnum SnakeLowerCase

-- | An unsigned transaction.
--
-- See 'Tx' for a signed transaction.
--
data UnsignedTx input output change withdrawal = UnsignedTx
    { unsignedInputs
        :: NonEmpty input
        -- Inputs are *necessarily* non-empty because Cardano requires at least
        -- one UTxO input per transaction to prevent replayable transactions.
        -- (each UTxO being unique, including at least one UTxO in the
        -- transaction body makes it seemingly unique).

    , unsignedOutputs
        :: [output]
        -- Unlike inputs, it is perfectly reasonable to have empty outputs. The
        -- main scenario where this might occur is when constructing a
        -- delegation for the sake of submitting a certificate. This type of
        -- transaction does not typically include any target output and,
        -- depending on which input(s) get selected to fuel the transaction, it
        -- may or may not include a change output should its value be less than
        -- the minimal UTxO value set by the network.

    , unsignedChange
        :: [change]

    , unsignedWithdrawals
        :: [withdrawal]
    }
    deriving (Eq, Show)

-- | The effect of a @Transaction@ on the wallet balance.
data Direction
    = Outgoing -- ^ The wallet balance decreases.
    | Incoming -- ^ The wallet balance increases or stays the same.
    deriving (Show, Bounded, Enum, Eq, Ord, Generic)

instance NFData Direction

instance Buildable Direction where
    build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText Direction where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText Direction where
    toText = toTextFromBoundedEnum SnakeLowerCase

data WithDirection a = WithDirection Direction a

instance Buildable a => Buildable (WithDirection a) where
    build (WithDirection d a) = mempty
        <> (case d of; Incoming -> "+"; Outgoing -> "-")
        <> build a

-- | @SealedTx@ is a serialised transaction that is ready to be submitted
-- to the node.
newtype SealedTx = SealedTx { getSealedTx :: ByteString }
    deriving stock (Show, Eq, Generic)
    deriving newtype (ByteArrayAccess)

-- | True if the given metadata refers to a pending transaction
isPending :: TxMeta -> Bool
isPending = (== Pending) . (status :: TxMeta -> TxStatus)

-- | Full expanded and resolved information about a transaction, suitable for
-- presentation to the user.
data TransactionInfo = TransactionInfo
    { txInfoId :: !(Hash "Tx")
    -- ^ Transaction ID of this transaction
    , txInfoFee :: !(Maybe Coin)
    -- ^ Explicit transaction fee
    , txInfoInputs :: ![(TxIn, Coin, Maybe TxOut)]
    -- ^ Transaction inputs and (maybe) corresponding outputs of the
    -- source. Source information can only be provided for outgoing payments.
    , txInfoOutputs :: ![TxOut]
    -- ^ Payment destination.
    , txInfoWithdrawals :: !(Map RewardAccount Coin)
    -- ^ Withdrawals on this transaction.
    , txInfoMeta :: !TxMeta
    -- ^ Other information calculated from the transaction.
    , txInfoDepth :: Quantity "block" Natural
    -- ^ Number of slots since the transaction slot.
    , txInfoTime :: UTCTime
    -- ^ Creation time of the block including this transaction.
    , txInfoMetadata :: !(Maybe TxMetadata)
    -- ^ Application-specific extension data.
    } deriving (Generic, Show, Eq)

instance NFData TransactionInfo

-- | Reconstruct a transaction info from a transaction.
fromTransactionInfo :: TransactionInfo -> Tx
fromTransactionInfo info = Tx
    { txId = txInfoId info
    , fee = txInfoFee info
    , resolvedInputs = (\(a,b,_) -> (a,b)) <$> txInfoInputs info
    , outputs = txInfoOutputs info
    , withdrawals = txInfoWithdrawals info
    , metadata = txInfoMetadata info
    }

-- | Test whether the given metadata map is empty.
txMetadataIsNull :: TxMetadata -> Bool
txMetadataIsNull (TxMetadata md) = Map.null md

-- | Drop time-specific information
toTxHistory :: TransactionInfo -> (Tx, TxMeta)
toTxHistory info =
    (fromTransactionInfo info, txInfoMeta info)

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
