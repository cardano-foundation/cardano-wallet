{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , UnsignedTx (..)
    , TransactionInfo (..)
    , Direction (..)
    , LocalTxSubmissionStatus (..)
    , TokenBundleSizeAssessor (..)
    , TokenBundleSizeAssessment (..)
    , TxScriptValidity(..)

    -- * Serialisation
    , SealedTx (serialisedTx, cardanoTx)
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , getSerialisedTxParts
    , unsafeSealedTxFromBytes
    , SerialisedTx (..)
    , SerialisedTxParts (..)
    , getSealedTxBody
    , getSealedTxWitnesses

    -- ** Unit testing helpers
    , MockSealedTx (..)
    , mockSealedTx
    , unMockSealedTxBytes

    -- * Functions
    , fromTransactionInfo
    , inputs
    , collateralInputs
    , isPending
    , toTxHistory
    , txIns
    , txMetadataIsNull
    , txOutCoin
    , failedScriptValidation

    -- * Constants
    , txOutMinTokenQuantity
    , txOutMaxTokenQuantity

    -- * Constraints
    , TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputCoinMinimum
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    , TxSize (..)
    , txSizeDistance

    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , InAnyCardanoEra (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , anyCardanoEra
    , deserialiseFromCBOR
    , serialiseToCBOR
    )
import Cardano.Binary
    ( DecoderError )
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
    ( TokenQuantity (..) )
import Cardano.Wallet.Util
    ( HasCallStack, internalError )
import Control.DeepSeq
    ( NFData (..), deepseq )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( partitionEithers )
import Data.Function
    ( on, (&) )
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
import Data.Type.Equality
    ( (:~:) (..), testEquality )
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
    , (+||)
    , (||+)
    )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )
import Text.Pretty.Simple
    ( pShowNoColor )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString.Char8 as B8
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

    , resolvedCollateral
        :: ![(TxIn, Coin)]
        -- ^ NOTE: The order of collateral inputs matters in the transaction
        -- representation.  The transaction id is computed from the binary
        -- representation of a tx, for which collateral inputs are serialized
        -- in a specific order.

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
        , blockListF' "collateral"
            build (fst <$> view #resolvedCollateral t)
        , blockListF' "inputs"
            build (fst <$> view #resolvedInputs t)
        , blockListF' "outputs"
            build (view #outputs t)
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
txIns = foldMap (Set.fromList . inputs)

inputs :: Tx -> [TxIn]
inputs = map fst . resolvedInputs

collateralInputs :: Tx -> [TxIn]
collateralInputs = map fst . resolvedCollateral

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
    { unsignedCollateral
        :: [input]
        -- Inputs used for collateral.

    , unsignedInputs
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
    deriving (Eq, Generic, Show)

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

-- | 'SealedTx' is a transaction for any hard fork era, possibly incomplete,
-- possibly unsigned, with dual representations to make it convenient to use.
--
-- Serialisation/deserialisation is usually done at the application boundaries
-- (e.g. in the API server), and then the wallet core can use it either as a
-- 'ByteString', or as a 'Cardano.Api.Tx'.
--
-- Construct it with either 'sealedTxFromCardano' or 'sealedTxFromBytes'.
data SealedTx = SealedTx
    { cardanoTx :: InAnyCardanoEra Cardano.Tx
    , serialisedTx :: ByteString
    } deriving stock Generic

instance Show SealedTx where
    -- InAnyCardanoEra is missing a Show instance, so define one inline.
    showsPrec d (SealedTx (InAnyCardanoEra era tx) bs) = showParen (d > 10) $
        showString "SealedTx (InAnyCardanoEra " .
        showsPrec 11 era .
        showChar ' ' .
        showsPrec 11 tx .
        showString ") " .
        showsPrec 11 bs

instance Buildable SealedTx where
    build (SealedTx (InAnyCardanoEra _ tx) _) = build $ pShowNoColor tx

instance Eq SealedTx where
    SealedTx (InAnyCardanoEra e1 _) a == SealedTx (InAnyCardanoEra e2 _) b =
        case testEquality e1 e2 of
            Just Refl -> a == b
            Nothing -> False

instance NFData SealedTx where
    rnf (SealedTx (InAnyCardanoEra _ tx) bs) = tx' `deepseq` bs `deepseq` ()
      where
        tx' = show tx -- should be good enough

getSealedTxBody :: SealedTx -> InAnyCardanoEra Cardano.TxBody
getSealedTxBody (SealedTx (InAnyCardanoEra era tx) _) =
    InAnyCardanoEra era (Cardano.getTxBody tx)

getSealedTxWitnesses :: SealedTx -> [InAnyCardanoEra Cardano.KeyWitness]
getSealedTxWitnesses (SealedTx (InAnyCardanoEra era tx) _) =
    [InAnyCardanoEra era w | w <- Cardano.getTxWitnesses tx]

-- | Construct a 'SealedTx' from a "Cardano.Api" transaction.
sealedTxFromCardano :: InAnyCardanoEra Cardano.Tx -> SealedTx
sealedTxFromCardano tx = SealedTx tx (cardanoTxToBytes tx)
  where
    cardanoTxToBytes :: InAnyCardanoEra Cardano.Tx -> ByteString
    cardanoTxToBytes (InAnyCardanoEra _era tx') = Cardano.serialiseToCBOR tx'

-- | Construct a 'SealedTx' from a "Cardano.Api" transaction.
sealedTxFromCardano' :: Cardano.IsCardanoEra era => Cardano.Tx era -> SealedTx
sealedTxFromCardano' = sealedTxFromCardano . InAnyCardanoEra Cardano.cardanoEra

-- | Construct a 'SealedTx' from a 'Cardano.Api.TxBody'.
sealedTxFromCardanoBody :: Cardano.IsCardanoEra era => Cardano.TxBody era -> SealedTx
sealedTxFromCardanoBody = sealedTxFromCardano . InAnyCardanoEra Cardano.cardanoEra . mk
  where
    mk body = Cardano.Tx body []

-- | Deserialise a Cardano transaction. The transaction can be in the format of
-- any era. This function will try the most recent era first, then
-- previous eras until 'ByronEra'.
cardanoTxFromBytes
    :: AnyCardanoEra -- ^ Most recent era
    -> ByteString -- ^ Serialised transaction
    -> Either DecoderError (InAnyCardanoEra Cardano.Tx)
cardanoTxFromBytes maxEra bs = asum $ map snd $ filter (withinEra maxEra . fst)
    [ deserialise AlonzoEra  Cardano.AsAlonzoEra
    , deserialise MaryEra    Cardano.AsMaryEra
    , deserialise AllegraEra Cardano.AsAllegraEra
    , deserialise ShelleyEra Cardano.AsShelleyEra
    , deserialise ByronEra   Cardano.AsByronEra
    ]
  where
    deserialise
        :: forall era. Cardano.IsCardanoEra era
        => CardanoEra era
        -> Cardano.AsType era
        -> (AnyCardanoEra, Either DecoderError (InAnyCardanoEra Cardano.Tx))
    deserialise era asEra =
        ( anyCardanoEra era
        , InAnyCardanoEra era <$> deserialiseFromCBOR (Cardano.AsTx asEra) bs
        )

    -- | Given a list of deserialise results that may fail, return the first
    -- success. If there was no success, then return the first failure message.
    asum :: [Either e a] -> Either e a
    asum xs = case partitionEithers xs of
        (_, (a:_)) -> Right a
        ((e:_), []) -> Left e
        ([], []) -> internalError "cardanoTxFromBytes: impossible"

-- | @a `withinEra` b@ is 'True' iff @b@ is the same era as @a@, or an earlier
-- one.
withinEra :: AnyCardanoEra -> AnyCardanoEra -> Bool
withinEra = (>=) `on` numberEra
  where
    numberEra :: AnyCardanoEra -> Int
    numberEra (AnyCardanoEra e) = case e of
        ByronEra   -> 1
        ShelleyEra -> 2
        AllegraEra -> 3
        MaryEra    -> 4
        AlonzoEra  -> 5

-- | Deserialise a transaction to construct a 'SealedTx'.
sealedTxFromBytes :: ByteString -> Either DecoderError SealedTx
sealedTxFromBytes = sealedTxFromBytes' (anyCardanoEra AlonzoEra)

-- | Deserialise a transaction to construct a 'SealedTx'.
sealedTxFromBytes'
    :: AnyCardanoEra -- ^ Most recent era
    -> ByteString -- ^ Serialised transaction
    -> Either DecoderError SealedTx
sealedTxFromBytes' era bs = SealedTx <$> cardanoTxFromBytes era bs <*> pure bs

-- | Get the serialised transaction body and witnesses from a 'SealedTx'.
getSerialisedTxParts :: SealedTx -> SerialisedTxParts
getSerialisedTxParts (SealedTx (InAnyCardanoEra _ tx) _) = SerialisedTxParts
    { serialisedTxBody = serialiseToCBOR $ Cardano.getTxBody tx
    , serialisedTxWitnesses = serialiseToCBOR <$> Cardano.getTxWitnesses tx
    }

-- | A serialised transaction that may be only partially signed, or even
-- invalid.
newtype SerialisedTx = SerialisedTx { payload :: ByteString }
    deriving stock (Show, Eq, Generic, Ord)
    deriving newtype (Semigroup, Monoid, ByteArray, ByteArrayAccess, NFData)

-- | @SerialisedTxParts@ is a serialised transaction body, and a possibly
-- incomplete set of serialised witnesses.
data SerialisedTxParts = SerialisedTxParts
    { serialisedTxBody :: ByteString
    , serialisedTxWitnesses :: [ByteString]
    } deriving stock (Show, Eq, Generic)

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
    , txInfoCollateral :: ![(TxIn, Coin, Maybe TxOut)]
    -- ^ Collateral inputs and (maybe) corresponding outputs.
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
    , txInfoScriptValidity :: !(Maybe TxScriptValidity)
    -- ^ Tag indicating whether non-native scripts in this transaction passed
    -- validation. This is added by the block creator when constructing the
    -- block. May be 'Nothing' for pre-Alonzo and pending transactions.
    } deriving (Generic, Show, Eq)

instance NFData TransactionInfo

-- | Indicates whether the script associated with a transaction has passed or
-- failed validation. Pre-Alonzo era, scripts were not supported.
data TxScriptValidity
    = TxScriptValid
    -- ^ Indicates that the script passed validation.
    | TxScriptInvalid
    -- ^ Indicates that the script failed validation.
  deriving (Generic, Show, Eq, Ord)

instance NFData TxScriptValidity

-- | Returns 'True' if and only if a transaction has failed script validation.
failedScriptValidation :: Tx -> Bool
failedScriptValidation Tx {scriptValidity} = case scriptValidity of
    Just TxScriptInvalid -> True
    Just TxScriptValid -> False
    -- Script validation always passes in eras that don't support scripts
    Nothing -> False

-- | Reconstruct a transaction info from a transaction.
fromTransactionInfo :: TransactionInfo -> Tx
fromTransactionInfo info = Tx
    { txId = txInfoId info
    , fee = txInfoFee info
    , resolvedCollateral = drop3rd <$> txInfoCollateral info
    , resolvedInputs = drop3rd <$> txInfoInputs info
    , outputs = txInfoOutputs info
    , withdrawals = txInfoWithdrawals info
    , metadata = txInfoMetadata info
    , scriptValidity = txInfoScriptValidity info
    }
  where
    drop3rd :: (a, b, c) -> (a, b)
    drop3rd (a, b, _) = (a, b)

-- | Test whether the given metadata map is empty.
txMetadataIsNull :: TxMetadata -> Bool
txMetadataIsNull (TxMetadata md) = Map.null md

-- | Drop time-specific information
toTxHistory :: TransactionInfo -> (Tx, TxMeta)
toTxHistory info =
    (fromTransactionInfo info, txInfoMeta info)

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
--              f  b1           == OutputTokenBundleSizeExceedsLimit
--          ==> f (b1 `add` b2) == OutputTokenBundleSizeExceedsLimit
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
    | OutputTokenBundleSizeExceedsLimit
    -- ^ Indicates that the size of a token bundle exceeds the maximum size
    -- that can be included in a transaction output.
    deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

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
    , txOutputMinimumAdaQuantity :: TokenMap -> Coin
      -- ^ The variable minimum ada quantity of a transaction output.
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

txOutputCoinMinimum :: TxConstraints -> Coin
txOutputCoinMinimum constraints = txOutputMinimumAdaQuantity constraints mempty

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

{-------------------------------------------------------------------------------
                      Internal functions for unit testing
-------------------------------------------------------------------------------}

-- | Only use this for tests.
unsafeSealedTxFromBytes :: HasCallStack => ByteString -> SealedTx
unsafeSealedTxFromBytes = either (internalError . errMsg) id . sealedTxFromBytes
  where
    errMsg reason = "unsafeSealedTxFromBytes: "+||reason||+""

-- | A helper type for mocking 'SealedTx' values by stuffing arbitrary strings
-- into the 'serialisedTx' field.
newtype MockSealedTx = MockSealedTx { unMockSealedTx :: SealedTx }
    deriving Generic

instance Eq MockSealedTx where
    MockSealedTx (SealedTx _ a) == MockSealedTx (SealedTx _ b) = a == b

instance Buildable MockSealedTx where
    build (MockSealedTx (SealedTx _ bs)) = build $ B8.unpack bs

instance Show MockSealedTx where
    show (MockSealedTx (SealedTx _ bs)) = "mockSealedTx " <> show bs

instance NFData MockSealedTx where
    rnf (MockSealedTx (SealedTx _ bs)) = bs `deepseq` ()

-- | Construct a 'MockSealedTx' from a string which need not be a well-formed
-- serialised Cardano transaction.
--
-- Be careful using the embedded 'SealedTx', because any attempt to evaluate the
-- 'cardanoTx' field will likely crash.
mockSealedTx :: HasCallStack => ByteString -> MockSealedTx
mockSealedTx = MockSealedTx . SealedTx
    (internalError "mockSealedTx: attempted to decode gibberish")

unMockSealedTxBytes :: MockSealedTx -> ByteString
unMockSealedTxBytes = serialisedTx . unMockSealedTx
