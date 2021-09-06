{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module contains the core primitive of a Wallet. This is roughly a
-- Haskell translation of the [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
--
-- It doesn't contain any particular business-logic code, but defines a few
-- primitive operations on Wallet core types as well.

module Cardano.Wallet.Primitive.Types
    (
    -- * Block
      Block(..)
    , BlockHeader(..)

    -- * Network Parameters
    , NetworkParameters (..)
    , GenesisParameters (..)
    , SlottingParameters (..)
    , ProtocolParameters (..)
    , MinimumUTxOValue (..)
    , TxParameters (..)
    , TokenBundleMaxSize (..)
    , EraInfo (..)
    , emptyEraInfo
    , ActiveSlotCoefficient (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , EpochNo (..)
    , unsafeEpochNo
    , isValidEpochNo
    , FeePolicy (..)
    , SlotId (..)
    , SlotNo (..)
    , SlotLength (..)
    , SlotInEpoch (..)
    , StartTime (..)
    , stabilityWindowByron
    , stabilityWindowShelley

    -- * Wallet Metadata
    , WalletMetadata(..)
    , WalletId(..)
    , WalletName(..)
    , walletNameMinLength
    , walletNameMaxLength
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletDelegationNext (..)
    , WalletPassphraseInfo(..)
    , PassphraseScheme(..)
    , IsDelegatingTo (..)

    -- * Stake Pools
    , StakePoolsSummary (..)
    , PoolId(..)
    , PoolOwner(..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker (..)
    , StakeKeyCertificate (..)
    , PoolMetadataGCStatus (..)

    -- * Querying
    , SortOrder (..)

    -- * Ranges
    , Range (..)
    , RangeBound (..)
    , wholeRange
    , isAfterRange
    , isBeforeRange
    , isSubrangeOf
    , isWithinRange
    , mapRangeLowerBound
    , mapRangeUpperBound
    , rangeIsFinite
    , rangeIsSingleton
    , rangeIsValid
    , rangeHasLowerBound
    , rangeHasUpperBound
    , rangeLowerBound
    , rangeUpperBound

    -- * ProtocolMagic
    , ProtocolMagic (..)
    , mainnetMagic
    , testnetMagic

    -- * Polymorphic
    , Signature (..)

    -- * Settings
    , Settings (..)
    , SmashServer
    , unSmashServer
    , PoolMetadataSource (..)
    , defaultSettings
    , unsafeToPMS

    , TokenMetadataServer (..)
    ) where

import Prelude

import Cardano.Wallet.Orphans
    ()
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( DelegationCertificate (..), StakeKeyCertificate (..) )
import Cardano.Wallet.Primitive.Types.Slotting
    ( EpochLength (..)
    , EpochNo (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , SlotNo (..)
    , StartTime (..)
    , isValidEpochNo
    , unsafeEpochNo
    )
import Cardano.Wallet.Primitive.Types.StakePools
    ( PoolId (..)
    , PoolMetadataGCStatus (..)
    , PoolMetadataSource (..)
    , PoolOwner (..)
    , Settings (..)
    , SmashServer
    , StakePoolMetadata (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker (..)
    , StakePoolsSummary (..)
    , decodePoolIdBech32
    , defaultSettings
    , encodePoolIdBech32
    , poolIdBytesLength
    , unSmashServer
    , unsafeToPMS
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxSize (..) )
import Cardano.Wallet.Util
    ( parseURI, uriToText )
import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData (..) )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Int
    ( Int32 )
import Data.Kind
    ( Type )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Format
    ( defaultTimeLocale, formatTime )
import Data.Word
    ( Word16, Word32, Word64 )
import Fmt
    ( Buildable (..)
    , blockListF
    , blockListF'
    , indentF
    , listF'
    , prefixF
    , pretty
    , suffixF
    )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, natVal )
import Network.URI
    ( URI )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..), oneof )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                             Wallet Metadata
-------------------------------------------------------------------------------}

-- | Additional information about a wallet that can't simply be derived from
-- the blockchain like @Wallet s@ is.
--
-- Whereas @Wallet s@ in 'Cardano.Wallet.Primitive' can be updated using
-- @applyBlock@, @WalletMetadata@ can not*.
--
-- *) Except for possibly 'status' and 'delegation'...
data WalletMetadata = WalletMetadata
    { name
        :: !WalletName
    , creationTime
        :: !UTCTime
    , passphraseInfo
        :: !(Maybe WalletPassphraseInfo)
    , delegation
        :: !WalletDelegation
    } deriving (Eq, Show, Generic)

instance NFData WalletMetadata

formatUTCTime :: UTCTime -> Text
formatUTCTime =
    T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

instance Buildable WalletMetadata where
    build (WalletMetadata wName wTime _ wDelegation) = mempty
        <> build wName <> ", "
        <> "created at " <> build (formatUTCTime wTime) <> ", "
        <> build wDelegation

-- | Length-restricted name of a wallet
newtype WalletName = WalletName { getWalletName ::  Text }
    deriving (Generic, Eq, Show)

instance NFData WalletName

instance FromText WalletName where
    fromText t
        | T.length t < walletNameMinLength =
            Left $ TextDecodingError $
                "name is too short: expected at least "
                    <> show walletNameMinLength <> " character"
        | T.length t > walletNameMaxLength =
            Left $ TextDecodingError $
                "name is too long: expected at most "
                    <> show walletNameMaxLength <> " characters"
        | otherwise =
            return $ WalletName t

instance ToText WalletName where
    toText = getWalletName

instance Buildable WalletName where
    build = build . toText

-- | Calling 'fromText @WalletName' on shorter string will fail.
walletNameMinLength :: Int
walletNameMinLength = 1

-- | Calling 'fromText @WalletName' on a longer string will fail.
walletNameMaxLength :: Int
walletNameMaxLength = 255

newtype WalletId = WalletId { getWalletId :: Digest Blake2b_160 }
    deriving (Generic, Eq, Ord, Show)

instance NFData WalletId

instance FromText WalletId where
    fromText txt = maybe
        (Left $ TextDecodingError msg)
        (Right . WalletId)
        (decodeHex txt >>= digestFromByteString @_ @ByteString)
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"
        decodeHex =
            either (const Nothing) Just . convertFromBase Base16 . T.encodeUtf8

instance ToText WalletId where
    toText = T.decodeUtf8 . convertToBase Base16 . getWalletId

instance Buildable WalletId where
    build wid = prefixF 8 widF <> "..." <> suffixF 8 widF
      where
        widF = toText wid

data WalletDelegationStatus
    = NotDelegating
    | Delegating !PoolId
    deriving (Generic, Eq, Show)
instance NFData WalletDelegationStatus

instance Buildable WalletDelegationStatus where
    build = \case
        NotDelegating -> "∅"
        Delegating poolId -> build poolId

data WalletDelegationNext = WalletDelegationNext
    { changesAt :: !EpochNo
    , status :: !WalletDelegationStatus
    } deriving (Eq, Generic, Show)
instance NFData WalletDelegationNext

instance Buildable WalletDelegationNext where
    build (WalletDelegationNext e st) =
        build st <> " (in epoch: " <> build e <> ")"

data WalletDelegation = WalletDelegation
    { active :: !WalletDelegationStatus
    , next :: ![WalletDelegationNext]
    } deriving (Eq, Generic, Show)
instance NFData WalletDelegation

instance Buildable WalletDelegation where
    build (WalletDelegation act []) =
        "delegating to " <> build act
    build (WalletDelegation act xs) =
        build (WalletDelegation act []) <> " → "
        <> build (T.intercalate " → " $ pretty <$> xs)

class IsDelegatingTo a where
    isDelegatingTo :: (PoolId -> Bool) -> a -> Bool

instance IsDelegatingTo WalletDelegationStatus where
    isDelegatingTo predicate = \case
        Delegating pid -> predicate pid
        NotDelegating  -> False

instance IsDelegatingTo WalletDelegationNext where
    isDelegatingTo predicate WalletDelegationNext{status} =
        isDelegatingTo predicate status

instance IsDelegatingTo WalletDelegation where
    isDelegatingTo predicate WalletDelegation{active,next} =
        isDelegatingTo predicate active || any (isDelegatingTo predicate) next

data WalletPassphraseInfo = WalletPassphraseInfo
    { lastUpdatedAt :: UTCTime
    , passphraseScheme :: PassphraseScheme
    } deriving (Generic, Eq, Ord, Show)

instance NFData WalletPassphraseInfo

-- | A type to capture which encryption scheme should be used
data PassphraseScheme
    = EncryptWithScrypt
        -- ^ Legacy encryption scheme for passphrases
    | EncryptWithPBKDF2
        -- ^ Encryption scheme used since cardano-wallet
    deriving (Generic, Eq, Ord, Show, Read)

instance NFData PassphraseScheme

{-------------------------------------------------------------------------------
                                   Queries
-------------------------------------------------------------------------------}

-- | Represents a sort order, applicable to the results returned by a query.
data SortOrder
    = Ascending
        -- ^ Sort in ascending order.
    | Descending
        -- ^ Sort in descending order.
    deriving (Bounded, Enum, Eq, Generic, Show)

instance ToText SortOrder where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText SortOrder where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | Represents a range of values.
--
-- A range is defined by two /optional/ bounds:
--
-- 1. an /inclusive/ lower bound
-- 2. an /inclusive/ upper bound
--
-- There are four cases:
--
-- +---------------------------------+-------------+---------------------------+
-- | Value                           | Range       | Membership                |
-- |                                 | Represented | Function                  |
-- +=================================+=============+===========================+
-- | @'Range' ('Just' x) ('Just' y)@ | @[ x, y ]@  | @\\p -> p >= x && p <= y@ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' ('Just' x) 'Nothing' @ | @[ x, ∞ )@  | @\\p -> p >= x          @ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' 'Nothing'  ('Just' y)@ | @(−∞, y ]@  | @\\p -> p <= y          @ |
-- +---------------------------------+-------------+---------------------------+
-- | @'Range' 'Nothing'  'Nothing' @ | @(−∞, ∞ )@  | @\\p -> True            @ |
-- +---------------------------------+-------------+---------------------------+
--
data Range a = Range
    { inclusiveLowerBound :: Maybe a
    , inclusiveUpperBound :: Maybe a
    } deriving (Eq, Functor, Show)

-- | Apply a function to the lower bound of a range.
mapRangeLowerBound :: (a -> a) -> Range a -> Range a
mapRangeLowerBound f (Range x y) = Range (f <$> x) y

-- | Apply a function to the upper bound of a range.
mapRangeUpperBound :: (a -> a) -> Range a -> Range a
mapRangeUpperBound f (Range x y) = Range x (f <$> y)

-- | Represents a range boundary.
data RangeBound a
    = NegativeInfinity
    | InclusiveBound a
    | PositiveInfinity
    deriving (Eq, Ord)

-- | The range that includes everything.
wholeRange :: Range a
wholeRange = Range Nothing Nothing

-- | Returns 'True' if (and only if) the given range has an upper bound and the
--   specified value is greater than the upper bound.
isAfterRange :: Ord a => a -> Range a -> Bool
isAfterRange x (Range _ high) =
    maybe False (x >) high

-- | Returns 'True' if (and only if) the given range has a lower bound and the
--   specified value is smaller than the lower bound.
isBeforeRange :: Ord a => a -> Range a -> Bool
isBeforeRange x (Range low _) =
    maybe False (x <) low

-- | Returns 'True' if (and only if) the given value is not smaller than the
--   lower bound (if present) of the given range and is not greater than the
--   upper bound (if present) of the given range.
isWithinRange :: Ord a => a -> Range a -> Bool
isWithinRange x (Range low high) =
    (maybe True (x >=) low) &&
    (maybe True (x <=) high)

-- | Returns 'True' if (and only if) the given range has a lower bound.
rangeHasLowerBound :: Range a -> Bool
rangeHasLowerBound = isJust . inclusiveLowerBound

-- | Returns 'True' if (and only if) the given range has an upper bound.
rangeHasUpperBound :: Range a -> Bool
rangeHasUpperBound = isJust . inclusiveUpperBound

-- | Returns 'True' if (and only if) the given range has both a lower and upper
--   bound.
rangeIsFinite :: Range a -> Bool
rangeIsFinite r = rangeHasLowerBound r && rangeHasUpperBound r

-- | Returns 'True' if (and only if) the range covers exactly one value.
rangeIsSingleton :: Eq a => Range a -> Bool
rangeIsSingleton (Range a b) = ((==) <$> a <*> b) == Just True

-- | Returns 'True' if (and only if) the lower bound of a range is not greater
--   than its upper bound.
rangeIsValid :: Ord a => Range a -> Bool
rangeIsValid (Range a b) = ((<=) <$> a <*> b) /= Just False

-- | Get the lower bound of a 'Range'.
rangeLowerBound :: Range a -> RangeBound a
rangeLowerBound = maybe NegativeInfinity InclusiveBound . inclusiveLowerBound

-- | Get the upper bound of a 'Range'.
rangeUpperBound :: Range a -> RangeBound a
rangeUpperBound = maybe PositiveInfinity InclusiveBound . inclusiveUpperBound

-- | Returns 'True' if (and only if) the first given range is a subrange of the
--   second given range.
isSubrangeOf :: Ord a => Range a -> Range a -> Bool
isSubrangeOf r1 r2 =
    rangeLowerBound r1 >= rangeLowerBound r2 &&
    rangeUpperBound r1 <= rangeUpperBound r2

{-------------------------------------------------------------------------------
                                    Block
-------------------------------------------------------------------------------}

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: ![Tx]
    , delegations
        :: ![DelegationCertificate]
    } deriving (Show, Eq, Ord, Generic)

instance NFData Block

instance Buildable (Block) where
    build (Block h txs _) = mempty
        <> build h
        <> if null txs then " ∅" else "\n" <> indentF 4 (blockListF txs)

data BlockHeader = BlockHeader
    { slotNo
        :: SlotNo
    , blockHeight
        :: Quantity "block" Word32
    , headerHash
        :: !(Hash "BlockHeader")
    , parentHeaderHash
        :: !(Hash "BlockHeader")
    } deriving (Show, Eq, Ord, Generic)

instance NFData BlockHeader

instance Buildable BlockHeader where
    build (BlockHeader s (Quantity bh) hh ph) =
        prefixF 8 phF
        <> "<-["
        <> prefixF 8 hhF
        <> "-"
        <> build s
        <> "#" <> build (show bh)
        <> "]"
      where
        hhF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash hh
        phF = build $ T.decodeUtf8 $ convertToBase Base16 $ getHash ph

-- | A linear equation of a free variable `x`. Represents the @\x -> a + b*x@
-- function where @x@ can be the transaction size in bytes or, a number of
-- inputs + outputs.
--
-- @a@ and @b@ are constant coefficients.
--
-- FIXME 'Double' is an old artifact from the Byron era on cardano-sl. It must
-- go.
data FeePolicy = LinearFee
    (Quantity "lovelace" Double)
    (Quantity "lovelace/byte" Double)
    deriving (Eq, Show, Generic)

instance NFData FeePolicy

instance ToText FeePolicy where
    toText (LinearFee (Quantity a) (Quantity b)) =
        toText a <> " + " <> toText b <> "x"

instance FromText FeePolicy where
    fromText txt = case T.splitOn " + " txt of
        [a, b] | T.takeEnd 1 b == "x" ->
            left (const err) $ LinearFee
                <$> fmap Quantity (fromText a)
                <*> fmap Quantity (fromText (T.dropEnd 1 b))
        _ ->
            Left err
      where
        err = TextDecodingError
            "Unable to decode FeePolicy: \
            \Linear equation not in expected format: a + bx \
            \where 'a' and 'b' are numbers"

-- | A thin wrapper around derivation indexes. This can be used to represent
-- derivation path as homogeneous lists of 'DerivationIndex'. This is slightly
-- more convenient than having to carry heterogeneous lists of 'Index depth type'
-- and works fine because:
--
-- 1. The 'depth' matters not because what the depth captures is actually the
--    position of the index in that list. It makes sense to carry at the type
--    level when manipulating standalone indexes to avoid mistakes, but when
--    treating them as a part of a list it is redundant.
--
-- 2. The derivationType is captured by representing indexes as plain Word32.
--    The Soft / Hardened notation is for easing human-readability but in the
--    end, a soft index is simply a value < 2^31, whereas a "hardened" index is
--    simply a value >= 2^31. Therefore, instead of representing indexes as
--    derivationType + relative index within 0 and 2^31, we can represent them
--    as just an index between 0 and 2^32, which is what DerivationIndex does.
newtype DerivationIndex
    = DerivationIndex Word32
    deriving (Show, Eq, Ord, Generic)

instance NFData DerivationIndex

instance FromText DerivationIndex where
    fromText = fmap DerivationIndex . fromText

instance ToText DerivationIndex where
    toText (DerivationIndex index) = toText index

{-------------------------------------------------------------------------------
                              Network Parameters
-------------------------------------------------------------------------------}

-- | Records the complete set of parameters currently in use by the network
-- that are relevant to the wallet.
data NetworkParameters = NetworkParameters
    { genesisParameters :: GenesisParameters
       -- ^ See 'GenesisParameters'.
    , slottingParameters :: SlottingParameters
       -- ^ See 'SlottingParameters'.
    , protocolParameters :: ProtocolParameters
       -- ^ See 'ProtocolParameters'.
    } deriving (Generic, Show, Eq)

instance NFData NetworkParameters

instance Buildable NetworkParameters where
    build (NetworkParameters gp sp pp) = build gp <> build sp <> build pp

-- | Parameters defined by the __genesis block__.
--
-- At present, these values cannot be changed through the update system.
--
-- They can only be changed through a soft or hard fork.
data GenesisParameters = GenesisParameters
    { getGenesisBlockHash :: Hash "Genesis"
        -- ^ Hash of the very first block
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain.
    } deriving (Generic, Show, Eq)

instance NFData GenesisParameters

instance Buildable GenesisParameters where
    build gp = blockListF' "" id
        [ "Genesis block hash: " <> genesisF (getGenesisBlockHash gp)
        , "Genesis block date: " <> startTimeF (getGenesisBlockDate
            (gp :: GenesisParameters))
        ]
      where
        genesisF = build . T.decodeUtf8 . convertToBase Base16 . getHash
        startTimeF (StartTime s) = build s

data SlottingParameters = SlottingParameters
    { getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch.
    , getActiveSlotCoefficient :: ActiveSlotCoefficient
        -- ^ a.k.a 'f', in Genesis/Praos, corresponds to the % of active slots
        -- (i.e. slots for which someone can be elected as leader).
        --
        -- Determines the value of 'stabilityWindowShelley'.

    , getSecurityParameter :: Quantity "block" Word32
        -- ^ a.k.a 'k', used to compute the 'stability window' on the chain
        -- (i.e. the longest possible chain fork in slots).
        --
        -- Determines the value of 'stabilityWindowByron' and
        -- 'stabilityWindowShelley'.
    } deriving (Generic, Show, Eq)

instance NFData SlottingParameters

-- | In Byron, this stability window is equal to 2k slots, where _k_ is the
-- 'getSecurityParameter'
stabilityWindowByron :: SlottingParameters -> Quantity "block" Word64
stabilityWindowByron sp = Quantity (2 * k)
  where
    k = fromIntegral $ getQuantity $ getSecurityParameter sp

-- | In Shelley, this stability window is equal to _3k/f_ slots where _k_ is the
-- 'getSecurityParameter' and _f_ is the 'ActiveSlotCoefficient'.
stabilityWindowShelley :: SlottingParameters -> Quantity "block" Word64
stabilityWindowShelley sp = Quantity len
  where
    len = ceiling (3 * k / f)
    k = fromIntegral $ getQuantity $ getSecurityParameter sp
    f = unActiveSlotCoefficient $ getActiveSlotCoefficient sp

instance Buildable SlottingParameters where
    build sp = blockListF' "" id
        [ "Slot length:        " <> slotLengthF (getSlotLength sp)
        , "Epoch length:       " <> epochLengthF (getEpochLength sp)
        , "Active slot coeff:  " <> build (sp ^. #getActiveSlotCoefficient)
        , "Security parameter: " <> build (sp ^. #getSecurityParameter)
        ]
      where
        slotLengthF (SlotLength s) = build s
        epochLengthF (EpochLength s) = build s

newtype ActiveSlotCoefficient
    = ActiveSlotCoefficient { unActiveSlotCoefficient :: Double }
    deriving stock (Generic, Eq, Show)
    deriving newtype (Buildable, Num, Fractional)

instance NFData ActiveSlotCoefficient

-- |
--
-- It is expected that there is an order, @byron, shelley, allegra, mary@, by
-- which the @Maybe@ fields are filled in.
--
-- It might be cumbersome to work with this type. /But/ we don't need to. A
-- product of @Maybe@ is both what we can query from the node, and
-- what we need to provide in the wallet API.
data EraInfo info = EraInfo
    { byron :: Maybe info
    , shelley :: Maybe info
    , allegra :: Maybe info
    , mary :: Maybe info
    , alonzo :: Maybe info
    } deriving (Eq, Generic, Show, Functor)

emptyEraInfo :: EraInfo info
emptyEraInfo = EraInfo Nothing Nothing Nothing Nothing Nothing

instance NFData info => NFData (EraInfo info)

instance Buildable (EraInfo EpochNo) where
    build (EraInfo byron shelley allegra mary alonzo) = blockListF' "-" id
        [ "byron" <> boundF byron
        , "shelley" <> boundF shelley
        , "allegra" <> boundF allegra
        , "mary" <> boundF mary
        , "alonzo" <> boundF alonzo
        ]

      where
        boundF (Just e) = " from " <> build e
        boundF Nothing = " <not started>"

data MinimumUTxOValue
    -- | In Shelley, tx outputs could only be created if they were larger than
    -- this `MinimumUTxOValue`.
    = MinimumUTxOValue Coin

    -- | With Alonzo, `MinimumUTxOValue` is replaced by an ada-cost per word of
    -- the output. Note that the alonzo ledger assumes fixed sizes for address
    -- and coin, so the size is not the serialized size exactly.
    | MinimumUTxOValueCostPerWord Coin
    deriving (Eq, Generic, Show)

instance NFData MinimumUTxOValue

instance Buildable MinimumUTxOValue where
    build (MinimumUTxOValue c) = "constant " <> build c
    build (MinimumUTxOValueCostPerWord c) = build c <> " per word"

-- | Protocol parameters that can be changed through the update system.
--
data ProtocolParameters = ProtocolParameters
    { decentralizationLevel
        :: DecentralizationLevel
        -- ^ The current level of decentralization in the network.
    , txParameters
        :: TxParameters
        -- ^ Parameters that affect transaction construction.
    , desiredNumberOfStakePools
        :: Word16
        -- ^ The current desired number of stakepools in the network.
        -- Also known as k parameter.
    , minimumUTxOvalue
        :: MinimumUTxOValue
        -- ^ The minimum UTxO value.
    , stakeKeyDeposit
        :: Coin
        -- ^ Registering a stake key requires storage on the node and as such
        -- needs a deposit. There may be more actions that require deposit
        -- (such as registering a stake pool).
    , eras
        :: EraInfo EpochNo
    , maximumCollateralInputCount
        :: Word16
        -- ^ Limit on the maximum number of collateral inputs present in a
        -- transaction.
    } deriving (Eq, Generic, Show)

instance NFData ProtocolParameters

instance Buildable ProtocolParameters where
    build pp = blockListF' "" id
        [ "Decentralization level: " <> build (pp ^. #decentralizationLevel)
        , "Transaction parameters: " <> build (pp ^. #txParameters)
        , "Desired number of pools: " <> build (pp ^. #desiredNumberOfStakePools)
        , "Minimum UTxO value: " <> build (pp ^. #minimumUTxOvalue)
        , "Eras:\n" <> indentF 2 (build (pp ^. #eras))
        ]

-- | Indicates the current level of decentralization in the network.
--
-- According to the Design Specification for Delegation and Incentives in
-- Cardano, the decentralization parameter __/d/__ is a value in the range
-- '[0, 1]', where:
--
--   * __/d/__ = '1' indicates that the network is /completely federalized/.
--   * __/d/__ = '0' indicates that the network is /completely decentralized/.
--
-- However, in Cardano Wallet, we represent the decentralization level as a
-- percentage, where:
--
--   * '  0 %' indicates that the network is /completely federalized/.
--   * '100 %' indicates that the network is /completely decentralized/.
--
newtype DecentralizationLevel = DecentralizationLevel
    { unDecentralizationLevel :: Percentage }
    deriving (Bounded, Eq, Generic, Show)

instance NFData DecentralizationLevel

instance Buildable DecentralizationLevel where
    build = build . unDecentralizationLevel

-- | The maximum size of a serialized `TokenBundle` (`_maxValSize` in the Alonzo
-- ledger)
newtype TokenBundleMaxSize = TokenBundleMaxSize
    { unTokenBundleMaxSize :: TxSize }
    deriving (Eq, Generic, Show)

instance NFData TokenBundleMaxSize

instance Arbitrary TokenBundleMaxSize where
    arbitrary = TokenBundleMaxSize . TxSize <$>
        oneof
          -- Generate values close to the mainnet value of 4000 (and guard
          -- against underflow)
          [ fromIntegral . max 0 . (4000 +) <$> arbitrary @Int

          -- Generate more extreme values (both small and large)
          , fromIntegral <$> arbitrary @Word64
          ]
    shrink (TokenBundleMaxSize (TxSize s)) =
        map (TokenBundleMaxSize . TxSize . fromIntegral)
        . shrink @Word64 -- Safe w.r.t the generator, despite TxSize wrapping a
                         -- Natural
        $ fromIntegral s


-- | Parameters that relate to the construction of __transactions__.
--
data TxParameters = TxParameters
    { getFeePolicy :: FeePolicy
        -- ^ Formula for calculating the transaction fee.
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit).
    , getTokenBundleMaxSize :: TokenBundleMaxSize
        -- ^ Maximum size of a serialized `TokenBundle` (_maxValSize in the
        -- Alonzo ledger)
    } deriving (Generic, Show, Eq)

instance NFData TxParameters

instance Buildable TxParameters where
    build txp = listF' id
        [ "Fee policy: " <> feePolicyF (txp ^. #getFeePolicy)
        , "Tx max size: " <> txMaxSizeF (txp ^. #getTxMaxSize)
        ]
      where
        feePolicyF = build . toText
        txMaxSizeF (Quantity s) = build s

{-------------------------------------------------------------------------------
                                Protocol Magic
-------------------------------------------------------------------------------}

-- | Magic constant associated to a given network
newtype ProtocolMagic = ProtocolMagic { getProtocolMagic :: Int32 }
    deriving (Generic, Show, Eq)

instance ToText ProtocolMagic where
    toText (ProtocolMagic pm) = T.pack (show pm)

instance FromText ProtocolMagic where
    fromText = fmap (ProtocolMagic . fromIntegral @Natural) . fromText

-- | Hard-coded protocol magic for the Byron MainNet
mainnetMagic :: ProtocolMagic
mainnetMagic =  ProtocolMagic 764824073

-- | Derive testnet magic from a type-level Nat
testnetMagic :: forall pm. KnownNat pm => ProtocolMagic
testnetMagic = ProtocolMagic $ fromIntegral $ natVal $ Proxy @pm

{-------------------------------------------------------------------------------
                               Polymorphic Types
-------------------------------------------------------------------------------}

-- | A newtype to wrap raw bytestring representing signed data, captured with a
-- phantom type.
newtype Signature (what :: Type) = Signature { getSignature :: ByteString }
    deriving stock (Show, Eq, Generic)
    deriving newtype (ByteArrayAccess)

{-------------------------------------------------------------------------------
                               Metadata Services
-------------------------------------------------------------------------------}

newtype TokenMetadataServer = TokenMetadataServer
    { unTokenMetadataServer :: URI }
    deriving (Show, Generic, Eq)

instance ToText TokenMetadataServer where
    toText = uriToText . unTokenMetadataServer

instance FromText TokenMetadataServer where
    fromText = fmap TokenMetadataServer . parseURI
