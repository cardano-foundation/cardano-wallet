{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
--
module Cardano.Wallet.Write.Tx.Balance.CoinSelection
    ( -- Coin-selection for balanceTx
      -- TODO: Move the actual coin-selection function to here
      estimateTxSize
    , estimateTxCost
    , TxSkeleton (..)
    , sizeOf_BootstrapWitnesses

      -- * Needed For migration
    , txConstraints

      -- * Needed by the wallet
    , _txRewardWithdrawalCost
    )

    where

import Prelude

import Cardano.Address.Script
    ( Script (..) )
import Cardano.Ledger.Api
    ( ppMaxTxSizeL, ppMaxValSizeL, ppMinFeeBL )
import Cardano.Wallet.Address.Discovery.Shared
    ( estimateMaxWitnessRequiredPerInput )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..), TxSize (..), txOutMaxCoin, txSizeDistance )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( Convert (..) )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..) )
import Cardano.Wallet.Write.ProtocolParameters
    ( ProtocolParameters (..) )
import Cardano.Wallet.Write.Tx
    ( FeePerByte (..)
    , IsRecentEra (recentEra)
    , RecentEra (..)
    , ShelleyLedgerEra
    , TxOut
    , computeMinimumCoinForTxOut
    , getFeePerByte
    , isBelowMinimumCoinForTxOut
    , withConstraints
    )
import Control.Lens
    ( (^.) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Set
    ( Set )
import Data.Word
    ( Word64, Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Shelley.Compatibility.Ledger as W
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

-- | Like the 'TxConstraints' field 'txRewardWithdrawalCost', but with added
-- support for shared wallets via the 'CA.ScriptTemplate' argument.
--
-- We may or may not want to support shared wallets in the full txConstraints.
_txRewardWithdrawalCost
    :: Write.FeePerByte
    -> Either CA.ScriptTemplate TxWitnessTag
    -> Coin
    -> Coin
_txRewardWithdrawalCost feePerByte witType =
    toWallet
    . Write.feeOfBytes feePerByte
    . unTxSize
    . _txRewardWithdrawalSize witType

-- | Like the 'TxConstraints' field 'txRewardWithdrawalSize', but with added
-- support for shared wallets via the 'CA.ScriptTemplate' argument.
--
-- We may or may not want to support shared wallets in the full txConstraints.
_txRewardWithdrawalSize
    :: Either CA.ScriptTemplate TxWitnessTag
    -> Coin
    -> TxSize
_txRewardWithdrawalSize _ (Coin 0) = TxSize 0
_txRewardWithdrawalSize witType _ =
        sizeOf_Withdrawals 1 <> wits
      where
        wits = case witType of
            Right TxWitnessByronUTxO ->
                sizeOf_BootstrapWitnesses 1 - sizeOf_BootstrapWitnesses 0
            Right TxWitnessShelleyUTxO ->
                sizeOf_VKeyWitnesses 1
            Left scriptTemplate ->
                let n = fromIntegral $ estimateMaxWitnessRequiredPerInput
                        $ view #template scriptTemplate
                in sizeOf_VKeyWitnesses n


txConstraints
    :: forall era. IsRecentEra era
    => ProtocolParameters era
    -> TxWitnessTag
    -> TxConstraints
txConstraints (ProtocolParameters protocolParams) witnessTag = TxConstraints
    { txBaseCost
    , txBaseSize
    , txInputCost
    , txInputSize
    , txOutputCost
    , txOutputSize
    , txOutputMaximumSize
    , txOutputMaximumTokenQuantity
    , txOutputMinimumAdaQuantity
    , txOutputBelowMinimumAdaQuantity
    , txRewardWithdrawalCost
    , txRewardWithdrawalSize
    , txMaximumSize
    }
  where
    era = recentEra @era

    txBaseCost =
        constantTxFee <> estimateTxCost feePerByte empty

    constantTxFee = withConstraints era $
        toWallet $ protocolParams ^. ppMinFeeBL

    feePerByte = getFeePerByte (recentEra @era) protocolParams

    txBaseSize =
        estimateTxSize empty

    txInputCost =
        marginalCostOf empty {txInputCount = 1}

    txInputSize =
        marginalSizeOf empty {txInputCount = 1}

    txOutputCost bundle =
        marginalCostOf empty {txOutputs = [mkTxOut bundle]}

    txOutputSize bundle =
        marginalSizeOf empty {txOutputs = [mkTxOut bundle]}

    txOutputMaximumSize = withConstraints era $ (<>)
        (txOutputSize mempty)
        (TxSize (protocolParams ^. ppMaxValSizeL))

    txOutputMaximumTokenQuantity =
        TokenQuantity $ fromIntegral $ maxBound @Word64

    txOutputMinimumAdaQuantity addr tokens = toWallet $
        computeMinimumCoinForTxOut
            era
            protocolParams
            (mkLedgerTxOut era addr (TokenBundle txOutMaxCoin tokens))

    txOutputBelowMinimumAdaQuantity addr bundle =
            isBelowMinimumCoinForTxOut
                era
                protocolParams
                (mkLedgerTxOut era addr bundle)

    txRewardWithdrawalCost =
        _txRewardWithdrawalCost feePerByte (Right witnessTag)

    txRewardWithdrawalSize =
        _txRewardWithdrawalSize (Right witnessTag)

    txMaximumSize = withConstraints era $
        TxSize $ protocolParams ^. ppMaxTxSizeL

    empty :: TxSkeleton
    empty = emptyTxSkeleton witnessTag

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalCostOf :: TxSkeleton -> Coin
    marginalCostOf skeleton =
        Coin.distance
            (estimateTxCost feePerByte empty)
            (estimateTxCost feePerByte skeleton)

    -- Computes the size difference between the given skeleton and an empty
    -- skeleton.
    marginalSizeOf :: TxSkeleton -> TxSize
    marginalSizeOf =
        txSizeDistance txBaseSize . estimateTxSize

    -- Constructs a real transaction output from a token bundle.
    mkTxOut :: TokenBundle -> W.TxOut
    mkTxOut = W.TxOut dummyAddress
      where
        dummyAddress :: Address
        dummyAddress = Address $ BS.replicate dummyAddressLength nullByte

        dummyAddressLength :: Int
        dummyAddressLength = 57
        -- Note: We are at liberty to overestimate the length of an address
        -- (which is safe). Therefore, we can choose a length that we know is
        -- greater than or equal to all address lengths.

        nullByte :: Word8
        nullByte = 0


--
-- Size estimation
--


-- | Includes just the parts of a transaction necessary to estimate its size.
--
-- In particular, this record type includes the minimal set of data needed for
-- the 'estimateTxCost' and 'estimateTxSize' functions to perform their
-- calculations, and nothing else.
--
-- The data included in 'TxSkeleton' is a subset of the data included in the
-- union of 'SelectionSkeleton' and 'TransactionCtx'.
--
data TxSkeleton = TxSkeleton
    { txWitnessTag :: !TxWitnessTag
    , txInputCount :: !Int
    , txOutputs :: ![W.TxOut]
    , txChange :: ![Set AssetId]
    , txPaymentTemplate :: !(Maybe (CA.Script CA.Cosigner))
    }
    deriving (Eq, Show, Generic)

-- | Constructs an empty transaction skeleton.
--
-- This may be used to estimate the size and cost of an empty transaction.
--
emptyTxSkeleton :: TxWitnessTag -> TxSkeleton
emptyTxSkeleton txWitnessTag = TxSkeleton
    { txWitnessTag
    , txInputCount = 0
    , txOutputs = []
    , txChange = []
    , txPaymentTemplate = Nothing
    }

-- | Estimates the final cost of a transaction based on its skeleton.
--
-- The constant tx fee is /not/ included in the result of this function.
estimateTxCost :: FeePerByte -> TxSkeleton -> Coin
estimateTxCost (FeePerByte feePerByte) skeleton =
    computeFee (estimateTxSize skeleton)
  where
    computeFee :: TxSize -> Coin
    computeFee (TxSize size) = Coin $ feePerByte * size

-- | Estimates the final size of a transaction based on its skeleton.
--
-- This function uses the upper bounds of CBOR serialized objects as the basis
-- for many of its calculations. The following document is used as a reference:
--
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley/test-suite/cddl-files/shelley.cddl
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/shelley-ma/test-suite/cddl-files/shelley-ma.cddl
-- https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/test-suite/cddl-files/alonzo.cddl
--
estimateTxSize
    :: TxSkeleton
    -> TxSize
estimateTxSize skeleton =
    sizeOf_Transaction
  where
    TxSkeleton
        { txWitnessTag
        , txInputCount
        , txOutputs
        , txChange
        , txPaymentTemplate
        } = skeleton

    numberOf_Inputs :: Natural
    numberOf_Inputs
        = fromIntegral txInputCount

    numberOf_ScriptVkeyWitnessesForPayment
        = maybe 0 estimateMaxWitnessRequiredPerInput txPaymentTemplate

    numberOf_VkeyWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO -> 0
            TxWitnessShelleyUTxO ->
                -- there cannot be missing payment script if there is delegation script
                -- the latter is optional
                if numberOf_ScriptVkeyWitnessesForPayment == 0 then
                    numberOf_Inputs
                else
                    (numberOf_Inputs * numberOf_ScriptVkeyWitnessesForPayment)

    numberOf_BootstrapWitnesses
        = case txWitnessTag of
            TxWitnessByronUTxO -> numberOf_Inputs
            TxWitnessShelleyUTxO -> 0

    -- transaction =
    --   [ transaction_body
    --   , transaction_witness_set
    --   , transaction_metadata / null
    --   ]
    sizeOf_Transaction
        = sizeOf_SmallArray
        + sizeOf_TransactionBody
        + sizeOf_WitnessSet

    -- transaction_body =
    --   { 0 : set<transaction_input>
    --   , 1 : [* transaction_output]
    --   , 2 : coin ; fee
    --   , 3 : uint ; ttl
    --   , ? 4 : [* certificate]
    --   , ? 5 : withdrawals
    --   , ? 6 : update
    --   , ? 7 : metadata_hash
    --   , ? 8 : uint ; validity interval start
    --   , ? 9 : mint
    --   }
    sizeOf_TransactionBody
        = sizeOf_SmallMap
        + sizeOf_Inputs
        + sizeOf_Outputs
        + sizeOf_Fee
        + sizeOf_Ttl
        + sizeOf_Update
        + sizeOf_ValidityIntervalStart
        + sizeOf_HistoricalPadding
      where
        -- Preserved out of caution during refactoring. We should be able to
        -- drop this, but we may as well wait until we have completely
        -- water-proof testing of the size estimation, e.g:
        -- prop> forall baseTx update.
        --      sizeOf_Update x >=
        --          (serializedSize (update baseTx)
        --          - serializedSize baseTx)
        -- where update is something similar to 'TxUpdate' or 'TxSkeleton'.
        sizeOf_HistoricalPadding = sizeOf_NoMetadata
          where
            -- When it's "empty", metadata are represented by a special
            -- "null byte" in CBOR `F6`.
            sizeOf_NoMetadata = 1

        -- 0 => set<transaction_input>
        sizeOf_Inputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + sizeOf_Input * TxSize numberOf_Inputs

        -- 1 => [* transaction_output]
        sizeOf_Outputs
            = sizeOf_SmallUInt
            + sizeOf_Array
            + F.sum (sizeOf_Output <$> txOutputs)
            + F.sum (sizeOf_ChangeOutput <$> txChange)

        -- 2 => fee
        sizeOf_Fee
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- 3 => ttl
        sizeOf_Ttl
            = sizeOf_SmallUInt
            + sizeOf_UInt

        -- ?6 => update
        sizeOf_Update
            = 0 -- Assuming no updates is running through cardano-wallet

        -- ?8 => uint ; validity interval start
        sizeOf_ValidityIntervalStart
            = sizeOf_UInt

    -- transaction_input =
    --   [ transaction_id : $hash32
    --   , index : uint
    --   ]
    sizeOf_Input
        = sizeOf_SmallArray
        + sizeOf_Hash32
        + sizeOf_UInt

    -- post_alonzo_transaction_output =
    --   { 0 : address
    --   , 1 : value
    --   , ? 2 : datum_option ; New; datum option
    --   , ? 3 : script_ref   ; New; script reference
    --   }
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_PostAlonzoTransactionOutput W.TxOut {address, tokens}
        = sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_Address address
        + sizeOf_SmallUInt
        + sizeOf_SmallArray
        + sizeOf_Coin (TokenBundle.getCoin tokens)
        + sumVia sizeOf_NativeAsset (TokenBundle.getAssets tokens)

    sizeOf_Output
        = sizeOf_PostAlonzoTransactionOutput

    sizeOf_ChangeOutput :: Set AssetId -> TxSize
    sizeOf_ChangeOutput
        = sizeOf_PostAlonzoChangeOutput

    -- post_alonzo_transaction_output =
    --   { 0 : address
    --   , 1 : value
    --   , ? 2 : datum_option ; New; datum option
    --   , ? 3 : script_ref   ; New; script reference
    --   }
    -- value =
    --   coin / [coin,multiasset<uint>]
    sizeOf_PostAlonzoChangeOutput :: Set AssetId -> TxSize
    sizeOf_PostAlonzoChangeOutput xs
        = sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_ChangeAddress
        + sizeOf_SmallMap
        + sizeOf_SmallUInt
        + sizeOf_LargeUInt
        + sumVia sizeOf_NativeAsset xs

    -- We carry addresses already serialized, so it's a matter of measuring.
    sizeOf_Address addr
        = 2 + fromIntegral (BS.length (unAddress addr))

    -- For change address, we consider the worst-case scenario based on the
    -- given wallet scheme. Byron addresses are larger.
    --
    -- NOTE: we could do slightly better if we wanted to for Byron addresses and
    -- discriminate based on the network as well since testnet addresses are
    -- larger than mainnet ones. But meh.
    sizeOf_ChangeAddress
        = case txWitnessTag of
            TxWitnessByronUTxO -> 85
            TxWitnessShelleyUTxO -> 59

    -- value = coin / [coin,multiasset<uint>]
    -- We consider "native asset" to just be the "multiasset<uint>" part of the
    -- above, hence why we don't also include the size of the coin. Where this
    -- is used, the size of the coin and array are are added too.
    sizeOf_NativeAsset AssetId{tokenName}
        = sizeOf_MultiAsset sizeOf_LargeUInt tokenName

    -- multiasset<a> = { * policy_id => { * asset_name => a } }
    -- policy_id = scripthash
    -- asset_name = bytes .size (0..32)
    sizeOf_MultiAsset sizeOf_a name
      = sizeOf_SmallMap -- NOTE: Assuming < 23 policies per output
      + sizeOf_Hash28
      + sizeOf_SmallMap -- NOTE: Assuming < 23 assets per policy
      + sizeOf_AssetName name
      + sizeOf_a

    -- asset_name = bytes .size (0..32)
    sizeOf_AssetName name
        = 2 + fromIntegral (BS.length $ unTokenName name)

    -- Coins can really vary so it's very punishing to always assign them the
    -- upper bound. They will typically be between 3 and 9 bytes (only 6 bytes
    -- difference, but on 20+ outputs, one starts feeling it).
    --
    -- So, for outputs, since we have the values, we can compute it accurately.
    sizeOf_Coin
        = TxSize
        . fromIntegral
        . BS.length
        . CBOR.toStrictByteString
        . CBOR.encodeWord64
        . Coin.unsafeToWord64

    determinePaymentTemplateSize scriptCosigner
        = sizeOf_Array
        + sizeOf_SmallUInt
        + TxSize numberOf_Inputs * (sizeOf_NativeScript scriptCosigner)

    -- transaction_witness_set =
    --   { ?0 => [* vkeywitness ]
    --   , ?1 => [* native_script ]
    --   , ?2 => [* bootstrap_witness ]
    --   }
    sizeOf_WitnessSet
        = sizeOf_SmallMap
        + sizeOf_VKeyWitnesses numberOf_VkeyWitnesses
        + maybe 0 determinePaymentTemplateSize txPaymentTemplate
        -- FIXME: Payment template needs to be multiplied with number of inputs
        + sizeOf_BootstrapWitnesses numberOf_BootstrapWitnesses

-- ?5 => withdrawals
sizeOf_Withdrawals :: Natural -> TxSize
sizeOf_Withdrawals n
    = (if n > 0
        then sizeOf_SmallUInt + sizeOf_SmallMap
        else 0)
    + sizeOf_Withdrawal * (TxSize n)

  where
    -- withdrawals =
    --   { * reward_account => coin }
    sizeOf_Withdrawal
        = sizeOf_Hash28
        + sizeOf_LargeUInt

-- ?0 => [* vkeywitness ]
sizeOf_VKeyWitnesses :: Natural -> TxSize
sizeOf_VKeyWitnesses n
    = (if n > 0
        then sizeOf_Array + sizeOf_SmallUInt else 0)
    + sizeOf_VKeyWitness * (TxSize n)

-- ?2 => [* bootstrap_witness ]
sizeOf_BootstrapWitnesses :: Natural -> TxSize
sizeOf_BootstrapWitnesses n
    = (if n > 0
        then sizeOf_Array + sizeOf_SmallUInt
        else 0)
    + sizeOf_BootstrapWitness * (TxSize n)

-- vkeywitness =
--  [ $vkey
--  , $signature
--  ]
sizeOf_VKeyWitness :: TxSize
sizeOf_VKeyWitness
    = sizeOf_SmallArray
    + sizeOf_VKey
    + sizeOf_Signature

-- bootstrap_witness =
--  [ public_key : $vkey
--  , signature  : $signature
--  , chain_code : bytes .size 32
--  , attributes : bytes
--  ]
sizeOf_BootstrapWitness :: TxSize
sizeOf_BootstrapWitness
    = sizeOf_SmallArray
    + sizeOf_VKey
    + sizeOf_Signature
    + sizeOf_ChainCode
    + sizeOf_Attributes
  where
    sizeOf_ChainCode  = 34
    sizeOf_Attributes = 45 -- NOTE: could be smaller by ~34 for Icarus

-- native_script =
--   [ script_pubkey      = (0, addr_keyhash)
--   // script_all        = (1, [ * native_script ])
--   // script_any        = (2, [ * native_script ])
--   // script_n_of_k     = (3, n: uint, [ * native_script ])
--   // invalid_before    = (4, uint)
--      ; Timelock validity intervals are half-open intervals [a, b).
--      ; This field specifies the left (included) endpoint a.
--   // invalid_hereafter = (5, uint)
--      ; Timelock validity intervals are half-open intervals [a, b).
--      ; This field specifies the right (excluded) endpoint b.
--   ]
sizeOf_NativeScript :: Script object -> TxSize
sizeOf_NativeScript = \case
    RequireSignatureOf _ ->
        sizeOf_SmallUInt + sizeOf_Hash28
    RequireAllOf ss ->
        sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
    RequireAnyOf ss ->
        sizeOf_SmallUInt + sizeOf_Array + sumVia sizeOf_NativeScript ss
    RequireSomeOf _ ss ->
        sizeOf_SmallUInt
            + sizeOf_UInt
            + sizeOf_Array
            + sumVia sizeOf_NativeScript ss
    ActiveFromSlot _ ->
        sizeOf_SmallUInt + sizeOf_UInt
    ActiveUntilSlot _ ->
        sizeOf_SmallUInt + sizeOf_UInt

-- A Blake2b-224 hash, resulting in a 28-byte digest wrapped in CBOR, so
-- with 2 bytes overhead (length <255, but length > 23)
sizeOf_Hash28 :: TxSize
sizeOf_Hash28
    = 30

-- A Blake2b-256 hash, resulting in a 32-byte digest wrapped in CBOR, so
-- with 2 bytes overhead (length <255, but length > 23)
sizeOf_Hash32 :: TxSize
sizeOf_Hash32
    = 34

-- A 32-byte Ed25519 public key, encoded as a CBOR-bytestring so with 2
-- bytes overhead (length < 255, but length > 23)
sizeOf_VKey :: TxSize
sizeOf_VKey
    = 34

-- A 64-byte Ed25519 signature, encoded as a CBOR-bytestring so with 2
-- bytes overhead (length < 255, but length > 23)
sizeOf_Signature :: TxSize
sizeOf_Signature
    = 66

-- A CBOR UInt which is less than 23 in value fits on a single byte. Beyond,
-- the first byte is used to encode the number of bytes necessary to encode
-- the number itself, followed by the number itself.
--
-- When considering a 'UInt', we consider the worst case scenario only where
-- the uint is encoded over 4 bytes, so up to 2^32 which is fine for most
-- cases but coin values.
sizeOf_SmallUInt :: TxSize
sizeOf_SmallUInt = 1

sizeOf_UInt :: TxSize
sizeOf_UInt = 5

sizeOf_LargeUInt :: TxSize
sizeOf_LargeUInt = 9

-- A CBOR array with less than 23 elements, fits on a single byte, followed
-- by each key-value pair (encoded as two concatenated CBOR elements).
sizeOf_SmallMap :: TxSize
sizeOf_SmallMap = 1

-- A CBOR array with less than 23 elements, fits on a single byte, followed
-- by each elements. Otherwise, the length of the array is encoded first,
-- very much like for UInt.
--
-- When considering an 'Array', we consider large scenarios where arrays can
-- have up to 65536 elements.
sizeOf_SmallArray :: TxSize
sizeOf_SmallArray = 1

sizeOf_Array :: TxSize
sizeOf_Array = 3

-- Small helper function for summing values. Given a list of values, get the sum
-- of the values, after the given function has been applied to each value.
sumVia :: (Foldable t, Num m) => (a -> m) -> t a -> m
sumVia f = F.foldl' (\t -> (t +) . f) 0

mkLedgerTxOut
    :: HasCallStack
    => RecentEra era
    -> W.Address
    -> TokenBundle
    -> TxOut (ShelleyLedgerEra era)
mkLedgerTxOut txOutEra address bundle =
    case txOutEra of
        RecentEraBabbage -> W.toBabbageTxOut txOut
        RecentEraConway -> W.toConwayTxOut txOut
      where
        txOut = W.TxOut address bundle
