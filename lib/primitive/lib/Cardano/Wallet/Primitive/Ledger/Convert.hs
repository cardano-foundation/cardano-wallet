{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to types and functions exported by the
-- ledger specification.
--
module Cardano.Wallet.Primitive.Ledger.Convert
    (
      -- * Conversions from wallet types to ledger specification types
      toLedgerAddress
    , toLedgerCoin
    , toLedgerTokenBundle
    , toLedgerTokenPolicyId
    , toLedgerAssetName
    , toLedgerTokenQuantity
    , toLedgerTimelockScript
    , toLedgerDelegatee
    , toLedgerDRep
    , toPlutusScriptInfo

      -- * Conversions from ledger specification types to wallet types
    , toWalletAddress
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletAssetName
    , toWalletTokenQuantity
    , toWalletScript
    , toWalletScriptFromShelley

      -- * Roundtrip conversion between wallet types and ledger specification
      --   types
    , Convert (..)

      -- * Conversions for transaction outputs in recent eras
    , toBabbageTxOut
    , toConwayTxOut
    , fromBabbageTxOut
    , fromConwayTxOut
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (..)
    , Script (..)
    )
import Cardano.Crypto.Hash
    ( hashFromBytes
    , hashToBytes
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenMapWithScripts
    ( PlutusVersion (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Data.ByteString.Short
    ( fromShort
    , toShort
    )
import Data.Foldable
    ( toList
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Maybe
    ( fromMaybe
    )
import Fmt
    ( pretty
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage
    , StandardConway
    , StandardCrypto
    )

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Scripts
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway.TxCert as Conway
import qualified Cardano.Ledger.Core as LCore
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Plutus.Language as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq

--------------------------------------------------------------------------------
-- Roundtrip conversion between wallet types and ledger specification types
--------------------------------------------------------------------------------

-- | Connects a wallet type with its equivalent ledger specification type.
--
-- Instances of this class should satisfy the following laws:
--
-- >>> toLedger . toWallet == id
-- >>> toWallet . toLedger == id
--
class Convert wallet ledger | wallet -> ledger where
    -- | Converts a value from a wallet type to the equivalent ledger
    --   specification type.
    toLedger
        :: HasCallStack => wallet -> ledger
    -- | Converts a value from a ledger specification type to the equivalent
    --   wallet type.
    toWallet
        :: HasCallStack => ledger -> wallet

--------------------------------------------------------------------------------
-- Conversions for 'Coin'
--------------------------------------------------------------------------------

instance Convert Coin Ledger.Coin where
    toLedger = toLedgerCoin
    toWallet = toWalletCoin

toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin (Coin c) = Ledger.Coin $ intCast @Natural @Integer c

toWalletCoin :: HasCallStack => Ledger.Coin -> Coin
toWalletCoin (Ledger.Coin c) = Coin.unsafeFromIntegral c

--------------------------------------------------------------------------------
-- Conversions for 'TokenBundle'
--------------------------------------------------------------------------------

-- Values of the ledger specification's 'Value' type are constructed in a way
-- that is similar to the wallet's 'TokenBundle' type. The ada quantity is
-- stored as a separate value, and asset quantities are stored in a nested map.

instance Convert TokenBundle (Ledger.MaryValue StandardCrypto) where
    toLedger = toLedgerTokenBundle
    toWallet = toWalletTokenBundle

toLedgerTokenBundle :: TokenBundle -> Ledger.MaryValue StandardCrypto
toLedgerTokenBundle bundle =
    Ledger.MaryValue ledgerAda ledgerTokens
  where
    ledgerAda = toLedgerCoin $ TokenBundle.getCoin bundle
    ledgerTokens :: Ledger.MultiAsset StandardCrypto
    ledgerTokens = bundle
        & view #tokens
        & TokenMap.toNestedMap
        & Map.mapKeys toLedgerTokenPolicyId
        & Map.map mapInner
        & Ledger.MultiAsset
    mapInner inner = inner
        & Map.mapKeys toLedgerAssetName
        & Map.map toLedgerTokenQuantity

toWalletTokenBundle :: Ledger.MaryValue StandardCrypto -> TokenBundle
toWalletTokenBundle
    (Ledger.MaryValue ledgerAda (Ledger.MultiAsset ledgerTokens)) =
        TokenBundle.fromNestedMap (walletAda, walletTokens)
  where
    walletAda = toWalletCoin ledgerAda
    walletTokens = ledgerTokens
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
    mapInner inner = inner
        & Map.mapKeys toWalletAssetName
        & Map.map toWalletTokenQuantity

--------------------------------------------------------------------------------
-- Conversions for 'AssetName'
--------------------------------------------------------------------------------

instance Convert AssetName Ledger.AssetName where
    toLedger = toLedgerAssetName
    toWallet = toWalletAssetName

toLedgerAssetName :: AssetName -> Ledger.AssetName
toLedgerAssetName (UnsafeAssetName bytes) =
    Ledger.AssetName $ toShort bytes

toWalletAssetName :: Ledger.AssetName -> AssetName
toWalletAssetName (Ledger.AssetName bytes) =
    UnsafeAssetName $ fromShort bytes

--------------------------------------------------------------------------------
-- Conversions for 'TokenPolicyId'
--------------------------------------------------------------------------------

instance Convert TokenPolicyId (Ledger.PolicyID StandardCrypto) where
    toLedger = toLedgerTokenPolicyId
    toWallet = toWalletTokenPolicyId

toLedgerTokenPolicyId :: TokenPolicyId -> Ledger.PolicyID StandardCrypto
toLedgerTokenPolicyId p@(UnsafeTokenPolicyId (Hash bytes)) =
    case hashFromBytes bytes of
        Just hash ->
            Ledger.PolicyID (Ledger.ScriptHash hash)
        Nothing ->
            error $ unwords
                [ "Ledger.toLedgerTokenPolicyId"
                , "Unable to construct hash for token policy:"
                , pretty p
                ]

toWalletTokenPolicyId :: Ledger.PolicyID StandardCrypto -> TokenPolicyId
toWalletTokenPolicyId (Ledger.PolicyID (Ledger.ScriptHash hash)) =
    UnsafeTokenPolicyId (Hash (hashToBytes hash))

--------------------------------------------------------------------------------
-- Conversions for 'TokenQuantity'
--------------------------------------------------------------------------------

instance Convert TokenQuantity Integer where
    toLedger = toLedgerTokenQuantity
    toWallet = toWalletTokenQuantity

toLedgerTokenQuantity :: TokenQuantity -> Integer
toLedgerTokenQuantity (TokenQuantity q) = fromIntegral q

toWalletTokenQuantity :: Integer -> TokenQuantity
toWalletTokenQuantity q
    | q >= 0 =
        TokenQuantity $ fromIntegral q
    | otherwise =
        error $ unwords
            [ "Ledger.toWalletTokenQuantity:"
            , "Unexpected negative value:"
            , pretty q
            ]

--------------------------------------------------------------------------------
-- Conversions for 'TxIn'
--------------------------------------------------------------------------------

instance Convert TxIn (Ledger.TxIn StandardCrypto) where
    toLedger (TxIn tid ix) =
        Ledger.TxIn (toLedgerHash tid) (toEnum $ intCast ix)
      where
        toLedgerHash (Hash h) =
            Ledger.TxId
                $ SafeHash.unsafeMakeSafeHash
                $ Crypto.UnsafeHash
                $ toShort h

    toWallet (Ledger.TxIn (Ledger.TxId tid) ix) =
        TxIn (convertId tid) (convertIx ix)
      where
        convertId = Hash . Crypto.hashToBytes . SafeHash.extractHash

        convertIx = fromMaybe err . intCastMaybe . fromEnum
          where
            err = error $ unwords
                [ "Ledger.toWallet @TxIn:"
                , "Unexpected out of bounds TxIx"
                , show ix
                ]

--------------------------------------------------------------------------------
-- Conversions for 'Address'
--------------------------------------------------------------------------------

instance Convert Address (Ledger.Addr StandardCrypto) where
    toLedger = toLedgerAddress
    toWallet = toWalletAddress

toLedgerAddress :: Address -> Ledger.Addr StandardCrypto
toLedgerAddress (Address bytes) = case Ledger.decodeAddrLenient bytes of
    Just addr -> addr
    Nothing -> error $ unwords
        [ "toLedger @Address: Invalid address:"
        , pretty (Address bytes)
        ]

toWalletAddress :: Ledger.Addr StandardCrypto -> Address
toWalletAddress = Address . Ledger.serialiseAddr

--------------------------------------------------------------------------------
-- Conversions for 'TxOut' (in recent eras)
--------------------------------------------------------------------------------

toBabbageTxOut
    :: HasCallStack
    => TxOut
    -> Babbage.BabbageTxOut StandardBabbage
toBabbageTxOut (TxOut addr bundle) =
    Babbage.BabbageTxOut
        (toLedger addr)
        (toLedger bundle)
        Ledger.NoDatum
        Ledger.SNothing

toConwayTxOut
    :: TxOut
    -> Babbage.BabbageTxOut StandardConway
toConwayTxOut (TxOut addr bundle) =
    Babbage.BabbageTxOut
        (toLedger addr)
        (toLedger bundle)
        Ledger.NoDatum
        Ledger.SNothing

-- NOTE: Inline scripts and datums will be lost in the conversion.
fromConwayTxOut
    :: Babbage.BabbageTxOut StandardConway
    -> TxOut
fromConwayTxOut (Babbage.BabbageTxOut addr val _ _)
    = TxOut (toWallet addr) (toWallet val)

-- NOTE: Inline scripts and datums will be lost in the conversion.
fromBabbageTxOut
    :: Babbage.BabbageTxOut StandardBabbage
    -> TxOut
fromBabbageTxOut (Babbage.BabbageTxOut addr val _ _)
    = TxOut (toWallet addr) (toWallet val)

toWalletScript
    :: LCore.Era crypto
    => (Hash "VerificationKey" -> KeyRole)
    -> Scripts.Timelock crypto
    -> Script KeyHash
toWalletScript tokeyrole = fromLedgerScript
  where
    fromLedgerScript (Scripts.RequireSignature (Ledger.KeyHash h)) =
        let payload = hashToBytes h
        in RequireSignatureOf (KeyHash (tokeyrole (Hash payload)) payload)
    fromLedgerScript (Scripts.RequireAllOf contents) =
        RequireAllOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (Scripts.RequireAnyOf contents) =
        RequireAnyOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (Scripts.RequireMOf num contents) =
        RequireSomeOf (fromIntegral num) $ fromLedgerScript <$> toList contents
    fromLedgerScript (Scripts.RequireTimeExpire (SlotNo slot)) =
        ActiveUntilSlot $ fromIntegral slot
    fromLedgerScript (Scripts.RequireTimeStart (SlotNo slot)) =
        ActiveFromSlot $ fromIntegral slot

toWalletScriptFromShelley
    :: LCore.Era crypto
    => KeyRole
    -> Ledger.MultiSig crypto
    -> Script KeyHash
toWalletScriptFromShelley keyrole = fromLedgerScript'
  where
    fromLedgerScript' (Ledger.RequireSignature (Ledger.KeyHash h)) =
        RequireSignatureOf (KeyHash keyrole (hashToBytes h))
    fromLedgerScript' (Ledger.RequireAllOf contents) =
        RequireAllOf $ map fromLedgerScript' $ toList contents
    fromLedgerScript' (Ledger.RequireAnyOf contents) =
        RequireAnyOf $ map fromLedgerScript' $ toList contents
    fromLedgerScript' (Ledger.RequireMOf num contents) =
        RequireSomeOf (fromIntegral num) $ fromLedgerScript' <$> toList contents

toLedgerTimelockScript
    :: LCore.Era era
    => Script KeyHash
    -> Scripts.Timelock era
toLedgerTimelockScript s = case s of
    RequireSignatureOf (KeyHash _ keyhash) ->
        case hashFromBytes keyhash of
            Just h -> Scripts.RequireSignature (Ledger.KeyHash h)
            Nothing -> error "Hash key not valid"
    RequireAllOf contents ->
        Scripts.RequireAllOf
        $ StrictSeq.fromList
        $ map toLedgerTimelockScript contents
    RequireAnyOf contents ->
        Scripts.RequireAnyOf
        $ StrictSeq.fromList
        $ map toLedgerTimelockScript contents
    RequireSomeOf num contents ->
        Scripts.RequireMOf (intCast num)
        $ StrictSeq.fromList
        $ map toLedgerTimelockScript contents
    ActiveUntilSlot slot ->
        Scripts.RequireTimeExpire
        (convertSlotNo slot)
    ActiveFromSlot slot ->
        Scripts.RequireTimeStart
        (convertSlotNo slot)
  where
    convertSlotNo :: Natural -> SlotNo
    convertSlotNo x = SlotNo $ fromMaybe err $ intCastMaybe x
      where
        err = error $ unwords
            [ "toLedgerTimelockScript:"
            , "Unexpected out of bounds SlotNo"
            , show x
            ]

toLedgerDelegatee
    :: Maybe PoolId
    -> Maybe DRep
    -> Conway.Delegatee StandardCrypto
toLedgerDelegatee poolM vaM = case (poolM, vaM) of
    (Just poolId, Nothing) ->
        Conway.DelegStake (toKeyHash poolId)
    (Nothing, Just vote) ->
        Conway.DelegVote (toLedgerDRep vote)
    (Just poolId, Just vote) ->
        Conway.DelegStakeVote (toKeyHash poolId) (toLedgerDRep vote)
    _ ->
        error "toLedgerDelegatee: wrong use, at least pool or vote action must be present"
  where
    toKeyHash (PoolId pid) = Ledger.KeyHash . Crypto.UnsafeHash $ toShort pid

toLedgerDRep
    :: DRep -> Ledger.DRep StandardCrypto
toLedgerDRep = \case
    Abstain -> Ledger.DRepAlwaysAbstain
    NoConfidence -> Ledger.DRepAlwaysNoConfidence
    FromDRepID (DRepFromKeyHash (DRepKeyHash keyhash)) ->
        Ledger.DRepCredential . Ledger.KeyHashObj . Ledger.KeyHash . Crypto.UnsafeHash $
        toShort keyhash
    FromDRepID (DRepFromScriptHash (DRepScriptHash scripthash)) ->
        Ledger.DRepCredential . Ledger.ScriptHashObj . Ledger.ScriptHash . Crypto.UnsafeHash $
        toShort scripthash

toPlutusScriptInfo
    :: forall era. Alonzo.AlonzoEraScript era
    => Alonzo.PlutusScript era
    -> PlutusVersion
toPlutusScriptInfo script = case Alonzo.plutusScriptLanguage @era script of
    Ledger.PlutusV1 -> PlutusVersionV1
    Ledger.PlutusV2 -> PlutusVersionV2
    Ledger.PlutusV3 -> PlutusVersionV3
