{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to types and functions exported by the
-- ledger specification.
--
module Cardano.Wallet.Shelley.Compatibility.Ledger
    (
      -- * Conversions from wallet types to ledger specification types
      toLedgerCoin
    , toLedgerTokenBundle
    , toLedgerTokenPolicyId
    , toLedgerTokenName
    , toLedgerTokenQuantity

      -- * Conversions from ledger specification types to wallet types
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletTokenName
    , toWalletTokenQuantity
    , toWalletScript
    , toWalletScriptFromShelley

      -- * Roundtrip conversion between wallet types and ledger specification
      --   types
    , Convert (..)

      -- * Conversions for transaction outputs
    , toShelleyTxOut
    , toAllegraTxOut
    , toMaryTxOut
    , toAlonzoTxOut
    , toBabbageTxOut
    , toConwayTxOut
    , fromBabbageTxOut
    , fromConwayTxOut
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Crypto.Hash
    ( hashFromBytes, hashToBytes )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Data.ByteString.Short
    ( fromShort, toShort )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra, StandardAlonzo, StandardBabbage, StandardConway,
    StandardCrypto, StandardMary, StandardShelley )

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Scripts
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as LCore
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Ouroboros.Network.Block as O

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

toWalletCoin :: Ledger.Coin -> Coin
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
    (Ledger.Coin ledgerAda) = toLedgerCoin $ TokenBundle.getCoin bundle
    ledgerTokens :: Ledger.MultiAsset StandardCrypto
    ledgerTokens = bundle
        & view #tokens
        & TokenMap.toNestedMap
        & Map.mapKeys toLedgerTokenPolicyId
        & Map.map mapInner
        & Ledger.MultiAsset
    mapInner inner = inner
        & NonEmptyMap.toMap
        & Map.mapKeys toLedgerTokenName
        & Map.map toLedgerTokenQuantity

toWalletTokenBundle :: Ledger.MaryValue StandardCrypto -> TokenBundle
toWalletTokenBundle
    (Ledger.MaryValue ledgerAda (Ledger.MultiAsset ledgerTokens)) =
        TokenBundle.fromNestedMap (walletAda, walletTokens)
  where
    walletAda = toWalletCoin $ Ledger.Coin ledgerAda
    walletTokens = ledgerTokens
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
    mapInner inner = inner
        & Map.mapKeys toWalletTokenName
        & Map.map toWalletTokenQuantity

--------------------------------------------------------------------------------
-- Conversions for 'TokenName'
--------------------------------------------------------------------------------

instance Convert TokenName Ledger.AssetName where
    toLedger = toLedgerTokenName
    toWallet = toWalletTokenName

toLedgerTokenName :: TokenName -> Ledger.AssetName
toLedgerTokenName (UnsafeTokenName bytes) =
    Ledger.AssetName $ toShort bytes

toWalletTokenName :: Ledger.AssetName -> TokenName
toWalletTokenName (Ledger.AssetName bytes) =
    UnsafeTokenName $ fromShort bytes

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
    toLedger (Address bytes ) = case Ledger.deserialiseAddr bytes of
        Just addr -> addr
        Nothing -> error $ unwords
            [ "toLedger @Address: Invalid address:"
            , pretty (Address bytes)
            ]
    toWallet = Address . Ledger.serialiseAddr

--------------------------------------------------------------------------------
-- Conversions for 'TxOut'
--------------------------------------------------------------------------------

toShelleyTxOut
    :: TxOut
    -> Shelley.ShelleyTxOut StandardShelley
toShelleyTxOut (TxOut addr bundle) =
    Shelley.ShelleyTxOut (toLedger addr) (toLedger (TokenBundle.coin bundle))

toAllegraTxOut
    :: TxOut
    -> Shelley.ShelleyTxOut StandardAllegra
toAllegraTxOut (TxOut addr bundle) =
    Shelley.ShelleyTxOut (toLedger addr) (toLedger (TokenBundle.coin bundle))

toMaryTxOut
    :: TxOut
    -> Shelley.ShelleyTxOut StandardMary
toMaryTxOut (TxOut addr bundle) =
    Shelley.ShelleyTxOut (toLedger addr) (toLedger bundle)

toAlonzoTxOut
    :: TxOut
    -> Alonzo.AlonzoTxOut StandardAlonzo
toAlonzoTxOut (TxOut addr bundle) =
    Alonzo.AlonzoTxOut
        (toLedger addr)
        (toLedger bundle)
        Ledger.SNothing

toBabbageTxOut
    :: TxOut
    -> Babbage.BabbageTxOut StandardBabbage
toBabbageTxOut (TxOut addr bundle) =
    Babbage.BabbageTxOut
        (toLedger addr)
        (toLedger bundle)
        Babbage.NoDatum
        Ledger.SNothing

toConwayTxOut
    :: TxOut
    -> Babbage.BabbageTxOut StandardConway
toConwayTxOut (TxOut addr bundle) =
    Babbage.BabbageTxOut
        (toLedger addr)
        (toLedger bundle)
        Babbage.NoDatum
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
    fromLedgerScript (Scripts.RequireTimeExpire (O.SlotNo slot)) =
        ActiveUntilSlot $ fromIntegral slot
    fromLedgerScript (Scripts.RequireTimeStart (O.SlotNo slot)) =
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
