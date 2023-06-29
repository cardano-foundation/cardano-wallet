{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- For a UTxO to be considered a suitable collateral input, it must:
--    - Be a pure ADA UTxO (no tokens)
--    - Require a verification key witness to be spent
--    - Not be locked by a script
--
-- UTxOs of this kind are sometimes referred to as "VK" inputs.
module Cardano.Wallet.Primitive.Collateral
    ( -- * Data types
      AddressType (..)
    , Credential (..)

      -- * Classifying address types
    , asCollateral
    , addressSuitableForCollateral
    , addressTypeSuitableForCollateral

      -- * Reading address types
    , addressTypeFromHeaderNibble
    , getAddressType
    , addressType

      -- * Writing address types
    , addressTypeToHeaderNibble
    , putAddressType
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Data.Word
    ( Word8
    )
import Data.Word.Odd
    ( Word4
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as BL

-- In the realm of cardano-ledger-specs, we recognize the following types of
-- addresses:
--   (see https://hydra.iohk.io/build/6752483/download/1/ledger-spec.pdf):
--

-- | Address type       | Payment Credential | Stake Credential | Header, first nibble |
-- |--------------------+--------------------+------------------+----------------------|
-- | Base address       | keyhash            | keyhash          |                 0000 |
-- |                    | scripthash         | keyhash          |                 0001 |
-- |                    | keyhash            | scripthash       |                 0010 |
-- |                    | scripthash         | scripthash       |                 0011 |
-- | Pointer address    | keyhash            | ptr              |                 0100 |
-- |                    | scripthash         | ptr              |                 0101 |
-- | Enterprise address | keyhash            | -                |                 0110 |
-- |                    | scripthash         | 0                |                 0111 |
-- | Bootstrap address  | keyhash            | -                |                 1000 |
-- | Stake address      | -                  | keyhash          |                 1110 |
-- |                    | -                  | scripthash       |                 1111 |
-- | Future formats     | ?                  | ?                |            1001-1101 |
--
-- We represent these types of addresses with the following data types:

-- | The type of the address.
data AddressType
    = BaseAddress Credential Credential
    | PointerAddress Credential
    | EnterpriseAddress Credential
    | StakeAddress Credential
    | -- | A Bootstrap (a.k.a. Byron) address
      BootstrapAddress
    deriving (Eq, Show)

-- | The type of the credential used in an address.
data Credential
    = CredentialKeyHash
    | CredentialScriptHash
    deriving (Eq, Show)

-- To parse the address type, we can inspect the first four bits (nibble) of the
-- address:

-- | Construct an @AddressType@ from the binary representation.
addressTypeFromHeaderNibble :: Word4 -> Maybe AddressType
addressTypeFromHeaderNibble = \case
    0b0000 -> Just (BaseAddress CredentialKeyHash CredentialKeyHash)
    0b0001 -> Just (BaseAddress CredentialScriptHash CredentialKeyHash)
    0b0010 -> Just (BaseAddress CredentialKeyHash CredentialScriptHash)
    0b0011 -> Just (BaseAddress CredentialScriptHash CredentialScriptHash)
    0b0100 -> Just (PointerAddress CredentialKeyHash)
    0b0101 -> Just (PointerAddress CredentialScriptHash)
    0b0110 -> Just (EnterpriseAddress CredentialKeyHash)
    0b0111 -> Just (EnterpriseAddress CredentialScriptHash)
    0b1000 -> Just (BootstrapAddress)
    0b1110 -> Just (StakeAddress CredentialKeyHash)
    0b1111 -> Just (StakeAddress CredentialScriptHash)
    _ -> Nothing

-- | Get an AddressType from a binary stream.
getAddressType :: B.Get AddressType
getAddressType = do
    headerAndNetwork <- B.getWord8
    let headerNibble =
            fromIntegral @Word8 @Word4 (headerAndNetwork `Bits.shiftR` 4)
    maybe
        (fail "Unknown address type.")
        (pure)
        (addressTypeFromHeaderNibble headerNibble)

-- For testing and other purposes, it is also helpful to have a way of writing
-- the AddressType back to a binary stream.

-- | Return the binary representation of an @AddressType@.
addressTypeToHeaderNibble :: AddressType -> Word4
addressTypeToHeaderNibble = \case
    BaseAddress CredentialKeyHash CredentialKeyHash -> 0b0000
    BaseAddress CredentialScriptHash CredentialKeyHash -> 0b0001
    BaseAddress CredentialKeyHash CredentialScriptHash -> 0b0010
    BaseAddress CredentialScriptHash CredentialScriptHash -> 0b0011
    PointerAddress CredentialKeyHash -> 0b0100
    PointerAddress CredentialScriptHash -> 0b0101
    EnterpriseAddress CredentialKeyHash -> 0b0110
    EnterpriseAddress CredentialScriptHash -> 0b0111
    BootstrapAddress -> 0b1000
    StakeAddress CredentialKeyHash -> 0b1110
    StakeAddress CredentialScriptHash -> 0b1111

-- | Write an AddressType to a binary stream.
putAddressType :: AddressType -> B.Put
putAddressType t =
    B.putWord8
        $ fromIntegral @Word4 @Word8 (addressTypeToHeaderNibble t) `Bits.shiftL` 4

-- | Indicates whether or not the given address is suitable for collateral.
addressSuitableForCollateral :: Address -> Bool
addressSuitableForCollateral =
    maybe False addressTypeSuitableForCollateral . addressType

-- By inspecting the bit pattern of an Address, we can determine its address
-- type.

-- | Get the address type of a given address.
addressType :: Address -> Maybe AddressType
addressType (Address bytes) =
    case B.runGetOrFail getAddressType (BL.fromStrict bytes) of
        Left _ ->
            Nothing
        Right (_, _, addrType) ->
            Just addrType

-- The funds associated with an address are considered suitable for use as
-- collateral iff the payment credential column of that address is "key hash".

-- | A simple function which determines if an @AddressType@ is suitable for use
-- as collateral. Only @AddressType@s with a "key hash" payment credential are
-- considered suitable for use as collateral.
addressTypeSuitableForCollateral :: AddressType -> Bool
addressTypeSuitableForCollateral = \case
    BaseAddress CredentialKeyHash CredentialKeyHash -> True
    BaseAddress CredentialKeyHash CredentialScriptHash -> True
    BaseAddress CredentialScriptHash CredentialKeyHash -> False
    BaseAddress CredentialScriptHash CredentialScriptHash -> False
    PointerAddress CredentialKeyHash -> True
    PointerAddress CredentialScriptHash -> False
    EnterpriseAddress CredentialKeyHash -> True
    EnterpriseAddress CredentialScriptHash -> False
    StakeAddress CredentialKeyHash -> False
    StakeAddress CredentialScriptHash -> False
    BootstrapAddress -> True

-- | If the given @TxOut@ represents a UTxO that is suitable for use as
-- a collateral input, returns @Just@ along with the total ADA value of the
-- UTxO. Otherwise returns @Nothing@ if it is not a suitable collateral value.
asCollateral
    :: TxOut
    -- ^ TxOut from a UTxO entry
    -> Maybe Coin
    -- ^ The total ADA value of that UTxO if it is suitable for collateral,
    -- otherwise Nothing.
asCollateral txOut
    | addressSuitableForCollateral (address txOut) =
        TokenBundle.toCoin (tokens txOut)
    | otherwise =
        Nothing
