{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Api.Types.Transaction
    ( AddressAmount (..)
    , ApiAssetMintBurn
    , ApiDecodedTransaction (..)
    , ApiPostPolicyKeyData (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutput
    , ApiTxOutputGeneral (..)
    , ApiWalletInput (..)
    , ApiWalletOutput (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalGeneral (..)
    , ResourceContext (..)
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.ApiAsArray
    ( ApiAsArray )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (ApiT) )
import Cardano.Wallet.Api.Lib.Options
    ( defaultRecordTypeOptions )
import Cardano.Wallet.Api.Types.Address
    ( DecodeAddress, DecodeStakeAddress, EncodeAddress, EncodeStakeAddress )
import Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate )
import Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn )
import Cardano.Wallet.Api.Types.Primitive
    ()
import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..), NetworkDiscriminant )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (unCoin), coinFromQuantity )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( coinIsValidForTxOut, txOutMaxCoin )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxIn (..), TxMetadata (..), TxScriptValidity, txMetadataIsNull )
import Cardano.Wallet.Transaction
    ( ValidityIntervalExplicit (..) )
import Control.DeepSeq
    ( NFData )
import Data.Aeson.Types
    ( FromJSON (..)
    , KeyValue (..)
    , ToJSON (..)
    , Value (..)
    , genericParseJSON
    , genericToJSON
    , object
    , prependFailure
    , withObject
    , (.!=)
    , (.:)
    , (.:?)
    )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Typeable
    ( Proxy, Typeable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Data.Aeson.Types as Aeson

newtype ApiTxMetadata = ApiTxMetadata
    { getApiTxMetadata :: Maybe (ApiT TxMetadata)
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiTxMetadata)

instance FromJSON ApiTxMetadata where
    parseJSON Aeson.Null = pure $ ApiTxMetadata Nothing
    parseJSON v = ApiTxMetadata . Just <$> parseJSON v
instance ToJSON ApiTxMetadata where
    toJSON (ApiTxMetadata x) = case x of
        Nothing -> Aeson.Null
        Just (ApiT md) | txMetadataIsNull md -> Aeson.Null
        Just md -> toJSON md

data ApiDecodedTransaction (n :: NetworkDiscriminant) = ApiDecodedTransaction
    { id :: ApiT (Hash "Tx")
    , fee :: Quantity "lovelace" Natural
    , inputs :: [ApiTxInputGeneral n]
    , outputs :: [ApiTxOutputGeneral n]
    , collateral :: [ApiTxInputGeneral n]
    , collateralOutputs ::
        ApiAsArray "collateral_outputs" (Maybe (ApiTxOutputGeneral n))
    , withdrawals :: [ApiWithdrawalGeneral n]
    , mint :: ApiAssetMintBurn
    , burn :: ApiAssetMintBurn
    , certificates :: [ApiAnyCertificate n]
    , depositsTaken :: [Quantity "lovelace" Natural]
    , depositsReturned :: [Quantity "lovelace" Natural]
    , metadata :: ApiTxMetadata
    , scriptValidity :: Maybe (ApiT TxScriptValidity)
    , validityInterval :: Maybe ValidityIntervalExplicit
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiDecodedTransaction n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiDecodedTransaction n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

data ApiWalletInput (n :: NetworkDiscriminant) = ApiWalletInput
    { id :: ApiT (Hash "Tx")
    , index :: Word32
    , address :: (ApiT Address, Proxy n)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    , amount :: Quantity "lovelace" Natural
    , assets :: ApiT W.TokenMap
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

instance DecodeAddress n => FromJSON (ApiWalletInput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletInput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

data ApiTxInputGeneral (n :: NetworkDiscriminant) =
      ExternalInput (ApiT TxIn)
    | WalletInput (ApiWalletInput n)
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiTxInputGeneral n)
  where
    parseJSON obj = do
        derPathM <-
            (withObject "ApiTxInputGeneral" $
             \o -> o .:? "derivation_path"
                :: Aeson.Parser (Maybe (NonEmpty (ApiT DerivationIndex)))) obj
        case derPathM of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser (ApiT TxIn)
                pure $ ExternalInput xs
            Just _ -> do
                xs <- parseJSON obj :: Aeson.Parser (ApiWalletInput n)
                pure $ WalletInput xs
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiTxInputGeneral n)
  where
    toJSON (ExternalInput content) = toJSON content
    toJSON (WalletInput content) = toJSON content

data ResourceContext = External | Our
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiWithdrawalGeneral (n :: NetworkDiscriminant) = ApiWithdrawalGeneral
    { stakeAddress :: (ApiT W.RewardAccount, Proxy n)
    , amount :: Quantity "lovelace" Natural
    , context :: ResourceContext
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletOutput (n :: NetworkDiscriminant) = ApiWalletOutput
    { address :: (ApiT Address, Proxy n)
    , amount :: Quantity "lovelace" Natural
    , assets :: ApiT W.TokenMap
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData
instance DecodeAddress n => FromJSON (ApiWalletOutput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletOutput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

data AddressAmount addr = AddressAmount
    { address :: addr
    , amount :: Quantity "lovelace" Natural
    , assets :: ApiT W.TokenMap
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

instance FromJSON a => FromJSON (AddressAmount a) where
    parseJSON = withObject "AddressAmount " $ \v ->
        prependFailure "parsing AddressAmount failed, " $
        AddressAmount
            <$> v .: "address"
            <*> (v .: "amount" >>= validateCoin)
            <*> v .:? "assets" .!= mempty
      where
        validateCoin q
            | coinIsValidForTxOut (coinFromQuantity q) = pure q
            | otherwise = fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (unCoin txOutMaxCoin) <> " lovelace."

instance ToJSON a => ToJSON (AddressAmount a) where
    toJSON = genericToJSON defaultRecordTypeOptions
-- | A helper type to reduce the amount of repetition.
--
type ApiTxOutput n = AddressAmount (ApiT Address, Proxy n)

data ApiTxOutputGeneral (n :: NetworkDiscriminant) =
      ExternalOutput (ApiTxOutput n)
    | WalletOutput (ApiWalletOutput n)
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiTxOutputGeneral n)
  where
    parseJSON obj = do
        derPathM <-
            (withObject "ApiTxOutputGeneral" $
             \o -> o .:? "derivation_path"
                :: Aeson.Parser (Maybe (NonEmpty (ApiT DerivationIndex)))) obj
        case derPathM of
            Nothing -> do
                xs <- parseJSON obj
                    :: Aeson.Parser (ApiTxOutput n)
                pure $ ExternalOutput xs
            Just _ -> do
                xs <- parseJSON obj
                    :: Aeson.Parser (ApiWalletOutput n)
                pure $ WalletOutput xs
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiTxOutputGeneral n)
  where
    toJSON (ExternalOutput content) = toJSON content
    toJSON (WalletOutput content) = toJSON content

newtype ApiPostPolicyKeyData = ApiPostPolicyKeyData
    { passphrase :: ApiT (Passphrase "user")
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiWithdrawal n = ApiWithdrawal
    { stakeAddress :: !(ApiT W.RewardAccount, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

instance DecodeStakeAddress n => FromJSON (ApiWithdrawal n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiWithdrawal n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiWithdrawalGeneral n) where
    parseJSON obj = do
        myResource <-
            (withObject "ApiWithdrawalGeneral" $
             \o -> o .:? "context" :: Aeson.Parser (Maybe Text)) obj
        case myResource of
            Nothing -> do
                (ApiWithdrawal addr amt)  <- parseJSON obj :: Aeson.Parser (ApiWithdrawal n)
                pure $ ApiWithdrawalGeneral addr amt External
            _ -> do
                (ApiWithdrawal addr amt)  <- parseJSON obj :: Aeson.Parser (ApiWithdrawal n)
                pure $ ApiWithdrawalGeneral addr amt Our

instance EncodeStakeAddress n => ToJSON (ApiWithdrawalGeneral n) where
    toJSON (ApiWithdrawalGeneral addr amt ctx) = do
        let obj = [ "stake_address" .= toJSON addr
                  , "amount" .= toJSON amt]
        case ctx of
            External -> object obj
            Our -> object $ obj ++ ["context" .= String "ours"]
