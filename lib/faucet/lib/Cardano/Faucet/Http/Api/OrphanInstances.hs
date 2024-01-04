{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Http.Api.OrphanInstances () where

import Prelude

import Cardano.Address
    ( NetworkTag (..)
    , fromBech32
    )
import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.Faucet.Http.Api.Utils
    ( typeName
    , typeRef
    )
import Cardano.Faucet.Mnemonics
    ( MnemonicLength (..)
    , unsafeMnemonic
    )
import Cardano.Faucet.Types
    ( AddressIndex (AddressIndex)
    , AddressStyle (..)
    , FaucetAddress (..)
    , IndexedAddress (..)
    , IndexedMnemonic (..)
    , Mnemonic (..)
    , MnemonicIndex (MnemonicIndex)
    )
import Cardano.Mnemonic.Extended
    ( SomeMnemonic (..)
    )
import Control.Lens
    ( (&)
    , (.~)
    , (?~)
    )
import Data.Aeson
    ( ToJSON (toJSON)
    , toJSON
    )
import Data.OpenApi
    ( HasDescription (description)
    , HasDescription (description)
    , HasExample (example)
    , HasExample (example)
    , HasFormat (format)
    , HasFormat (format)
    , HasMinimum (minimum_)
    , HasProperties (properties)
    , HasTitle (title)
    , HasTitle (title)
    , HasType (type_)
    , HasType (type_)
    , NamedSchema (..)
    , NamedSchema (..)
    , OpenApiType (..)
    , OpenApiType (..)
    , Referenced (Ref)
    , Schema
    , ToParamSchema
    , ToParamSchema (..)
    , ToSchema
    , ToSchema
    , declareNamedSchema
    , enum_
    , toParamSchema
    )
import GHC.Stack
    ( HasCallStack
    )
import Servant
    ( FromHttpApiData
    , ToHttpApiData
    , parseUrlPiece
    , toUrlPiece
    )

import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Text as T

--------------------------------------------------------------------------------

instance ToParamSchema NetworkTag where
    toParamSchema _proxy = mempty
        & type_ ?~ OpenApiInteger
        & format ?~ "uint32"
        & title ?~ "NetworkTag"
        & description ?~
            "Network tag is a \"magic\" constant associated with a network. \
            \It is mainly used in: \n\
            \- Address payloads, to discriminate addresses between networks.\n\
            \- At the network level, when doing handshake with nodes."
        & example ?~ toJSON shelleyTestnet

instance ToHttpApiData NetworkTag where
    toUrlPiece = T.pack . show . unNetworkTag

instance FromHttpApiData NetworkTag where
    parseUrlPiece = fmap NetworkTag . parseUrlPiece

instance ToSchema FaucetAddress where
    declareNamedSchema _proxy = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @FaucetAddress)
        , _namedSchemaSchema = mempty
            & title ?~ typeName @FaucetAddress
            & description ?~ "Cardano address of some type"
            & example ?~ toJSON exampleAddress
        }

instance ToSchema AddressStyle where
    declareNamedSchema _proxy = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @AddressStyle)
        , _namedSchemaSchema = mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["shelley", "icarus", "byron"]
            & title ?~ typeName @AddressStyle
            & description ?~ "Cardano Address style"
            & example ?~ toJSON AddressStyleShelley
        }

instance ToParamSchema AddressStyle where
    toParamSchema _proxy = mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ ["shelley", "icarus", "byron"]
        & title ?~ typeName @AddressStyle
        & description ?~ "Address style"
        & example ?~ toJSON AddressStyleShelley

instance ToSchema IndexedMnemonic where
    declareNamedSchema _proxy = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @IndexedMnemonic)
        , _namedSchemaSchema = mempty
            & title ?~ typeName @IndexedMnemonic
            & description ?~ "A natural index uniquely identifies a Mnemonic"
            & type_ ?~ OpenApiObject
            & properties .~ IOHM.fromList
                [ ("index", Ref (typeRef @MnemonicIndex))
                , ("mnemonic", Ref (typeRef @Mnemonic))
                ]
            & example ?~ toJSON exampleIndexedMnemonic
        }

instance ToSchema IndexedAddress where
    declareNamedSchema _proxy = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @IndexedAddress)
        , _namedSchemaSchema = mempty
            & title ?~ typeName @IndexedAddress
            & description ?~ "A natural index uniquely identifies an Address"
            & type_ ?~ OpenApiObject
            & properties .~ IOHM.fromList
                [ ("index", Ref (typeRef @AddressIndex))
                , ("address", Ref (typeRef @FaucetAddress))
                ]
            & example ?~ toJSON exampleIndexedAddress
        }

instance ToSchema Mnemonic where
    declareNamedSchema _ = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @Mnemonic)
        , _namedSchemaSchema = mempty
            & title ?~ typeName @Mnemonic
            & description ?~ "BIP-39 mnemonic sentence"
            & example ?~ toJSON exampleMnemonic
        }

instance ToSchema MnemonicIndex where
    declareNamedSchema _ = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @MnemonicIndex)
        , _namedSchemaSchema = mnemonicIndexSchema
        }

instance ToParamSchema MnemonicIndex where
    toParamSchema _proxy = mnemonicIndexSchema

mnemonicIndexSchema :: Schema
mnemonicIndexSchema = mempty
    & type_ ?~ OpenApiInteger
    & format ?~ "int32"
    & minimum_ ?~ 0
    & title ?~ typeName @MnemonicIndex
    & description ?~ "Index of a mnemonic in the collection"
    & example ?~ toJSON (MnemonicIndex 0)

instance ToSchema AddressIndex where
    declareNamedSchema _ = pure $ NamedSchema
        { _namedSchemaName = Just (typeName @AddressIndex)
        , _namedSchemaSchema = addressIndexSchema
        }

instance ToParamSchema AddressIndex where
    toParamSchema _proxy = addressIndexSchema

addressIndexSchema :: Schema
addressIndexSchema = mempty
    & title ?~ typeName @AddressIndex
    & description ?~ "Index of an address derived from a mnemonic."
    & type_ ?~ OpenApiInteger
    & minimum_ ?~ 0
    & format ?~ "int32"
    & example ?~ toJSON (AddressIndex 0)

deriving anyclass instance ToParamSchema MnemonicLength

instance ToHttpApiData MnemonicLength where
  toUrlPiece = \case
    M9 -> "9"
    M12 -> "12"
    M15 -> "15"
    M18 -> "18"
    M21 -> "21"
    M24 -> "24"

instance FromHttpApiData MnemonicLength where
  parseUrlPiece = \case
    "9" -> Right M9
    "12" -> Right M12
    "15" -> Right M15
    "18" -> Right M18
    "21" -> Right M21
    "24" -> Right M24
    _ -> Left "Invalid mnemonic length, must be one of: 9, 12, 15, 18, 21, 24"

--------------------------------------------------------------------------------
-- Example values --------------------------------------------------------------

exampleMnemonic :: Mnemonic
exampleMnemonic = Mnemonic . SomeMnemonic $ unsafeMnemonic @15 $
    T.words "vintage poem topic machine hazard cement dune \
    \glimpse fix brief account badge mass silly business"

exampleIndexedMnemonic :: IndexedMnemonic
exampleIndexedMnemonic = IndexedMnemonic (MnemonicIndex 42) exampleMnemonic

exampleAddress :: HasCallStack => FaucetAddress
exampleAddress = FaucetAddress $
    case fromBech32 "addr_test1wq5th50h46anh3v7zdvh7ve6\
        \amac7k4h3mdfvt0p6czm8zq7k79xv" of
        Nothing -> error "exampleAddress: failed to decode address"
        Just ad -> ad

exampleIndexedAddress :: IndexedAddress
exampleIndexedAddress = IndexedAddress (AddressIndex 42) exampleAddress
