{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.SendFaucetAssets
    ( SendFaucetAssets (..)
    , WithNetwork (..)
    , AssetMetadata
    , genSendFaucetAssets
    )
where

import Cardano.Wallet.Address.Encoding
    ( decodeAddress
    , encodeAddress
    )
import Cardano.Wallet.Faucet.Gen.Address
    ( NetworkTag (..)
    , genAddress
    )
import Cardano.Wallet.Launch.Cluster.Http.Faucet.OpenApi
    ( sendAssetsSchema
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (sNetworkId)
    , NetworkDiscriminant (..)
    , SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle
    , fromFlatList
    , toFlatList
    )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Monad
    ( (>=>)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value
    , object
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Bifunctor
    ( first
    )
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema (..)
    )
import Data.Typeable
    ( Typeable
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , listOf
    )
import Prelude

import qualified Cardano.Address as Addr

type AssetMetadata = [(String, String)]

-- | Payload to send assets to a list of addresses
data SendFaucetAssets = SendFaucetAssets
    { batchSize :: Int
    -- ^ batch size
    , assets :: [(Address, (TokenBundle, AssetMetadata))]
    -- ^ List of addresses and the assets to send to each address
    }
    deriving stock (Eq, Show)

-- | WithNetwork carries network discriminant around a value
newtype WithNetwork a (n :: NetworkDiscriminant) = WithNetwork a
    deriving stock (Eq, Show)

instance HasSNetworkId n => ToJSON (WithNetwork SendFaucetAssets n) where
    toJSON (WithNetwork SendFaucetAssets{batchSize, assets}) =
        object
            [ "batch-size" .= batchSize
            , "assets" .= renderAssets (sNetworkId @n) assets
            ]

instance Typeable n => ToSchema (WithNetwork SendFaucetAssets n) where
    declareNamedSchema _ =
        pure
            $ NamedSchema
                (Just "WithNetwork SendFaucetAssets")
                sendAssetsSchema

instance HasSNetworkId n => FromJSON (WithNetwork SendFaucetAssets n) where
    parseJSON = withObject "SendFaucetAssets" $ \o -> do
        batchSize <- o .: "batch-size"
        assets <- o .: "assets" >>= parseAssets (sNetworkId @n)
        pure $ WithNetwork $ SendFaucetAssets{batchSize, assets}

--- assets parsing/rendering ---------------------------------------------------

parseAssets
    :: SNetworkId n
    -> Value
    -> Parser [(Address, (TokenBundle, AssetMetadata))]
parseAssets n = parseJSON >=> mapM (parseAsset n)

parseAsset
    :: SNetworkId n
    -> Value
    -> Parser (Address, (TokenBundle, AssetMetadata))
parseAsset n = withObject "Asset" $ \o -> do
    addr <- o .: "address" >>= parseAddress n
    bundle <- o .: "bundle" >>= parseBundle
    metadata <- o .: "metadata" >>= parseAssetMetadata
    pure (addr, (bundle, metadata))

renderAssets
    :: SNetworkId n
    -> [(Address, (TokenBundle, AssetMetadata))]
    -> Value
renderAssets n = toJSON . map (renderAsset n)

renderAsset
    :: SNetworkId n
    -> (Address, (TokenBundle, AssetMetadata))
    -> Value
renderAsset n (addr, (bundle, metadata)) =
    object
        [ "address" .= renderAddress n addr
        , "bundle" .= renderBundle bundle
        , "metadata" .= renderAssetMetadata metadata
        ]

parseAssetMetadata :: Value -> Parser AssetMetadata
parseAssetMetadata = parseJSON >=> mapM parseMetadataValue

parseMetadataValue :: Value -> Parser (String, String)
parseMetadataValue = withObject "MetadataValue" $ \o -> do
    k <- o .: "key"
    v <- o .: "value"
    pure (k, v)

renderAssetMetadata :: AssetMetadata -> Value
renderAssetMetadata =
    toJSON
        . map (\(k, v) -> object ["key" .= k, "value" .= v])

-- address parsing/rendering ---------------------------------------------------

renderAddress :: SNetworkId n -> Address -> Value
renderAddress n = toJSON . encodeAddress n

parseAddress :: SNetworkId n -> Value -> Parser Address
parseAddress n x = do
    parseJSON x
        >>= eitherToParser
            . first (\e -> ShowFmt $ show (x, e))
            . decodeAddress n

eitherToParser :: Show s => Either s a -> Parser a
eitherToParser = either (fail . show) pure

--- bundle parsing/rendering ---------------------------------------------------

parseBundle :: Value -> Parser TokenBundle
parseBundle = parseJSON >=> fmap (uncurry fromFlatList) . parseBundle'

parseBundle' :: Value -> Parser (Coin, [(AssetId, TokenQuantity)])
parseBundle' = withObject "Bundle" $ \o -> do
    c <- o .: "coin" >>= parseCoin
    xs <- o .: "assets" >>= mapM parseAssetQuantity
    pure (c, xs)

parseCoin :: Value -> Parser Coin
parseCoin = fmap Coin . parseJSON

parseAssetQuantity :: Value -> Parser (AssetId, TokenQuantity)
parseAssetQuantity = withObject "AssetQuantity" $ \o -> do
    asset <- o .: "asset" >>= parseAssetId
    quantity <- o .: "quantity"
    pure (asset, quantity)

parseAssetId :: Value -> Parser AssetId
parseAssetId = withObject "AssetId" $ \o -> do
    tp <- o .: "policy"
    n <- o .: "name"
    pure $ AssetId tp n

renderBundle :: TokenBundle -> Value
renderBundle = toJSON . renderBundle' . toFlatList

renderBundle' :: (Coin, [(AssetId, TokenQuantity)]) -> Value
renderBundle' (c, xs) =
    object
        [ "coin" .= renderCoin c
        , "assets" .= map renderAssetQuantity xs
        ]

renderAssetQuantity :: (AssetId, TokenQuantity) -> Value
renderAssetQuantity (AssetId tp n, tq) =
    object
        [ "asset"
            .= object
                [ "policy" .= tp
                , "name" .= n
                ]
        , "quantity" .= tq
        ]

renderCoin :: Coin -> Value
renderCoin (Coin c) = toJSON c

-- | Generate a 'SendFaucetAssets' payload
genSendFaucetAssets
    :: forall n
     . HasSNetworkId n
    => Gen (WithNetwork SendFaucetAssets n)
genSendFaucetAssets = do
    batchSize <- arbitrary
    assets <- listOf $ genAsset $ case sNetworkId @n of
        SMainnet -> MainnetTag
        STestnet _ -> TestnetTag
    pure $ WithNetwork SendFaucetAssets{batchSize, assets}

genAsset
    :: NetworkTag -> Gen (Address, (TokenBundle, [(String, String)]))
genAsset tag = do
    addr <- Address . Addr.unAddress <$> genAddress [tag]
    bundle <- genTokenBundleSmallRange
    metadata <- listOf ((,) <$> arbitrary <*> arbitrary)
    pure (addr, (bundle, metadata))
