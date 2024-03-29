{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Faucet.Yaml
    ( retrieveFunds
    , saveFunds
    ) where

import Prelude

import Cardano.Address
    ( bech32
    , fromBech32
    )
import Cardano.Wallet.Launch.Cluster.Cluster
    ( FaucetFunds (..)
    )
import Cardano.Wallet.Launch.Cluster.FileOf
    ( FileOf
    , absFilePathOf
    )
import Cardano.Wallet.Primitive.Types.AssetId
    ( AssetId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    , fromFlatList
    , toFlatList
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , Value (Number, Object, String)
    , object
    , withObject
    , (.:)
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )

import qualified Data.Text as T
import qualified Data.Yaml as Y

newtype FaucetFundsYaml = FaucetFundsYaml {getFaucetFunds :: FaucetFunds}

instance Y.FromJSON FaucetFundsYaml where
    parseJSON = fmap FaucetFundsYaml . faucetFundsFromValue

instance Y.ToJSON FaucetFundsYaml where
    toJSON (FaucetFundsYaml funds) = faucetFundsToValue funds

faucetFundsToValue :: FaucetFunds -> Value
faucetFundsToValue FaucetFunds{..} =
    object
        [ "pureAdaFunds" .= pureAdaFunds'
        , "maryAllegraFunds" .= maryAllegraFunds'
        , "massiveWalletFunds" .= massiveWalletFunds'
        ]
  where
    pureAdaFunds' = encodeAddrCoins pureAdaFunds
    maryAllegraFunds' =
        map
            (bimap bech32 encodeTokenBundle)
            maryAllegraFunds
    massiveWalletFunds' = encodeAddrCoins massiveWalletFunds
    encodeAddrCoins = map (\(addr, Coin coin) -> (bech32 addr, coin))

    encodeTokenBundle :: (TokenBundle, [(String, String)]) -> Value
    encodeTokenBundle (bundle, keys) =
        let (Coin c, assets) = toFlatList bundle
        in  object
                [ "coin" .= c
                , "assets" .= (encodeAssets <$> assets)
                , "keys" .= keys
                ]
    encodeAssets :: (AssetId, TokenQuantity) -> Value
    encodeAssets (AssetId policyId assetName, q) =
        object
            [ "policyId" .= policyId
            , "assetName" .= assetName
            , "quantity" .= q
            ]

retrieveFunds :: FileOf "faucet-funds" -> IO FaucetFunds
retrieveFunds fp = getFaucetFunds <$> Y.decodeFileThrow (absFilePathOf fp)

saveFunds :: FileOf "faucet-funds" -> FaucetFunds -> IO ()
saveFunds fp = Y.encodeFile (absFilePathOf fp) . FaucetFundsYaml

faucetFundsFromValue :: Value -> Parser FaucetFunds
faucetFundsFromValue = withObject "FaucetFunds" $ \o -> do
    adaFunds <- o .: "pureAdaFunds" >>= decodeAddrCoins
    allegraFunds <- o .: "maryAllegraFunds" >>= decodeMaryAllegraFunds
    massiveFunds <- o .: "massiveWalletFunds" >>= decodeAddrCoins
    pure $ FaucetFunds adaFunds allegraFunds massiveFunds
  where
    decodeAddr = \case
        String addr -> case fromBech32 addr of
            Nothing -> fail $ "Invalid address: " <> T.unpack addr
            Just addr' -> pure addr'
        _ -> fail "Invalid address"
    decodeAddrCoins = traverse $ \case
        (addr, Number c) -> do
            addr' <- decodeAddr addr
            pure (addr', Coin $ floor c)
        _ -> fail "Invalid address/coin pair"
    decodeMaryAllegraFunds = traverse decodeMaryAllegraFund
    decodeMaryAllegraFund = \case
        (addr, Object o) -> do
            addr' <- decodeAddr addr
            cain <- Coin <$> o .: "coin"
            assets <- o .: "assets" >>= decodeAssets
            keys <- o .: "keys" >>= decodeKeys
            pure (addr', (fromFlatList cain assets, keys))
        _ -> fail "Invalid address/bundle pair"
    decodeAssets = traverse $ withObject "assets" $ \o -> do
        policyId <- o .: "policyId"
        assetName <- o .: "assetName"
        quantity <- o .: "quantity"
        pure (AssetId policyId assetName, quantity)
    decodeKeys = traverse $ \case
        (String sk, String vk) -> pure (T.unpack sk, T.unpack vk)
        _ -> fail "Invalid key"
