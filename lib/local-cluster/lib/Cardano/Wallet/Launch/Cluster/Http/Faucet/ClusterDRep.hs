{-# LANGUAGE DerivingStrategies #-}

module Cardano.Wallet.Launch.Cluster.Http.Faucet.ClusterDRep
    ( ClusterDRep (..)
    , genClusterDRep
    )
where

import Cardano.Wallet.Launch.Cluster.Http.Faucet.OpenApi
    ( clusterDRepSchema
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepKeyHash (..)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertFromBase
    , convertToBase
    )
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema (..)
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , vectorOf
    )
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

-- | A DRep whose keys the local cluster holds, named by the hash of its
-- verification key.
newtype ClusterDRep = ClusterDRep {clusterDRepKeyHash :: DRepKeyHash}
    deriving stock (Eq, Show)

instance ToJSON ClusterDRep where
    toJSON (ClusterDRep (DRepKeyHash bytes)) =
        object ["key_hash" .= T.decodeUtf8 (convertToBase Base16 bytes)]

instance FromJSON ClusterDRep where
    parseJSON = withObject "ClusterDRep" $ \o -> do
        hex <- o .: "key_hash"
        case convertFromBase Base16 (T.encodeUtf8 hex) of
            Right bytes
                | BS.length bytes == 28 ->
                    pure $ ClusterDRep $ DRepKeyHash bytes
            _ -> fail "key_hash: expected 28 hex-encoded bytes"

instance ToSchema ClusterDRep where
    declareNamedSchema _ =
        pure $ NamedSchema (Just "ClusterDRep") clusterDRepSchema

-- | Generate a 'ClusterDRep' with a key hash of the right length.
genClusterDRep :: Gen ClusterDRep
genClusterDRep =
    ClusterDRep . DRepKeyHash . BS.pack <$> vectorOf 28 arbitrary
