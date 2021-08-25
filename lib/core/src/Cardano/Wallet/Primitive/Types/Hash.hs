{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Types and functions relating to hash values.
--
module Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , hashFromText
    , mockHash
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Hashable
    ( Hashable )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Fmt
    ( Buildable (..), prefixF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Quiet
    ( Quiet (..) )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as C
import qualified Data.Text.Encoding as T

newtype Hash (tag :: Symbol) = Hash { getHash :: ByteString }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (ByteArrayAccess)
    deriving (Read, Show) via (Quiet (Hash tag))
    deriving anyclass (NFData, Hashable)

instance Buildable (Hash tag) where
    build h = mempty
        <> prefixF 8 builder
      where
        builder = build . toText $ h

instance ToText (Hash tag) where
    toText = T.decodeUtf8 . convertToBase Base16 . getHash

instance FromText (Hash "Tx")            where fromText = hashFromText 32
instance FromText (Hash "Account")       where fromText = hashFromText 32
instance FromText (Hash "Genesis")       where fromText = hashFromText 32
instance FromText (Hash "Block")         where fromText = hashFromText 32
instance FromText (Hash "BlockHeader")   where fromText = hashFromText 32
instance FromText (Hash "RewardAccount") where fromText = hashFromText 28
instance FromText (Hash "TokenPolicy")   where fromText = hashFromText 28 -- Script Hash

hashFromText
    :: forall t. (KnownSymbol t)
    => Int
        -- ^ Expected decoded hash length
    -> Text
    -> Either TextDecodingError (Hash t)
hashFromText len text = case decoded of
    Right bytes | BS.length bytes == len ->
        Right $ Hash bytes
    _ ->
        Left $ TextDecodingError $ unwords
            [ "Invalid"
            , mapFirst C.toLower $ symbolVal $ Proxy @t
            , "hash: expecting a hex-encoded value that is"
            , show len
            , "bytes in length."
            ]
  where
    decoded = convertFromBase Base16 $ T.encodeUtf8 text

    mapFirst :: (a -> a) -> [a] -> [a]
    mapFirst _     [] = []
    mapFirst fn (h:q) = fn h:q

-- | Constructs a hash that is good enough for testing.
--
mockHash :: Show a => a -> Hash whatever
mockHash = Hash . blake2b256 . B8.pack . show
  where
     blake2b256 :: ByteString -> ByteString
     blake2b256 =
         BA.convert . hash @_ @Blake2b_256
