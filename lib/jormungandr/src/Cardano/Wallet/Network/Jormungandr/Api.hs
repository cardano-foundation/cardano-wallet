{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-#  OPTIONS_GHC -fno-warn-orphans #-} -- for content types

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An specification for the Jörmungandr REST API.
module Cardano.Wallet.Network.Jormungandr.Api
    ( Api
    , GetBlock
    , GetTipId
    , GetBlockDecendantIds
    , PostSignedTx
    , BlockId
    , api
    , SignedTx
    ) where

import Prelude

import Cardano.Wallet.Binary.Jormungandr
    ( FromBinary (..), runGet )
import Cardano.Wallet.Primitive.Types
    ( Block, Hash (..) )
import Data.Binary.Get
    ( getByteString )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Encoding
    ( decodeUtf8 )
import Servant.API
    ( (:<|>)
    , (:>)
    , Capture
    , Get
    , MimeUnrender (..)
    , NoContent
    , Post
    , QueryParam
    , ReqBody
    , ToHttpApiData (..)
    )
import Servant.API.ContentTypes
    ( OctetStream, PlainText )

import qualified Data.ByteString.Lazy as BL


api :: Proxy Api
api = Proxy

type Api =
    GetBlock :<|> GetTipId :<|> GetBlockDecendantIds :<|> PostSignedTx


-- | Retrieve a block by its id.
type GetBlock
    = "api"
    :> "v0"
    :> "block"
    :> Capture "blockHeaderHash" BlockId
    :> Get '[OctetStream] Block

-- | Retrieve a list of 'n' block decendant ids, sorted from closest to
-- farthest.
--
-- There might also exist fewer than 'n' decendants.
type GetBlockDecendantIds
    = "api"
    :> "v0"
    :> "block"
    :> Capture "blockId" BlockId
    :> QueryParam "count" Int
    :> Get '[OctetStream] [BlockId]

-- | Retrieve the header of the latest known block.
type GetTipId
    = "api"
    :> "v0"
    :> "tip"
    :> Get '[PlainText] BlockId

type PostSignedTx
    = "api"
    :> "v0"
    :> "transaction"
    :> ReqBody '[OctetStream] SignedTx
    :> Post '[NoContent] NoContent

-- TODO: Replace SignedTx with something real
data SignedTx

newtype BlockId = BlockId (Hash "block")
    deriving Show

instance ToHttpApiData BlockId where
    toUrlPiece (BlockId (Hash bytes)) = decodeUtf8 $ convertToBase Base16 bytes

instance FromBinary BlockId where
    get = BlockId . Hash <$> getByteString 32

instance MimeUnrender PlainText BlockId where
    mimeUnrender _ bs =
        BlockId . Hash <$> convertFromBase Base16 (BL.toStrict bs)

-- Orphan instance
instance FromBinary a => MimeUnrender OctetStream a where
    mimeUnrender _ bs = Right $ runGet get bs
