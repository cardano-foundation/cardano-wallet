{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An specification for the Jörmungandr REST API.
module Cardano.Wallet.Jormungandr.Api
    ( Api
    , GetBlock
    , GetTipId
    , GetBlockDescendantIds
    , PostMessage
    , BlockId (..)
    , api
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( Block
    , MessageType (..)
    , getBlock
    , putSignedTx
    , runGet
    , runPut
    , withHeader
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), TxWitness )
import Control.Applicative
    ( many )
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
    , Accept (..)
    , Capture
    , Get
    , MimeRender (..)
    , MimeUnrender (..)
    , NoContent
    , Post
    , QueryParam
    , ReqBody
    , ToHttpApiData (..)
    )

import qualified Data.ByteString.Lazy as BL
import qualified Servant.API.ContentTypes as Servant


api :: Proxy Api
api = Proxy

type Api = GetTipId :<|> GetBlock :<|> GetBlockDescendantIds :<|> PostMessage

-- | Retrieve a block by its id.
type GetBlock
    = "v0"
    :> "block"
    :> Capture "blockHeaderHash" BlockId
    :> Get '[JormungandrBinary] Block

-- | Retrieve 'n' descendants of a given block, sorted from closest to
-- farthest.
--
-- There might also exist fewer than 'n' descendants.
--
-- For n=3 we might have:
--
-- > [genesis] ... -- [b] -- [b+1] -- [b+2] -- [b+3] -- ... -- [tip]
-- >                   \       \                  \
-- >                  parent    +--- descendants ---+
type GetBlockDescendantIds
    = "v0"
    :> "block"
    :> Capture "blockId" BlockId
    :> "next_id"
    :> QueryParam "count" Word
    :> Get '[JormungandrBinary] [BlockId]

-- | Retrieve the header of the latest known block.
type GetTipId
    = "v0"
    :> "tip"
    :> Get '[Hex] BlockId

type PostMessage
    = "v0"
    :> "message"
    :> ReqBody '[JormungandrBinary] (Tx, [TxWitness])
    :> Post '[NoContent] NoContent

newtype BlockId = BlockId { getBlockId :: Hash "BlockHeader" }

instance ToHttpApiData BlockId where
    toUrlPiece (BlockId (Hash bytes)) = decodeUtf8 $ convertToBase Base16 bytes

instance MimeUnrender JormungandrBinary [BlockId] where
    mimeUnrender _ =
        pure . fmap (BlockId . Hash) . runGet (many $ getByteString 32)

instance MimeUnrender Hex BlockId where
    mimeUnrender _ bs =
        BlockId . Hash <$> convertFromBase Base16 (BL.toStrict bs)

{-------------------------------------------------------------------------------
                            Content Types
-------------------------------------------------------------------------------}

-- | Represents the binary format of Jörmungandr.
data JormungandrBinary

instance Accept JormungandrBinary where
    contentType _ = contentType $ Proxy @Servant.OctetStream

instance MimeUnrender JormungandrBinary Block where
    mimeUnrender _ = pure . runGet getBlock

instance MimeRender JormungandrBinary (Tx, [TxWitness]) where
    mimeRender _ (Tx _ ins outs, wits) =
        runPut $ withHeader MsgTypeTransaction $ putSignedTx ins outs wits

data Hex

-- | Represents data rendered to hexadecimal text.
instance Accept Hex where
    contentType _ = contentType $ Proxy @Servant.PlainText
