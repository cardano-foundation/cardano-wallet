{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Server.Handlers.TxCBOR
  ( parseTxCBOR
  , ParsedTxCBOR (..)
  )
  where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.Api.Server.Error
    ( IsServerError (..), apiError, liftE, showT )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (UnexpectedError) )
import Cardano.Wallet.Primitive.Types
    ( Certificate )
import Cardano.Wallet.Read
    ( Tx (..) )
import Cardano.Wallet.Read.Eras
    ( (:.:)
    , EraFun (..)
    , EraValue
    , K (..)
    , applyEraFun
    , extractEraValue
    , sequenceEraValue
    , (*.**)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFunK (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR, deserializeTx )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates )
import Codec.CBOR.Read
    ( DeserialiseFailure )
import Control.Category
    ( (.) )
import GHC.Generics
    ( Generic )
import Servant.Server
    ( Handler, err500 )

import qualified Cardano.Wallet.Read.Primitive.Tx.Features.Certificates as Feature
import qualified Data.ByteString.Lazy as BL

newtype ErrParseCBOR = ErrParseCBOR DeserialiseFailure
    deriving (Eq, Show)

instance IsServerError ErrParseCBOR where
    toServerError (ErrParseCBOR df) =
        apiError err500 UnexpectedError $ mconcat
            [ "Error while trying to parse a transaction CBOR from the database"
            , showT df
            ]

-- | Values parsed out of a CBOR for a 'Tx' in any era
newtype ParsedTxCBOR = ParsedTxCBOR
    { certificates :: [Certificate]
    }
    deriving Generic

parser :: EraFun Tx (K ParsedTxCBOR)
parser = fromEraFunK
    $ ParsedTxCBOR
        <$> EraFunK (Feature.certificates . getEraCertificates)

txCBORParser :: EraFun
    (K BL.ByteString)
    (Either DeserialiseFailure :.: K (ParsedTxCBOR))
txCBORParser  = parser *.** deserializeTx

-- | Parse CBOR to some values and throw a server deserialize error if failing.
parseTxCBOR
    :: TxCBOR
    -> Handler ParsedTxCBOR
parseTxCBOR cbor =
    case  fmap extractEraValue
            $ sequenceEraValue
            $ applyEraFun txCBORParser cbor
    of
        Left df -> liftE $ ErrParseCBOR df
        Right result -> pure result
