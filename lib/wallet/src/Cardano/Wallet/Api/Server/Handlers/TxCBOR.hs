{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.Server.Handlers.TxCBOR
  ( parseTxCBOR
  , ParsedTxCBOR (..)
  )
  where

import Prelude

import Cardano.Wallet.Api.Server.Error
    ( IsServerError (..), apiError, liftE, showT )
import Cardano.Wallet.Api.Server.Handlers.Certificates
    ( extractCertificates )
import Cardano.Wallet.Api.Server.Handlers.MintBurn
    ( extractMintBurn )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (UnexpectedError) )
import Cardano.Wallet.Primitive.Types
    ( Certificate )
import Cardano.Wallet.Read
    ( Tx (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
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
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts )
import Codec.CBOR.Read
    ( DeserialiseFailure )
import GHC.Generics
    ( Generic )
import Servant.Server
    ( Handler, err500 )

newtype ErrParseCBOR = ErrParseCBOR DeserialiseFailure
    deriving (Eq, Show)

instance IsServerError ErrParseCBOR where
    toServerError (ErrParseCBOR df) =
        apiError err500 UnexpectedError $ mconcat
            [ "Error while trying to parse a transaction CBOR from the database"
            , showT df
            ]

parseTxCBOR' :: Maybe TxCBOR -> EraFun Tx (K a) -> Handler (Maybe a)
parseTxCBOR' Nothing _ = pure Nothing
parseTxCBOR' (Just cbor) f =
    case fmap extractEraValue
            $ sequenceEraValue
            $ applyEraFun (f *.** deserializeTx) cbor
    of
        Left df -> liftE $ ErrParseCBOR df
        Right result -> pure $ Just result

-- | Values parsed out of a CBOR for a 'Tx' in any era
data ParsedTxCBOR = ParsedTxCBOR
    { certificates :: [Certificate]
    , mintBurn :: (TokenMapWithScripts, TokenMapWithScripts)
    }
    deriving Generic

parser :: EraFun Tx (K ParsedTxCBOR)
parser = fromEraFunK
    $ ParsedTxCBOR
        <$> EraFunK extractCertificates
        <*> EraFunK extractMintBurn

-- | Parse CBOR to some values and throw a server deserialize error if failing.
--
-- Missing CBOR in the tx is mapped to Nothing. This is a hack until
-- restoration of the wallet will be required for the users.
parseTxCBOR :: Maybe TxCBOR -> Handler (Maybe ParsedTxCBOR)
parseTxCBOR cbor = parseTxCBOR' cbor parser
