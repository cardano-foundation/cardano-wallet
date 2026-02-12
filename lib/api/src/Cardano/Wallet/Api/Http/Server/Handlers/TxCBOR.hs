{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( parseTxCBOR
    , ParsedTxCBOR (..)
    )
where

import Cardano.Binary
    ( DecoderError
    )
import Cardano.Read.Ledger.Tx.Certificates
    ( getEraCertificates
    )
import Cardano.Read.Ledger.Tx.ExtraSigs
    ( getEraExtraSigs
    )
import Cardano.Read.Ledger.Tx.Integrity
    ( getEraIntegrity
    )
import Cardano.Read.Ledger.Tx.Mint
    ( getEraMint
    )
import Cardano.Read.Ledger.Tx.ReferenceInputs
    ( getEraReferenceInputs
    )
import Cardano.Read.Ledger.Tx.Validity
    ( getEraValidity
    )
import Cardano.Read.Ledger.Tx.Witnesses
    ( getEraWitnesses
    )
import Cardano.Wallet.Api.Http.Server.Error
    ( IsServerError (..)
    , apiError
    , liftE
    , showT
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (UnexpectedError)
    )
import Cardano.Wallet.Primitive.Types
    ( Certificate
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash
    )
import Cardano.Wallet.Read
    ( IsEra
    , Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( applyEraFun
    , (:*:) (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    , parseTxFromCBOR
    )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts
    , ValidityIntervalExplicit
    )
import GHC.Generics
    ( Generic
    )
import Servant.Server
    ( Handler
    , err500
    )
import Prelude

import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ExtraSigs as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Integrity as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity as Feature
import qualified Data.Text as T

newtype ErrParseCBOR = ErrParseCBOR DecoderError
    deriving (Eq, Show)

instance IsServerError ErrParseCBOR where
    toServerError (ErrParseCBOR decoderError) =
        apiError err500 UnexpectedError
            $ T.unwords
                [ "Error while trying to parse a transaction CBOR from the database"
                , showT decoderError
                ]

-- | Values parsed out of a CBOR for a 'Tx' in any era
data ParsedTxCBOR = ParsedTxCBOR
    { certificates :: [Certificate]
    , mintBurn :: (TokenMapWithScripts, TokenMapWithScripts)
    , validityInterval :: Maybe ValidityIntervalExplicit
    , scriptIntegrity :: Maybe (Hash "ScriptIntegrity")
    , extraSignatures :: [Hash "ExtraSignature"]
    }
    deriving (Generic)

parser :: IsEra era => Tx era -> ParsedTxCBOR
parser = do
    certificates <- Feature.getCertificates . getEraCertificates
    witnesses <- getEraWitnesses
    referenceInputs <- getEraReferenceInputs
    mint <- getEraMint
    let mintBurn = Feature.mint $ mint :*: witnesses :*: referenceInputs
    validityInterval <- Feature.getValidity . getEraValidity
    scriptIntegrity <- Feature.integrity . getEraIntegrity
    extraSignatures <- Feature.extraSigs . getEraExtraSigs
    pure $ ParsedTxCBOR{..}

-- | Parse CBOR to some values and throw a server deserialize error if failing.
parseTxCBOR :: TxCBOR -> Handler ParsedTxCBOR
parseTxCBOR =
    either (liftE . ErrParseCBOR) (pure . applyEraFun parser)
        . parseTxFromCBOR
