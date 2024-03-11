{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
  ( parseTxCBOR
  , ParsedTxCBOR (..)
  )
  where

import Prelude

import Cardano.Binary
    ( DecoderError
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
    ( Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    , applyEraFun
    , extractEraValue
    , sequenceEraValue
    , (:*:) (..)
    , (:.:) (..)
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( mkEraFun
    , mkEraFunK
    , runEraFunK
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    , deserializeTx
    )
import Cardano.Wallet.Read.Tx.Certificates
    ( getEraCertificates
    )
import Cardano.Wallet.Read.Tx.ExtraSigs
    ( getEraExtraSigs
    )
import Cardano.Wallet.Read.Tx.Integrity
    ( getEraIntegrity
    )
import Cardano.Wallet.Read.Tx.Mint
    ( getEraMint
    )
import Cardano.Wallet.Read.Tx.ReferenceInputs
    ( getEraReferenceInputs
    )
import Cardano.Wallet.Read.Tx.Validity
    ( getEraValidity
    )
import Cardano.Wallet.Read.Tx.Witnesses
    ( getEraWitnesses
    )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts
    , ValidityIntervalExplicit
    )
import Control.Category
    ( (<<<)
    )
import GHC.Generics
    ( Generic
    )
import Servant.Server
    ( Handler
    , err500
    )

import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ExtraSigs as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Integrity as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint as Feature
import qualified Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Validity as Feature
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

newtype ErrParseCBOR = ErrParseCBOR DecoderError
    deriving (Eq, Show)

instance IsServerError ErrParseCBOR where
    toServerError (ErrParseCBOR decoderError) =
        apiError err500 UnexpectedError $ T.unwords
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
    deriving Generic

parser :: EraFun Tx (K ParsedTxCBOR)
parser = mkEraFunK $ do
    certificates <-
        runEraFunK $ Feature.primitiveCertificates <<< getEraCertificates
    witnesses <- runEraFun getEraWitnesses
    referenceInputs <- runEraFun getEraReferenceInputs
    mint <- runEraFun getEraMint
    let mintBurn = runEraFunK Feature.mint
            $ mint :*: witnesses :*: referenceInputs
    validityInterval <- runEraFunK $ Feature.getValidity <<< getEraValidity
    scriptIntegrity <- runEraFunK $ Feature.integrity <<< getEraIntegrity
    extraSignatures <- runEraFunK $ Feature.extraSigs <<< getEraExtraSigs
    pure $ ParsedTxCBOR{..}

txCBORParser ::
    EraFun (K BL.ByteString) (Either DecoderError :.: K (ParsedTxCBOR))
txCBORParser = mkEraFun (Comp . fmap (runEraFun parser) . unComp . runEraFun deserializeTx)
  where unComp (Comp a) = a

-- | Parse CBOR to some values and throw a server deserialize error if failing.
parseTxCBOR :: TxCBOR -> Handler ParsedTxCBOR
parseTxCBOR cbor =
    either
        (liftE . ErrParseCBOR)
        (pure . extractEraValue)
        (sequenceEraValue (applyEraFun txCBORParser cbor))
