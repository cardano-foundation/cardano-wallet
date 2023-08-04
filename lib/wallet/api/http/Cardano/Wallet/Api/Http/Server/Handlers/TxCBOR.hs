{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

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
  , (*&&&*)
  , (*.**)
  , (:.:)
  )
import Cardano.Wallet.Read.Eras.EraFun
  ( EraFunK (..)
  )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates qualified as Feature
import Cardano.Wallet.Read.Primitive.Tx.Features.ExtraSigs qualified as Feature
import Cardano.Wallet.Read.Primitive.Tx.Features.Integrity qualified as Feature
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint qualified as Feature
import Cardano.Wallet.Read.Primitive.Tx.Features.Validity qualified as Feature
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
import Data.ByteString.Lazy qualified as BL
import Data.Function
  ( (&)
  )
import Data.Text qualified as T
import GHC.Generics
  ( Generic
  )
import Servant.Server
  ( Handler
  , err500
  )
import Prelude

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

parser :: EraFun Tx (K ParsedTxCBOR)
parser =
  fromEraFunK
    $ ParsedTxCBOR
      <$> EraFunK (Feature.certificates <<< getEraCertificates)
      <*> EraFunK
        ( Feature.mint
            <<< getEraMint *&&&* getEraWitnesses *&&&* getEraReferenceInputs
        )
      <*> EraFunK (Feature.getValidity <<< getEraValidity)
      <*> EraFunK (Feature.integrity <<< getEraIntegrity)
      <*> EraFunK (Feature.extraSigs <<< getEraExtraSigs)

txCBORParser
  :: EraFun (K BL.ByteString) (Either DecoderError :.: K (ParsedTxCBOR))
txCBORParser = parser *.** deserializeTx

-- | Parse CBOR to some values and throw a server deserialize error if failing.
parseTxCBOR :: TxCBOR -> Handler ParsedTxCBOR
parseTxCBOR cbor =
  extractEraValue <$> sequenceEraValue (applyEraFun txCBORParser cbor)
    & either (liftE . ErrParseCBOR) pure
