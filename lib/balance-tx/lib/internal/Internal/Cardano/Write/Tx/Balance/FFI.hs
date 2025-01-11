{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wwarn #-}
module Internal.Cardano.Write.Tx.Balance.FFI where

import Prelude

import qualified Cardano.Api as Cardano
import Cardano.Ledger.Api
    ( PParams
    , StandardCrypto
    , TxOut
    )
import Cardano.Ledger.Api.Tx.In
    ( TxIn
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Control.Monad.Trans.Random
    ( evalRand
    )
import Data.ByteString
    ( ByteString
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Unsafe
    ( unsafePackCStringLen
    , unsafeUseAsCString
    , unsafeUseAsCStringLen
    )
import Data.Map
    ( Map
    )
import qualified Data.Map as Map
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Internal.Cardano.Write.Eras
import Internal.Cardano.Write.Tx
    ( Address
    , deserializeTx
    , serializeTx
    , utxoFromTxOutsInRecentEra
    )
import Internal.Cardano.Write.Tx.Balance
import Internal.Cardano.Write.Tx.TimeTranslation
import System.IO.Unsafe
    ( unsafePerformIO
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenFromSeed
    )

deserializeUTxOMap :: ByteString -> Map (TxIn StandardCrypto) (TxOut Conway)
deserializeUTxOMap = undefined -- Define format and logic

deserializeTimeTranslation :: ByteString -> TimeTranslation
deserializeTimeTranslation = undefined -- Define deserialization

deserializeAddress :: ByteString -> Address
deserializeAddress = undefined -- Define deserialization

errorCodeFromErr :: ErrBalanceTx Conway -> CInt
errorCodeFromErr = undefined -- Map errors to integers

-- | Draft simplified balanceTx function for use with C FFI and JS
balanceTxFFI
    :: ByteString -- CBOR PParams, could use something else than cbor
    -> TimeTranslation -- TODO: Hard-code and/or come up with neat encoding
    -> Int -- Random seed
    -> Map (TxIn StandardCrypto) (TxOut Conway) -- UTxO set
    -> ByteString -- Change address (wrapped bytestring)
    -> ByteString -- Tx
    -> Either (ErrBalanceTx Conway) ByteString -- Tx
balanceTxFFI pparamsCbor timeTranslation seed utxo changeAddr txCbor
    = (`evalRand` stdGenFromSeed (StdGenSeed $ toEnum seed)) $ runExceptT $ do
        (transactionInEra, _nextChangeState) <-
            balanceTx
                (deserializePParams pparamsCbor)
                timeTranslation
                AllKeyPaymentCredentials
                (constructUTxOIndex $ UTxO utxo)
                (simpleChangeAddressGen $ deserializeAddress changeAddr)
                ()
                (PartialTx
                    (deserializeTx txCbor)
                    mempty
                    mempty
                    (StakeKeyDepositMap mempty)
                    mempty)

        pure $ serializeTx transactionInEra

  where
    deserializePParams :: ByteString -> PParams Conway
    deserializePParams = error "todo"

    deserializeConwayTx :: ByteString -> Cardano.Tx Cardano.ConwayEra
    deserializeConwayTx = either (error . show) id
        . Cardano.deserialiseFromCBOR (Cardano.AsTx Cardano.AsConwayEra)

    serializeConwayTx :: Cardano.Tx Cardano.ConwayEra -> ByteString
    serializeConwayTx = Cardano.serialiseToCBOR

    simpleChangeAddressGen :: Address -> ChangeAddressGen ()
    simpleChangeAddressGen addr = ChangeAddressGen
        { genChangeAddress = \() -> (addr, ())
        , maxLengthChangeAddress = addr
        }
