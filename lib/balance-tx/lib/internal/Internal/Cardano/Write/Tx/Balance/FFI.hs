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

-- Exported function to balance a transaction
-- Convert input and output for C compatibility
foreign export ccall balanceTxFFI_C
    :: CString -- CBOR PParams
    -> CString -- TimeTranslation (as string; encoding up to you)
    -> CInt -- Random seed
    -> CString -- Serialized UTxO Map (define format)
    -> CString -- Change address (serialized)
    -> CString -- Input transaction CBOR
    -> Ptr CString -- Result buffer (output)
    -> IO CInt -- Error code (0 = success, non-zero = failure)

balanceTxFFI_C :: CString -> CString -> CInt -> CString -> CString -> CString -> Ptr CString -> IO CInt
balanceTxFFI_C pparamsCborPtr timeTranslationPtr seed utxoPtr changeAddrPtr txCborPtr resultPtr = do
    -- Convert C strings to Haskell ByteStrings
    pparamsCbor <- unsafePackCStringLen =<< cStringLenFromCString pparamsCborPtr
    timeTranslation <- unsafePackCStringLen =<< cStringLenFromCString timeTranslationPtr
    utxoSerialized <- unsafePackCStringLen =<< cStringLenFromCString utxoPtr
    changeAddr <- unsafePackCStringLen =<< cStringLenFromCString changeAddrPtr
    txCbor <- unsafePackCStringLen =<< cStringLenFromCString txCborPtr

    let seedInt = fromIntegral seed :: Int
        utxoMap = deserializeUTxOMap utxoSerialized -- Define deserialization
        result = balanceTxFFI pparamsCbor (deserializeTimeTranslation timeTranslation) seedInt utxoMap changeAddr txCbor

    case result of
        Left err -> return $ errorCodeFromErr err -- Define error mapping
        Right balancedTx -> do
            balancedTxCStr <- unsafeUseAsCString balancedTx (return . id)
            poke resultPtr balancedTxCStr
            return 0 -- Success

-- Helpers
cStringLenFromCString :: CString -> IO (CString, Int)
cStringLenFromCString cstr = do
    len <- lengthCString cstr
    return (cstr, len)

lengthCString :: CString -> IO Int
lengthCString cstr = go 0
  where
    go n = do
        c <- peekElemOff cstr n
        if c == 0 then return n else go (n + 1)

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
