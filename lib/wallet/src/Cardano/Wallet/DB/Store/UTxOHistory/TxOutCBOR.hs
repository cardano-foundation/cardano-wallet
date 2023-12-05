module Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR
    ( serializeTxOut
    , deserializeTxOut
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (TokenBundle)
    )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId)
    , fromFlatList
    , toFlatList
    )
import Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (UnsafeTokenName)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (UnsafeTokenPolicyId)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (TxOut)
    )
import Codec.CBOR.Decoding
    ( Decoder
    , decodeBytes
    , decodeInteger
    , decodeListLen
    )
import Codec.CBOR.Encoding
    ( Encoding
    , encodeBytes
    , encodeInteger
    , encodeListLen
    )
import Codec.CBOR.Read
    ( deserialiseFromBytes
    )
import Codec.CBOR.Write
    ( toLazyByteString
    )
import Codec.Serialise
    ( DeserialiseFailure
    )
import Control.Exception
    ( Exception
    )
import Control.Monad
    ( replicateM
    )
import Data.ByteString.Lazy
    ( ByteString
    )

-- | Signal a failure to decode a 'TxOut' from a ByteString.
data FailedDecodingDeltaUTxO
    = FailedPatternMatching String
    | FailedDecoding DeserialiseFailure
    deriving (Show, Eq)

instance Exception FailedDecodingDeltaUTxO

encodeTxOut :: TxOut -> Encoding
encodeTxOut (TxOut (Address addr) (TokenBundle (Coin c) m)) =
    encodeListLen 3
        <> encodeBytes addr
        <> encodeInteger (fromIntegral c)
        <> let tokens = toFlatList m
            in encodeListLen (fromIntegral $ length tokens)
                <> foldMap
                    ( \( AssetId
                            (UnsafeTokenPolicyId (Hash policy))
                            (UnsafeTokenName name)
                        , TokenQuantity quant
                        ) ->
                            encodeListLen 3
                                <> encodeBytes policy
                                <> encodeBytes name
                                <> encodeInteger (fromIntegral quant)
                    )
                    tokens

decodeTxOut :: Decoder s TxOut
decodeTxOut = do
    len <- decodeListLen
    case len of
        3 -> do
            addr <- decodeBytes
            c <- decodeInteger
            tokensLen <- decodeListLen
            tokens <- replicateM tokensLen $ do
                len' <- decodeListLen
                case len' of
                    3 -> do
                        policy <- decodeBytes
                        name <- decodeBytes
                        quant <- decodeInteger
                        return
                            ( AssetId
                                (UnsafeTokenPolicyId (Hash policy))
                                (UnsafeTokenName name)
                            , TokenQuantity $
                                fromIntegral quant
                            )
                    _ -> fail $ "decodeTxOut: expected 3, got " ++ show len
            return $
                TxOut
                    (Address addr)
                    ( TokenBundle (Coin $ fromIntegral c) $
                        fromFlatList tokens
                    )
        _ -> fail $ "decodeTxOut: expected 3, got " ++ show len

-- | Read a 'TxOut' from a binary blob.
deserializeTxOut :: ByteString -> Either DeserialiseFailure TxOut
deserializeTxOut = fmap snd . deserialiseFromBytes decodeTxOut

-- | Write a 'TxOut' to a binary blob.
serializeTxOut :: TxOut -> ByteString
serializeTxOut = toLazyByteString . encodeTxOut
