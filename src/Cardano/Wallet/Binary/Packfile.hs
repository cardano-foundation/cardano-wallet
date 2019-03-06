-- | Decoder for the rust-cardano packfile format.
--
-- A pack file is a collection of bytestring blobs.
--
-- The reference implementation is in
-- <https://github.com/input-output-hk/rust-cardano/blob/master/storage-units/src/packfile.rs packfile.rs>.

module Cardano.Wallet.Binary.Packfile
    ( decodePackfile
    , PackfileError (..)
    ) where

import Prelude

import Control.Monad
    ( guard, when )
import Data.Binary.Get
    ( Get, getByteString, getInt32be, isEmpty, runGetOrFail )
import Data.Int
    ( Int32 )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- | Things related to the file format that can go wrong when decoding a pack file.
data PackfileError
    = MissingMagicError
    | WrongFileTypeError
    | VersionTooOldError
    | VersionTooNewError
    | BlobDecodeError String
    deriving (Show, Eq)

-- | Decode a Cardano version 1 pack file. The blobs are returned as a
-- list. Decoding is not incremental, and all data is stored in memory.
decodePackfile :: BL.ByteString -> Either PackfileError [BS.ByteString]
decodePackfile pf = case runGetOrFail getHeader pf of
    Left _ -> Left MissingMagicError
    Right (rest, _, hdr) -> case checkHeader hdr of
        Left e -> Left e
        Right () -> case runGetOrFail getBlobs rest of
            Left (_, _, msg) -> Left (BlobDecodeError msg)
            Right ("", _, res) -> Right res
            Right (_, _, _) -> Left (BlobDecodeError "Unconsumed data")

data Header = Header !BS.ByteString !Int

getHeader :: Get Header
getHeader = do
    magic <- getByteString 8
    guard (magic == "\254CARDANO")
    fileType <- getByteString 4
    version <- getInt32be
    pure $! Header fileType (fromIntegral version)

-- The current version of a rust-cardano packfile is 1, and we do not know how
-- to decode any other version.
checkHeader :: Header -> Either PackfileError ()
checkHeader = checkHeader' "PACK" 1 1

checkHeader' :: BS.ByteString -> Int -> Int -> Header -> Either PackfileError ()
checkHeader' expFileType minVersion maxVersion (Header fileType version)
    | fileType /= expFileType = Left WrongFileTypeError
    | version < minVersion = Left VersionTooOldError
    | version > maxVersion = Left VersionTooNewError
    | otherwise = Right ()

getBlobs :: Get [BS.ByteString]
getBlobs = do
    empty <- isEmpty
    if empty
        then pure []
        else do
            blob <- getBlob
            blobs <- getBlobs
            pure (blob:blobs)

getBlob :: Get BS.ByteString
getBlob = do
    size <- getInt32be
    when (size >= 20000000) $
        -- limit blob size to 20MB to avoid consuming all memory
        fail ("read block of size: " <> show size)
    bytes <- getByteString (fromIntegral size)
    _pad <- getByteString (npad size)
    pure bytes
    where
        npad :: Int32 -> Int
        npad n = -((fromIntegral n) `mod` (-4))
