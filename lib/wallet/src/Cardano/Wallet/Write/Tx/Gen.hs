{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
module Cardano.Wallet.Write.Tx.Gen
    ( genDatum
    , genData
    , genDatumHash
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.Data
    ( Data (..), dataToBinaryData)
import Cardano.Wallet.Write.Tx
    ( type Datum, type DatumHash, pattern Datum, pattern DatumHash, pattern NoDatum, datumHashFromBytes, BinaryData )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Plutus.V1.Ledger.Api as PV1

genDatum :: Gen Datum
genDatum = oneof
    [ Datum <$> genData
    , DatumHash <$> genDatumHash
    , pure NoDatum
    ]

-- Originally from https://github.com/input-output-hk/cardano-ledger/blob/c7c63dabdb215ebdaed8b63274965966f2bf408f/eras/alonzo/test-suite/src/Test/Cardano/Ledger/Alonzo/Serialisation/Generators.hs#L66-L79
genData :: Gen BinaryData
genData = dataToBinaryData . Data <$> resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
          oneof
            [ PV1.I <$> arbitrary,
              PV1.B <$> genByteString,
              PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2)),
              PV1.Constr <$> fmap fromIntegral (arbitrary :: Gen Integer)
                <*> listOf (gendata (n `div` 2)),
              PV1.List <$> listOf (gendata (n `div` 2))
            ]
    gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> genByteString]

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
    . datumHashFromBytes
    . BS.pack <$> vectorOf 32 arbitrary

genByteString :: Gen ByteString
genByteString = BS.pack <$> (choose (0, 64) >>= vector)
