{-# LANGUAGE TypeFamilies #-}
module Cardano.Wallet.Write.Tx.Gen
    ( genDatum
    , genData
    , genDatumHash
    , shrinkData
    , shrinkDatum
    )
    where

import Prelude

import Cardano.Api.Gen
    ( shrinkScriptData )
import Cardano.Ledger.Alonzo.Data
    ( Data (..), dataToBinaryData )
import Cardano.Wallet.Write.Tx
    ( BinaryData
    , Datum (..)
    , DatumHash
    , LatestLedgerEra
    , datumFromCardanoScriptData
    , datumHashFromBytes
    , datumToCardanoScriptData
    )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Ouroboros.Consensus.Cardano.Block
    ( EraCrypto, StandardCrypto )
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Plutus.V1.Ledger.Api as PV1

genDatum :: (EraCrypto era ~ StandardCrypto) => Gen (Datum era)
genDatum = oneof
    [ Datum <$> genData
    , DatumHash <$> genDatumHash
    , pure NoDatum
    ]

-- Originally from https://github.com/input-output-hk/cardano-ledger/blob/c7c63dabdb215ebdaed8b63274965966f2bf408f/eras/alonzo/test-suite/src/Test/Cardano/Ledger/Alonzo/Serialisation/Generators.hs#L66-L79
genData :: Gen (BinaryData era)
genData = dataToBinaryData . Data <$> resize 5 (sized gendata)
  where
    gendata n
      | n > 0 =
          oneof
            [ PV1.I <$> arbitrary,
              PV1.B <$> genByteString,
              PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2)),
              PV1.Constr <$> fmap (fromIntegral . abs) (arbitrary :: Gen Integer)
                <*> listOf (gendata (n `div` 2)),
              PV1.List <$> listOf (gendata (n `div` 2))
            ]
    gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> genByteString]

shrinkDatum :: Datum LatestLedgerEra -> [Datum LatestLedgerEra]
shrinkDatum (Datum x) = NoDatum : map Datum (shrinkData x)
shrinkDatum (DatumHash x) = [NoDatum]
shrinkDatum NoDatum = []

shrinkData :: BinaryData LatestLedgerEra -> [BinaryData LatestLedgerEra]
shrinkData = shrinkMapBy
    datumFromCardanoScriptData
    datumToCardanoScriptData
    shrinkScriptData

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
    . datumHashFromBytes
    . BS.pack <$> vectorOf 32 arbitrary

genByteString :: Gen ByteString
genByteString = BS.pack <$> (choose (0, 64) >>= vector)
