{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Write.Tx.Gen
    ( genDatum
    , genBinaryData
    , genDatumHash
    , shrinkBinaryData
    , shrinkDatum
    , genTxOut
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.Scripts.Data
    ( Data (..), dataToBinaryData )
import Cardano.Wallet.Write.Tx
    ( BinaryData
    , Datum (..)
    , DatumHash
    , LatestLedgerEra
    , RecentEra
    , ShelleyLedgerEra
    , TxOut
    , cardanoEraFromRecentEra
    , datumFromCardanoScriptData
    , datumHashFromBytes
    , datumToCardanoScriptData
    , shelleyBasedEraFromRecentEra
    , withConstraints
    )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , choose
    , listOf
    , oneof
    , scale
    , shrinkMapBy
    , sized
    , vector
    , vectorOf
    )

import qualified Cardano.Api.Gen as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Data.ByteString as BS
import qualified PlutusLedgerApi.V1 as PV1

genDatum :: RecentEra era -> Gen (Datum (ShelleyLedgerEra era))
genDatum recentEra = withConstraints recentEra $
    oneof
        [ Datum <$> genBinaryData recentEra
        , DatumHash <$> genDatumHash
        , pure NoDatum
        ]

-- Originally from https://github.com/input-output-hk/cardano-ledger/blob/c7c63dabdb215ebdaed8b63274965966f2bf408f/eras/alonzo/test-suite/src/Test/Cardano/Ledger/Alonzo/Serialisation/Generators.hs#L66-L79
genBinaryData :: RecentEra era -> Gen (BinaryData (ShelleyLedgerEra era))
genBinaryData recentEra = withConstraints recentEra $
    dataToBinaryData . Data <$> scale (`div` 10) (sized gendata)
  where
    gendata n | n > 0 = oneof
        [ PV1.I <$> arbitrary
        , PV1.B <$> genByteString
        , PV1.Map
            <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2))
        , PV1.Constr
            <$> fmap abs (arbitrary :: Gen Integer)
            <*> listOf (gendata (n `div` 2))
        , PV1.List
            <$> listOf (gendata (n `div` 2))
        ]
    gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> genByteString]

shrinkDatum :: Datum LatestLedgerEra -> [Datum LatestLedgerEra]
shrinkDatum (Datum x) = NoDatum : map Datum (shrinkBinaryData _recentEra x)
shrinkDatum (DatumHash _) = [NoDatum]
shrinkDatum NoDatum = []

shrinkBinaryData :: RecentEra era -> BinaryData era -> [BinaryData era]
shrinkBinaryData recentEra = withConstraints recentEra $
    shrinkMapBy
        datumFromCardanoScriptData
        datumToCardanoScriptData $
        shrinkMapBy
            Cardano.unsafeHashableScriptData
            Cardano.getScriptData
            Cardano.shrinkScriptData

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
    . datumHashFromBytes
    . BS.pack <$> vectorOf 32 arbitrary

genByteString :: Gen ByteString
genByteString = BS.pack <$> (choose (0, 64) >>= vector)

genTxOut :: RecentEra era -> Gen (TxOut (ShelleyLedgerEra era))
genTxOut era = Cardano.toShelleyTxOut (shelleyBasedEraFromRecentEra era)
    <$> Cardano.genTxOut (cardanoEraFromRecentEra era)
