{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xprvFromBytes, xprvToBytes )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..), UTxO (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( toSealed )
import Cardano.Wallet.Shelley.Transaction
    ( mkUnsignedTx, mkWitness, _decodeSignedTx )
import Control.Monad
    ( replicateM )
import Data.Function
    ( on )
import Data.Proxy
    ( Proxy (..) )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , property
    , scale
    , vectorOf
    , (===)
    )

import qualified Cardano.Api as Cardano
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Tx as SL

spec :: Spec
spec = do
    describe "decodeSignedTx testing" $
        it "roundtrip"
            (property prop_decodeSignedTxRoundtrip)

prop_decodeSignedTxRoundtrip
    :: DecodeSetup
    -> Property
prop_decodeSignedTxRoundtrip (DecodeSetup utxo outs slotNo pairs) = do
    let ownedIns = Map.toList $ getUTxO utxo
    let unsigned = mkUnsignedTx slotNo ownedIns outs []
    let addrWits = Set.fromList $ map (mkWitness unsigned) pairs
    let metadata = SL.SNothing
    let wits = SL.WitnessSet addrWits mempty mempty
    let ledgerTx = SL.Tx unsigned wits metadata

    _decodeSignedTx (Cardano.txSignedToCBOR (Cardano.TxSignedShelley ledgerTx))
        === Right (toSealed ledgerTx)

data DecodeSetup = DecodeSetup
    { inputs :: UTxO
    , outputs :: [TxOut]
    , ttl :: SlotNo
    , keyPasswd :: [(XPrv, Passphrase "encryption")]
    } deriving Show

instance Arbitrary DecodeSetup where
    arbitrary = do
        utxo <- arbitrary
        n <- choose (1,10)
        outs <- vectorOf n arbitrary
        slot <- arbitrary
        let numInps = Map.size $ getUTxO utxo
        pairs <- vectorOf numInps arbitrary
        pure $ DecodeSetup utxo outs slot pairs

instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> choose (1, 1000)

instance Arbitrary TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ TxIn txId ix

instance Arbitrary (Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ Hash $ BS.pack bs

instance Arbitrary Coin where
    arbitrary =
        Coin <$> choose (1, 200000)

instance Arbitrary TxOut where
    arbitrary = do
        let addr = Address $ BS.pack (1:replicate 64 0)
        TxOut addr <$> arbitrary

instance Arbitrary UTxO where
    arbitrary = do
        n <- choose (1,10)
        inps <- vectorOf n arbitrary
        let addr = Address $ BS.pack (1:replicate 64 0)
        coins <- vectorOf n arbitrary
        let outs = map (TxOut addr) coins
        pure $ UTxO $ Map.fromList $ zip inps outs

instance Arbitrary XPrv where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        let (Just xprv) = xprvFromBytes $ BS.pack $ take 96 bytes
        pure xprv

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . xprvToBytes

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

instance Arbitrary (Passphrase "raw") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "raw"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "raw"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "raw")
