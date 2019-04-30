{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.SigningSpec where

import Prelude

import Cardano.Environment
    ( Network (..), network )
import Cardano.Wallet.Binary
    ( decodeSignedTx, encodeSignedTx, toByteString )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressScheme (..) )
import Cardano.Wallet.Primitive.Signing
    ( mkStdTx )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , Tx
    , TxIn (..)
    , TxOut (..)
    , TxWitness
    )
import Data.Map
    ( Map )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T

signedTx :: (Tx, [TxWitness])
signedTx =
    let
        seed = Passphrase $ BA.convert $ BS.pack $ replicate 128 0
        pass = Passphrase mempty
        addrXPrv = unsafeGenerateKeyFromSeed (seed, mempty) pass
        addr = keyToAddress $ publicKey addrXPrv
        h = Hash "\bztb\249\223n5\189A\239Y&\243tgv\253\223\140\222i\236O\221\&8\205\218>\234\240\238"

        ownedIns = (TxIn h 0, TxOut addr (Coin 1) )
        outs = [TxOut addr (Coin 1)]

        m = (Map.singleton addr addrXPrv) :: Map Address (Key 'AddressK XPrv)

        fromR = either undefined Prelude.id
    in
        fromR $ mkStdTx m (error "no root key", pass) [ownedIns] outs

-- | Reads different fixtures depending on the network.
--
-- Fixtures were generated using 'wallet/server/Main.hs' on the
-- 'anviking/wallet-tx-golden' branch on cardano-sl.
readFixture :: String -> IO BS.ByteString
readFixture name
    = BS.readFile
        $ "test/data/Cardano/Wallet/Primitive/Signing/"
          ++ T.unpack (toText network') ++ "/" ++ name
  where
    network' =
        case network of
            Mainnet -> Mainnet
            Staging -> Mainnet
            Testnet -> Testnet
            Local -> Testnet

spec :: Spec
spec = do
    describe "Compare signed transactions with cardano-sl" $ do
        it "slAddress xpub == walletAddress xpub" $ do
            bin <- readFixture "sl-addr.bin"
            let seed = Passphrase $ BA.convert $ BS.pack $ replicate 128 0
            let pass = Passphrase mempty
            let addrXPrv = unsafeGenerateKeyFromSeed (seed, mempty) pass
            let (Address addr) = keyToAddress $ publicKey addrXPrv
            addr `shouldBe` bin


        it "decodeSignedTx slBinary == walletTx" $ do
            bin <- readFixture "sl-signedTx.bin"

            case CBOR.deserialiseFromBytes decodeSignedTx (BL.fromStrict bin) of
                Left err -> error $ show err
                Right (_, decodedSlTx) -> decodedSlTx `shouldBe` signedTx

        it "encodeSignedTx walletTx == slBinary" $ do
            bin <- readFixture "sl-signedTx.bin"
            toByteString (encodeSignedTx signedTx) `shouldBe` bin

instance AddressScheme (Map Address (Key 'AddressK XPrv)) where
    keyFrom addr _ = Map.lookup addr
    nextChangeAddress = error "unimplemented"

