{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.BinarySpec (spec) where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPub (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , LinearFee (..)
    , Message (..)
    , Milli (..)
    , getBlock
    , getBlockHeader
    , runGet
    , singleAddressFromKey
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, genesis )
import Cardano.Wallet.Jormungandr.Environment
    ( Network (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , EncodeAddress (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxOut (..)
    )
import Control.Exception
    ( evaluate )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, anyErrorCall, describe, it, runIO, shouldBe, shouldThrow )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    blockBinary <- runIO . BL.readFile $ testDir <>
        "4d2324138a42a8d9e93a6b749bedeec80308ecfbc4d01383da8ac20df109e9bc.bin"

    describe "Decoding blocks" $ do
        it "should decode a genesis block header" $ do
            runGet getBlockHeader genesisBlockBinary
            `shouldBe`
            genesisHeader

        it "should decode a genesis block" $ do
            runGet getBlock genesisBlockBinary
            `shouldBe`
            genesisBlock

        it "should decode a non-genesis BFT block" $ do
            runGet getBlock blockBinary
            `shouldBe`
            Block
                BlockHeader
                    { version = 1
                    , contentSize = 0
                    , slot = SlotId {epochNumber = 797, slotNumber = 212}
                    , chainLength = 143
                    , contentHash = Hash {getHash = "\SOWQ\192&\229C\178\232\171.\176`\153\218\161\209\229\223Gw\143w\135\250\171E\205\241/\227\168"}
                    , parentHeaderHash = Hash {getHash = "\216OY\rX\199\234\188.<O\\\244Y\211\210\254\224`i\216\DC3\167\132\139\154\216\161T\174\247\155"}
                    }
                []
    describe "singleAddressFromKey" $ do
        let pub = "3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"
        let singleAddrOnMainnet = 0x3
        it "encodes (network <> tag <> key) correctly into an address" $
            singleAddressFromKey (Proxy @'Mainnet) (XPub pub cc)
            `shouldBe`
            Address (BS.pack [singleAddrOnMainnet] <> pub)
        it "throws when length (key) != 32" $
            evaluate (singleAddressFromKey (Proxy @'Mainnet) (XPub "\148" cc))
            `shouldThrow` anyErrorCall
    describe "keyToAddress is consistent with jcli" $ do
        -- The goldens have been verified with jcli as follows:
        -- $ jcli key generate --type=Ed25519 | jcli key to-public
        -- ed25519_pk1yv0er3wlzvcauqj470tesdlcwy4dll9w7cvsvn4q30678gh44tnsxdh75c
        -- $ jcli address single ed25519_pk1yv0er3wlzvcauqj470tesdlcwy4dll9w7cvsvn4q30678gh44tnsxdh75c --testing
        -- ta1sv33lyw9mufnrhsz2hea0xphlpcj4hlu4mmpjpjw5z9ltcaz7k4wwywkd6j
        -- $ jcli address single ed25519_pk1yv0er3wlzvcauqj470tesdlcwy4dll9w7cvsvn4q30678gh44tnsxdh75c
        -- ca1qv33lyw9mufnrhsz2hea0xphlpcj4hlu4mmpjpjw5z9ltcaz7k4ww0xfchg

        bech32KeyToAddrGolden
            "ed25519_pk1yv0er3wlzvcauqj470tesdlcwy4dll9w7cvsvn4q30678gh44tnsxdh75c"
            "ta1sv33lyw9mufnrhsz2hea0xphlpcj4hlu4mmpjpjw5z9ltcaz7k4wwywkd6j"
            "ca1qv33lyw9mufnrhsz2hea0xphlpcj4hlu4mmpjpjw5z9ltcaz7k4ww0xfchg"

        bech32KeyToAddrGolden
            "ed25519_pk1gf85kgfspqzgz9mvmral4e8zatg6d64ep3gk223fla09parjm7zqf4xy3n"
            "ta1sdpy7jepxqyqfqghdnv0h7hyut4drfh2hyx9zef298l4u585wt0cgmkm96t"
            "ca1qdpy7jepxqyqfqghdnv0h7hyut4drfh2hyx9zef298l4u585wt0cgs7ysh3"

        bech32KeyToAddrGolden
            "ed25519_pk16cwyfh3jy6ls8ypfpen8pk55d3ufkp9793xc96u32rl83uuv89nsxsnnhg"
            "ta1s0tpc3x7xgnt7qus9y8xvux6j3k83xcyhckymqhtj9g0u78n3sukwk7ku9u"
            "ca1q0tpc3x7xgnt7qus9y8xvux6j3k83xcyhckymqhtj9g0u78n3sukwakffgx"

bech32KeyToAddrGolden
    :: Text
    -- ^ public key ("ed25519_pk...")
    -> Text
    -- ^ corresponding testnet address ("ta...")
    -> Text
    -- ^ corresponding mainnet address ("ca...")
    -> Spec
bech32KeyToAddrGolden key testnetAddr mainnetAddr = it msg $ do
    let xpub = XPub decodeBech32Key cc
    encodeAddress (Proxy @(Jormungandr 'Testnet))
        (singleAddressFromKey (Proxy @'Testnet) xpub) `shouldBe` testnetAddr
    encodeAddress (Proxy @(Jormungandr 'Mainnet))
        (singleAddressFromKey (Proxy @'Mainnet) xpub) `shouldBe` mainnetAddr

  where
    msg = T.unpack key ++ " generates correct testnet and mainnet addresses"
    decodeBech32Key :: ByteString
    decodeBech32Key =
        let
            (hrp', dp) = either (error . show) id (Bech32.decodeLenient key)
            hrp = T.unpack $ Bech32.humanReadablePartToText hrp'
        in
            if hrp == "ed25519_pk"
            then maybe (error "invalid bech32") id (Bech32.dataPartToBytes dp)
            else error $ "failed to decode key: " ++ hrp ++ " is not ed25519_pk"

cc :: ChainCode
cc = ChainCode "<ChainCode shouldn't be used by keyToAddress>"

genesisHeader :: BlockHeader
genesisHeader = BlockHeader
    { version = 0
    , contentSize = 458
    , slot = genesis ^. #slotId
    , chainLength = 0
    , contentHash = Hash "\247\190\205\248\a\199\ACK\206\245N\196\131-*tu\145\195\242\DC4\GS\227\228\242\174\245\154\DC3\r\137\f\DC2"
    , parentHeaderHash = genesis ^. #prevBlockHash
    }

genesisBlock :: Block
genesisBlock = Block genesisHeader
    [ Initial
        [ Block0Date 1556202057
        , Discrimination Testnet
        , Consensus BFT
        , SlotsPerEpoch (Quantity 2160)
        , SlotDuration (Quantity 15)
        , EpochStabilityDepth (Quantity 10)
        , AddBftLeader (LeaderId "0\166\148\184\r\187\162\209\184\164\181VR\176=\150\&1\\\132\DC4\176T\250stE\172-*\134\\v")
        , ConsensusGenesisPraosParamF (Milli 220)
        , MaxNumberOfTransactionsPerBlock 255
        , BftSlotsRatio (Milli 220)
        , AllowAccountCreation True
        , ConfigLinearFee $ LinearFee (Quantity 0) (Quantity 0) (Quantity 0)
        ]
    , Transaction $ Tx
        { inputs = []
        , outputs =
            [ TxOut
                { address = Address "\131\&3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"
                , coin = Coin 14
                }
            ]
        }
    , UnimplementedMessage 1
    , UnimplementedMessage 3
    ]

genesisBlockBinary :: BL.ByteString
genesisBlockBinary = either error BL.fromStrict $ convertFromBase @ByteString Base16
    "00520000000001ca000000000000000000000000f7becdf807c706cef54ec4832d2a747591c3f21\
    \41de3e4f2aef59a130d890c12000000000000000000000000000000000000000000000000000000\
    \0000000000007c00000c0088000000005cc1c24900410200c2000101040000087001410f0184000\
    \0000a02e030a694b80dbba2d1b8a4b55652b03d96315c8414b054fa737445ac2d2a865c76020800\
    \000000000000dc0244000000ff028800000000000000dc034101039800000000000000000000000\
    \0000000000000000000000000002c020001833324c37869c122689a35917df53a4f2294a3a52f68\
    \5e05f5f8e53b87e7ea452f000000000000000e00620101000000000000007b005682d818584c835\
    \81c2ac3cc97bbec476496e84807f35df7349acfbaece200a24b7e26250ca20058208200581ca6d9\
    \aef475f3418967e87f7e93f20f99d8c7af406cba146affdb71910146450102030405001a89a5937\
    \100b803000004000000000000000000000000000000d501d0fa7e180d33987d17f77cbf70e1463b\
    \ce01d32d952ed6f9823f0d69eb37e35f931417c6075e0f3e5858198fe15831ba7fb51368fa2f0ac\
    \27a799032729e08a624a4aafb7a4dde35e4742d258d04c5f3ec87e616b9bcb0cdc070b503fe634b\
    \46010040a856b8a6f8d18d588b5e1cfd3ea2e56ae45b80126bb25feb8ccde27fe61ebc7fd64deb7\
    \667ab1a79ca2448f56e60f3097c2fa657febdec19e7bd7abfb0ea4705"

testDir :: FilePath
testDir = "test/data/Cardano/Wallet/Jormungandr/"
