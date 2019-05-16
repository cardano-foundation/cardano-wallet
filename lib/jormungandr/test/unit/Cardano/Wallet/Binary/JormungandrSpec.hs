{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Binary.JormungandrSpec (spec) where

import Prelude

import Cardano.Environment.Jormungandr
    ( Network (..) )
import Cardano.Wallet.Binary.Jormungandr
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
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), SlotId (..), Tx (..), TxOut (..) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.ByteString.Lazy as BL

{-# ANN spec ("HLint: ignore Use head" :: String) #-}
spec :: Spec
spec = do
    describe "Decoding blocks" $ do
        it "should decode a genesis block header" $ do
            runGet getBlockHeader genesisBlockBinary
            `shouldBe`
            genesisHeader

        it "should decode a genesis block" $ do
            runGet getBlock genesisBlockBinary
            `shouldBe`
            genesisBlock



genesisHeader :: BlockHeader
genesisHeader = BlockHeader
    { version = 0
    , contentSize = 458
    , slot = SlotId {epochNumber = 0 , slotNumber = 0}
    , chainLength = 0
    , contentHash = Hash "\247\190\205\248\a\199\ACK\206\245N\196\131-*tu\145\195\242\DC4\GS\227\228\242\174\245\154\DC3\r\137\f\DC2"
    , parentHeaderHash = Hash "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
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
                { address = Address "3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"
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
