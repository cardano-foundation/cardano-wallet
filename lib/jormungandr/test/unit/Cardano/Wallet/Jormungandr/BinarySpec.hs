{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.BinarySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPub (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , Message (..)
    , MessageType (..)
    , Milli (..)
    , fragmentId
    , getAddress
    , getBlock
    , getMessage
    , putAddress
    , putSignedTx
    , runGet
    , runPut
    , singleAddressFromKey
    , withHeader
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, block0 )
import Cardano.Wallet.Jormungandr.Environment
    ( Network (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , StartTime (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress, unsafeDecodeHex, unsafeFromHex )
import Control.Exception
    ( SomeException, evaluate, try )
import Control.Monad.IO.Class
    ( liftIO )
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
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe, shouldThrow )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, oneof, property, shrinkList, vectorOf )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Decoding" $ do
        it "should decode a genesis block (Testnet)" $ do
            let proxy = Proxy @(Jormungandr 'Testnet)
            let bytes =
                    "00520000000001ca000000000000000000000000f7becdf807c706cef5\
                    \4ec4832d2a747591c3f2141de3e4f2aef59a130d890c12000000000000\
                    \0000000000000000000000000000000000000000000000000000007c00\
                    \000c0088000000005cc1c24900410200c2000101040000087001410f01\
                    \840000000a02e030a694b80dbba2d1b8a4b55652b03d96315c8414b054\
                    \fa737445ac2d2a865c76020800000000000000dc0244000000ff028800\
                    \000000000000dc03410103980000000000000000000000000000000000\
                    \00000000000000002c020001833324c37869c122689a35917df53a4f22\
                    \94a3a52f685e05f5f8e53b87e7ea452f000000000000000e0062010100\
                    \0000000000007b005682d818584c83581c2ac3cc97bbec476496e84807\
                    \f35df7349acfbaece200a24b7e26250ca20058208200581ca6d9aef475\
                    \f3418967e87f7e93f20f99d8c7af406cba146affdb7191014645010203\
                    \0405001a89a5937100b803000004000000000000000000000000000000\
                    \d501d0fa7e180d33987d17f77cbf70e1463bce01d32d952ed6f9823f0d\
                    \69eb37e35f931417c6075e0f3e5858198fe15831ba7fb51368fa2f0ac2\
                    \7a799032729e08a624a4aafb7a4dde35e4742d258d04c5f3ec87e616b9\
                    \bcb0cdc070b503fe634b46010040a856b8a6f8d18d588b5e1cfd3ea2e5\
                    \6ae45b80126bb25feb8ccde27fe61ebc7fd64deb7667ab1a79ca2448f5\
                    \6e60f3097c2fa657febdec19e7bd7abfb0ea4705"
            let block = Block
                    BlockHeader
                        { version = 0
                        , contentSize = 458
                        , slot = block0 ^. #slotId
                        , chainLength = 0
                        , contentHash = Hash $ unsafeFromHex
                            "f7becdf807c706cef54ec4832d2a7475\
                            \91c3f2141de3e4f2aef59a130d890c12"
                        , parentHeaderHash = block0 ^. #prevBlockHash
                        }
                    [ Initial
                        [ Block0Date (StartTime $ posixSecondsToUTCTime 1556202057)
                        , Discrimination Testnet
                        , Consensus BFT
                        , SlotsPerEpoch $ W.EpochLength 2160
                        , SlotDuration 15
                        , EpochStabilityDepth (Quantity 10)
                        , AddBftLeader $ LeaderId $ unsafeFromHex
                            "30a694b80dbba2d1b8a4b55652b03d96\
                            \315c8414b054fa737445ac2d2a865c76"
                        , ConsensusGenesisPraosParamF (Milli 220)
                        , MaxNumberOfTransactionsPerBlock 255
                        , BftSlotsRatio (Milli 220)
                        , AllowAccountCreation True
                        , ConfigLinearFee $ LinearFee (Quantity 0) (Quantity 0)
                        ]
                    , Transaction (Tx
                        { txid = Hash $ unsafeFromHex
                            "6f5e01c34590f5ead789c234a816ac25\
                            \41baece53f6e51bf52222bd7feb5cd04"
                        , inputs = []
                        , outputs =
                            [ TxOut
                                { address = unsafeDecodeAddress proxy
                                    "ta1svejfsmcd8qjy6y6xkghmaf6fu3f\
                                    \fga99a59up04lrjnhpl8afzj7w4yyxw"
                                , coin = Coin 14
                                }
                            ]
                        }, [])
                    , UnimplementedMessage 1
                    , UnimplementedMessage 3
                    ]
            unsafeDecodeHex getBlock bytes `shouldBe` block

        it "should decode a genesis block (Mainnet)" $ do
            let bytes =
                    "00520000000000810000000000000000000000005df3b1c19c1400a992\
                    \5158ade2b7191374df85f6976fe81681f0aef2e0ddc2a3000000000000\
                    \0000000000000000000000000000000000000000000000000000007f00\
                    \000c0088000000005cc1c24900410100c200010104000001f401410a01\
                    \840000000a02e0b216ee388fc25596cf43fbca815c463c37d79560b985\
                    \800c3d6f3a0f5a977e32020800000000000000dc0244000000ff028800\
                    \000000000000dc03980000000000000000000000000000000000000000\
                    \00000000040400000010"
            let block = Block
                    BlockHeader
                        { version = 0
                        , contentSize = 129
                        , slot = block0 ^. #slotId
                        , chainLength = 0
                        , contentHash = Hash $ unsafeFromHex
                            "5df3b1c19c1400a9925158ade2b71913\
                            \74df85f6976fe81681f0aef2e0ddc2a3"
                        , parentHeaderHash = block0 ^. #prevBlockHash
                        }
                    [ Initial
                        [ Block0Date (StartTime $ posixSecondsToUTCTime 1556202057)
                        , Discrimination Mainnet
                        , Consensus BFT
                        , SlotsPerEpoch (W.EpochLength 500)
                        , SlotDuration 10
                        , EpochStabilityDepth (Quantity 10)
                        , AddBftLeader $ LeaderId $ unsafeFromHex
                            "b216ee388fc25596cf43fbca815c463c\
                            \37d79560b985800c3d6f3a0f5a977e32"
                        , ConsensusGenesisPraosParamF (Milli 220)
                        , MaxNumberOfTransactionsPerBlock 255
                        , BftSlotsRatio (Milli 220)
                        , ConfigLinearFee $ LinearFee (Quantity 0) (Quantity 0)
                        , KesUpdateSpeed (Quantity 16)
                        ]
                    ]
            unsafeDecodeHex getBlock bytes `shouldBe` block

        it "should decode a non-genesis BFT block (no transactions)" $ do
            let bytes =
                    "00b20001000000000000031d000000d40000008f0e5751c026e543b2e8\
                    \ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8d84f590d58c7\
                    \eabc2e3c4f5cf459d3d2fee06069d813a7848b9ad8a154aef79bb216ee\
                    \388fc25596cf43fbca815c463c37d79560b985800c3d6f3a0f5a977e32\
                    \56818aa41fca8cbd5e18b6b1a97e8f6fc0fdfb209a14106e18088e8e40\
                    \8f6262ae55c39accca704c1ac8c0dfffc66d4848c0b911202535bc33f6\
                    \3d326cf9b504"
            let block = Block
                    BlockHeader
                        { version = 1
                        , contentSize = 0
                        , slot = SlotId {epochNumber = 797, slotNumber = 212}
                        , chainLength = 143
                        , contentHash = Hash $ unsafeFromHex
                            "0e5751c026e543b2e8ab2eb06099daa1\
                            \d1e5df47778f7787faab45cdf12fe3a8"
                        , parentHeaderHash = Hash $ unsafeFromHex
                            "d84f590d58c7eabc2e3c4f5cf459d3d2\
                            \fee06069d813a7848b9ad8a154aef79b"
                        }
                    []
            unsafeDecodeHex getBlock bytes `shouldBe` block

    describe "Encoding" $ do
        let cc = ChainCode "<ChainCode is not used by singleAddressToKey>"
        let mainnet = Proxy @'Mainnet
        let testnet = Proxy @'Testnet

        let userException str (e :: SomeException) = show e == str

        it "throws when encoding XPub of invalid length (Mainnet)" $ do
            let msg = "length was 1, but expected to be 32"
            evaluate (singleAddressFromKey mainnet (XPub "\148" cc))
                `shouldThrow` userException msg

        it "throws when encoding XPub of invalid length (Testnet)" $ do
            let msg = "length was 1, but expected to be 32"
            evaluate (singleAddressFromKey testnet (XPub "\148" cc))
                `shouldThrow` userException msg

        it "throws when encoding an address of invalid length" $ do
            let msg = "Address has unexpected length 1: \
                \Address {unAddress = \"0\"}"
            evaluate (runPut $ putAddress $ Address "0")
                `shouldThrow` userException msg

        it "decode (encode address) === address" $ property $
            \addr -> monadicIO $ liftIO $ do
                let encode = runPut . putAddress
                let decode = runGet getAddress
                addr' <- try' (decode $ encode addr)
                if addr' == Right addr
                then return ()
                else expectationFailure $
                    "addr /= decode (encode addr) == " ++ show addr'

        it "decode (encode tx) === tx" $ property $
            \(SignedTx signedTx) -> monadicIO $ liftIO $ do
                let encode ((Tx _ inps outs), wits) = runPut
                        $ withHeader MsgTypeTransaction
                        $ putSignedTx inps outs wits
                let decode =
                        unMessage . runGet getMessage
                tx' <- try' (decode $ encode signedTx)
                if tx' == Right signedTx
                then return ()
                else expectationFailure $
                    "tx /= decode (encode tx) == " ++ show tx'

  where
    unMessage :: Message -> (Tx, [TxWitness])
    unMessage m = case m of
        Transaction stx -> stx
        _ -> error "expected a Transaction message"

    try' :: a -> IO (Either String a)
    try' = fmap (either (Left . show) Right)
        . (try @SomeException) . evaluate


-- Only generating single addresses!
instance Arbitrary Address where
    arbitrary = Address . prependTag 3 <$> genFixed 32
    shrink (Address addr) = Address . prependTag 3
        <$> shrinkFixedBS (BS.tail addr)

-- Observation:
-- genFixed and shrinkFixed would be nice candidates for DerivingVia.
-- e.g.
-- deriving instance Arbitrary Address via (ByteStringOfLength @33)
genFixed :: Int -> Gen BS.ByteString
genFixed n = BS.pack <$> (vectorOf n arbitrary)

shrinkFixedBS :: ByteString -> [ByteString]
shrinkFixedBS bs = [zeros | bs /= zeros]
      where
        len = BS.length bs
        zeros = BS.pack (replicate len 0)

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash <$> genFixed 32
    shrink (Hash bytes) = Hash <$> shrinkFixedBS bytes

instance Arbitrary Coin where
    arbitrary = do
        n <- choose (0, 100)
        oneof [
            return $ Coin n
            , return $ Coin (getCoin (maxBound :: Coin) - n)
            , Coin <$> choose (0, getCoin (maxBound :: Coin))
            ]
    shrink (Coin c) = map Coin (shrink c)

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> arbitrary
        <*> (fromIntegral <$> arbitrary @Word8)
    shrink = genericShrink

instance Arbitrary TxOut where
    arbitrary = genericArbitrary
    shrink = genericShrink

newtype SignedTx = SignedTx (Tx, [TxWitness])
    deriving (Eq, Show, Generic)

instance Arbitrary SignedTx where
    arbitrary = do
        nIns <- fromIntegral <$> arbitrary @Word8
        nOut <- fromIntegral <$> arbitrary @Word8
        inps <- vectorOf nIns arbitrary
        outs <- vectorOf nOut arbitrary
        wits <- vectorOf nIns arbitrary
        let tid = fragmentId inps outs wits
        return $ SignedTx (Tx tid inps outs, wits)

    shrink (SignedTx (Tx _ inps outs, wits)) =
        [ SignedTx (Tx (fragmentId inps' outs wits') inps' outs, wits')
        | (inps', wits') <- unzip <$> shrinkList' (zip inps wits)
        ]
        ++
        [ SignedTx (Tx (fragmentId inps outs' wits) inps outs', wits)
        | outs' <- shrinkList' outs
        ]

      where
        shrinkList' xs  =
            (shrinkHeadAndReplicate shrink xs) ++
            (shrinkList shrink xs)

        -- Try shrinking the 'head' of the list and replace the elements in
        -- the 'tail' with the exact same element. If the failure is related
        -- to the size of the list, this makes the shrinking much faster.
        shrinkHeadAndReplicate f (x:xs) =
            (\x' -> x':(map (const x') xs)) <$> f x
        shrinkHeadAndReplicate _f [] = []

-- | Only generates single address witnesses
instance Arbitrary TxWitness where
    arbitrary = TxWitness <$> genFixed 64
    shrink (TxWitness bytes) = TxWitness <$> shrinkFixedBS bytes

prependTag :: Int -> ByteString -> ByteString
prependTag tag bs = BS.pack [fromIntegral tag] <> bs
