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

import Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , Message (..)
    , MessageType (..)
    , Milli (..)
    , TxWitnessTag (..)
    , delegationFragmentId
    , fragmentId
    , getAddress
    , getBlock
    , getMessage
    , putSignedTx
    , putStakeDelegationTx
    , putTxWitnessTag
    , runGet
    , runPut
    , txWitnessSize
    , withHeader
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , ChimericAccount (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , StartTime (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeHex, unsafeFromHex )
import Control.Exception
    ( SomeException, evaluate, try )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.List
    ( isSuffixOf )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import System.Directory
    ( getDirectoryContents )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, runIO, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , choose
    , oneof
    , property
    , shrinkList
    , vectorOf
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = do
    describe "Decoding" $ do
        it "should decode a genesis block (Testnet)" $ do
            let bytes =
                    "00520000000001950000000000000000000000006fb1140e69d3acdd82d\
                    \682b1a4a52a2bb00e182c645b5af775a8115814779e7200000000000000\
                    \00000000000000000000000000000000000000000000000000007f00000\
                    \c0088000000005cc1c24900410200c200020398000000000000002a0000\
                    \000000000000000000000000000001040000000301410104040000a8c00\
                    \20800000000000003e8028800000000000000000244000000ff01840000\
                    \000402e06555f34e4de7ef3b01e889dba07a399b3a3ea0c36f21184d014\
                    \39af96c4442eb002c02000185877fbb2283ba1ed56d835fb8cd66694a84\
                    \0360e69c690a04aeef39629cdd804f0000000000000001009f050000000\
                    \000000000000000003c34eb12000000000000000000010001643b112bac\
                    \cbfb2298cdd2e02dea6a04fb4c0791cfd62e26f7de3afa073c284700000\
                    \00000000000000000000000000000000000000000010000000000000000\
                    \4666e022f961efc82c507a7b8654b5727d6c5ea40bb44402004b9d17de0\
                    \ea52ca41576922d5ca087c85df4f5d2de844282993df3534d45f0b9795d\
                    \bdc3cde73d0000004304877fbb2283ba1ed56d835fb8cd66694a840360e\
                    \69c690a04aeef39629cdd804fa09ae0da1e618eeb09e7b78d73e265af18\
                    \f87d4d5320386ebf0235f54ecd03470000"
            let block = Block
                    BlockHeader
                        { version = 0
                        , contentSize = 405
                        , slot = SlotId 0 0
                        , chainLength = 0
                        , contentHash = Hash $ unsafeFromHex
                            "6fb1140e69d3acdd82d682b1a4a52a2bb0\
                            \0e182c645b5af775a8115814779e72"
                        , headerHash = Hash $ unsafeFromHex
                            "3d7a861feff6d266e07c6fc2e0f41a842\
                            \3fdf6e54db38de9b7c0a975c3b8cebe"
                        , parentHeaderHash = Hash (BS.replicate 32 0)
                        , producedBy = Nothing
                        }
                    [ Initial
                        [ Block0Date (StartTime $ posixSecondsToUTCTime 1556202057)
                        , Discrimination Testnet
                        , Consensus GenesisPraos
                        , ConfigLinearFee $ LinearFee (Quantity 42) (Quantity 0) (Quantity 0)
                        , SlotsPerEpoch (W.EpochLength 3)
                        , SlotDuration 1
                        , KesUpdateSpeed (Quantity 43200)
                        , ConsensusGenesisPraosParamF (Milli 1000)
                        , BftSlotsRatio (Milli 0)
                        , MaxNumberOfTransactionsPerBlock 255
                        , EpochStabilityDepth (Quantity 4)
                        , AddBftLeader $ LeaderId $ unsafeFromHex
                            "6555f34e4de7ef3b01e889dba07a399b\
                            \3a3ea0c36f21184d01439af96c4442eb"
                        ]
                    , Transaction (Tx
                        { txid = Hash $ unsafeFromHex
                            "30b99c425ca5aa64e24f23b5cef542\
                            \170ac96ea32ea823904f9446cd49966013"
                        , inputs = []
                        , outputs =
                            [ TxOut
                                { address = Address $ unsafeFromHex
                                    "85877fbb2283ba1ed56d835fb8cd66694a8\
                                    \40360e69c690a04aeef39629cdd804f"
                                , coin = Coin 1
                                }
                            ]
                        }, [])
                    , UnimplementedMessage 5
                    , TransactionWithDelegation (
                            PoolId (unsafeFromHex
                                    "a09ae0da1e618eeb09e7b78d73e265af18f8\
                                    \7d4d5320386ebf0235f54ecd0347")
                            , ChimericAccount (unsafeFromHex
                                    "877fbb2283ba1ed56d835fb8cd66694a840\
                                    \360e69c690a04aeef39629cdd804f")
                            , Tx
                              { txid = Hash $ unsafeFromHex
                                  "4ab1923eb7e84ab2ac73769ff863138cce3a7\
                                  \1ba9f4d2533a007d36496aa58c9"
                              , inputs = []
                              , outputs = []
                              }
                            , []
                        )
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
                        , slot = SlotId 0 0
                        , chainLength = 0
                        , contentHash = Hash $ unsafeFromHex
                            "5df3b1c19c1400a9925158ade2b71913\
                            \74df85f6976fe81681f0aef2e0ddc2a3"
                        , headerHash = Hash $ unsafeFromHex
                            "2c0d36bfc65bb59c5c7df68e7b70dab8\
                            \347b96802a343e609258573fd1155c24"
                        , parentHeaderHash = Hash (BS.replicate 32 0)
                        , producedBy = Nothing
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
                        , ConfigLinearFee $ LinearFee (Quantity 0) (Quantity 0) (Quantity 0)
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
                        , headerHash = Hash $ unsafeFromHex
                            "4d2324138a42a8d9e93a6b749bedeec8\
                            \0308ecfbc4d01383da8ac20df109e9bc"
                        , parentHeaderHash = Hash $ unsafeFromHex
                            "d84f590d58c7eabc2e3c4f5cf459d3d2\
                            \fee06069d813a7848b9ad8a154aef79b"
                        , producedBy = Nothing
                        }
                    []
            unsafeDecodeHex getBlock bytes `shouldBe` block

        it "should decode a non-genesis Praos/Genesis block" $ do
            let dir = "test/data/Cardano/Wallet/Jormungandr/BinarySpec"
            bs <- BL.readFile (dir ++ "/genesis-praos-block.bin")
            res <- try' (runGet getBlock bs)
            res `shouldSatisfy` isRight
            return ()

        it "should decode an account address golden" $ do
            -- This address was manually retrieved from a jcli-created genesis
            -- block.
            let hex = "850C0DF2B9A4CD3E8CD36B81BE295EF2A\
                      \B5BA25929A36359F84CD1AA5CEBE2FAED"
            res <- try' (runGet getAddress (BL.fromStrict $ unsafeFromHex hex))
            res `shouldSatisfy` isRight

        describe "golden block0s generated in jormungandr-lib" $ do
            let dir = "test/data/block0s"
            files <- runIO $ filter (".bin" `isSuffixOf`)
                <$> getDirectoryContents dir
            forM_ files $ \filename -> do
                it ("should decode " ++ filename) $ do
                    bs <- BL.readFile (dir ++ "/" ++ filename)
                    res <- try' (runGet getBlock bs)
                    res `shouldSatisfy` isRight
                    return ()

    describe "Encoding" $ do
        it "decode (encode tx) === tx standard transaction" $ property $
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

        it "decode (encode tx) === tx stake delegation transaction" $ property $
            \(StakeDelegationTx stakeDelTx) -> monadicIO $ liftIO $ do
                let encode (poolId, pubKey, (Tx _ inps outs), wits) =
                          runPut
                        $ withHeader MsgTypeDelegation
                        $ putStakeDelegationTx poolId pubKey inps outs wits
                let decode =
                        getStakeDelegationTxMessage . runGet getMessage
                tx' <- try' (decode $ encode stakeDelTx)
                if tx' == Right stakeDelTx
                then return ()
                else expectationFailure $
                    "tx /= decode (encode tx) == " ++ show tx'
  where
    unMessage :: Message -> (Tx, [TxWitness])
    unMessage m = case m of
        Transaction stx -> stx
        _ -> error "expected a Transaction message"

    getStakeDelegationTxMessage :: Message -> (PoolId, ChimericAccount, Tx, [TxWitness])
    getStakeDelegationTxMessage m = case m of
        TransactionWithDelegation stx -> stx
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

newtype StakeDelegationTx =
    StakeDelegationTx (PoolId, ChimericAccount, Tx, [TxWitness])
    deriving (Eq, Show, Generic)

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes

instance Arbitrary StakeDelegationTx where
    arbitrary = do
        nIns <- fromIntegral <$> arbitrary @Word8
        nOut <- fromIntegral <$> arbitrary @Word8
        inps <- vectorOf nIns arbitrary
        outs <- vectorOf nOut arbitrary
        wits <- vectorOf nIns arbitrary
        poolId <- arbitrary
        accId <- ChimericAccount . B8.pack <$> replicateM 32 arbitrary
        let tid = delegationFragmentId poolId accId inps outs wits
        return $ StakeDelegationTx (poolId, accId, Tx tid inps outs, wits)

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
    arbitrary = taggedWitness TxWitnessUTxO . TxWitness
        <$> genFixed (txWitnessSize TxWitnessUTxO)

prependTag :: Int -> ByteString -> ByteString
prependTag tag bs = BS.pack [fromIntegral tag] <> bs

taggedWitness :: TxWitnessTag -> TxWitness -> TxWitness
taggedWitness tag (TxWitness bytes) = TxWitness (prefix <> bytes)
  where
    prefix = BL.toStrict $ runPut $ putTxWitnessTag tag
