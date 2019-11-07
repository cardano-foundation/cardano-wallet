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
    ( Message (..)
    , MessageType (..)
    , TxWitnessTag (..)
    , delegationFragmentId
    , fragmentId
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
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , ChimericAccount (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Control.Exception
    ( SomeException, evaluate, try )
import Control.Monad
    ( forM_, replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Loops
    ( whileM )
import Data.Binary.Get
    ( getWord16be, isEmpty, isolate, label )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.List
    ( isSuffixOf )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import System.Directory
    ( getDirectoryContents )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , runIO
    , shouldContain
    , shouldSatisfy
    , xit
    )
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = do
    describe "Decoding" $ do
        it "should decode the block0.bin used for integration tests" $ do
            bs <- BL.readFile $ "test" </> "data" </> "jormungandr" </> "block0.bin"
            res <- try' (runGet getBlock bs)
            res `shouldSatisfy` isRight

        describe "golden block0s generated in jormungandr-lib" $ do
            let dir = "test" </> "data" </> "block0s"
            files <- runIO $ filter (".bin" `isSuffixOf`)
                <$> getDirectoryContents dir
            forM_ files $ \filename -> do
                it ("should decode " ++ filename) $ do
                    bs <- BL.readFile (dir </> filename)
                    res <- try' (runGet getBlock bs)
                    res `shouldSatisfy` isRight
                    return ()
        describe "whileM (not <$> isEmpty)" $ do
            it "should fail immediately when the decoder errors" $ do
                -- Context: We use whileM not <$> isEmpty to decode multiple
                -- block-fragments/messages. If a message-decoder is wrong, we
                -- want to know that clearly.
                let getMessage' = label "getMessage" $ isolate 3 getWord16be
                let getBlock' = whileM (not <$> isEmpty) getMessage'
                res <- try' (runGet getBlock' $ BL.pack [0,0,0])
                case res of
                    Right _ -> expectationFailure
                        "Faulty decoder scenario should not succeed"
                    Left e -> do
                        e `shouldContain`
                            "the decoder consumed 2 bytes which is less than \
                            \the expected 3 bytes"
                        e `shouldContain` "getMessage"


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

        xit "decode (encode tx) === tx stake delegation transaction" $
            property $ \(StakeDelegationTx stakeDelTx) -> monadicIO $ liftIO $ do
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
        StakeDelegation stx -> stx
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
