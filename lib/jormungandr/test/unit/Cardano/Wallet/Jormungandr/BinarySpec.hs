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
    ( Fragment (..)
    , FragmentSpec (..)
    , TxWitnessTag (..)
    , constructTransactionFragment
    , delegationFragmentId
    , getBlock
    , getBlockHeader
    , getFragment
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
    , SealedTx (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( force )
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
import Data.Functor.Identity
    ( Identity (..) )
import Data.List
    ( isSuffixOf )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Paths_cardano_wallet_jormungandr
    ( getDataDir, getDataFileName )
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
    , shouldBe
    , shouldContain
    , shouldSatisfy
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
            bs <- BL.readFile =<< getDataFileName "jormungandr/block0.bin"
            res <- try' (runGet getBlock bs)
            res `shouldSatisfy` isRight

        describe "golden block0s generated in jormungandr-lib" $ do
            dir <- (</> "block0s") <$> runIO getDataDir
            files <- runIO $ filter (".bin" `isSuffixOf`)
                <$> getDirectoryContents dir
            forM_ files $ \filename -> do
                it ("should decode " ++ filename) $ do
                    bs <- BL.readFile (dir </> filename)
                    res <- try' (runGet getBlock bs)
                    force res `shouldSatisfy` isRight

        describe "whileM (not <$> isEmpty)" $ do
            it "should fail immediately when the decoder errors" $ do
                -- Context: We use whileM not <$> isEmpty to decode multiple
                -- block-fragments/messages. If a message-decoder is wrong, we
                -- want to know that clearly.
                let getFragment' = label "getFragment" $ isolate 3 getWord16be
                let getBlock' = whileM (not <$> isEmpty) getFragment'
                res <- try' (runGet getBlock' $ BL.pack [0,0,0])
                case res of
                    Right _ -> expectationFailure
                        "Faulty decoder scenario should not succeed"
                    Left e -> do
                        e `shouldContain`
                            "the decoder consumed 2 bytes which is less than \
                            \the expected 3 bytes"
                        e `shouldContain` "getFragment"

        it "Regression #1025" $ do
            -- Blockheader taken from current `block0.bin` in out integration
            -- test data, with a modified slot number `ffffffff` making sure it
            -- overflows way beyond Word16
            let bytes = BL.fromStrict $ unsafeFromHex
                    "005200000002d5d800000000ffffffff000000003a2954d0068a79b7fa\
                    \44d9b75d81a78e0c527b3060e923b28d6c981e9024ca28000000000000\
                    \0000000000000000000000000000000000000000000000000000"
            res <- try' (runGet getBlockHeader bytes)
            force res `shouldSatisfy` isRight

    describe "Encoding" $ do
        let pkWitness = TxWitness $ BS.pack $ [1] <> replicate 64 3
        let mkDummyWit _addr = Identity $ const pkWitness
        let mkTx ins outs = runIdentity $
                constructTransactionFragment ins outs mkDummyWit
        it "decode (encode tx) === tx standard transaction" $ property $
            \(InputsOutputs ios) -> monadicIO $ liftIO $ do
                let (hash, bin) = uncurry mkTx ios
                let decode = unFragment
                        . runGet getFragment
                        . BL.fromStrict
                let tx' = decode $ getSealedTx bin

                let originalTx =
                        let (ins, outs) = ios
                        in Tx hash (map (second coin) ins) outs
                if originalTx == tx'
                then return ()
                else expectationFailure $
                    "tx /= decode (encode tx) == " ++ show tx'

        it "empty tx has correct binary and fragmentId" $ do
            let (hash, sealed) = mkTx [] []
            (getSealedTx sealed) `shouldBe` (unsafeFromHex "000400020000")
            hash `shouldBe` (Hash $ unsafeFromHex "cab5e2102001bf73faca4a23ce1b8541cfe37fafe30d85ef41db1d8da15ec26c")

        it "decode (encode tx) === tx stake delegation transaction" $
            property $ \(StakeDelegationTx args) -> monadicIO $ liftIO $ do
                let (poolId, accId, accSig, tx@(Tx _ inps outs), wits) = args
                let encode =
                        BL.fromStrict $ getSealedTx $ snd $ withHeader
                            FragmentDelegation
                        $ putStakeDelegationTx
                            poolId accId accSig inps outs wits
                let decode =
                        getStakeDelegationTxFragment . runGet getFragment
                tx' <- try' (decode encode)
                if tx' == Right (poolId, accId, tx)
                then return ()
                else expectationFailure $
                    "tx /= decode (encode tx) == " ++ show tx'
  where
    unFragment :: Fragment -> Tx
    unFragment m = case m of
        Transaction stx -> stx
        _ -> error "expected a Transaction message"

    getStakeDelegationTxFragment
        :: Fragment -> (PoolId, ChimericAccount, Tx)
    getStakeDelegationTxFragment m = case m of
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
    StakeDelegationTx
        ( PoolId
        , ChimericAccount
        , Hash "AccountSignature"
        , Tx
        , [TxWitness]
        )
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
        accSig <- Hash . B8.pack <$> replicateM 64 arbitrary
        let tid = delegationFragmentId poolId accId accSig inps outs wits
        pure $ StakeDelegationTx (poolId, accId, accSig, Tx tid inps outs, wits)

newtype InputsOutputs = InputsOutputs ([(TxIn, TxOut)], [TxOut])
    deriving (Eq, Show, Generic)

instance Arbitrary InputsOutputs where
    arbitrary = do
        nIns <- fromIntegral <$> arbitrary @Word8
        nOut <- fromIntegral <$> arbitrary @Word8
        inps <- vectorOf nIns arbitrary
        outs <- vectorOf nOut arbitrary
        return $ InputsOutputs (inps, outs)

    shrink (InputsOutputs (inps, outs)) =
        [ InputsOutputs (inps', outs)
        | inps' <- shrinkList' inps
        ]
        ++
        [ InputsOutputs (inps, outs')
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
