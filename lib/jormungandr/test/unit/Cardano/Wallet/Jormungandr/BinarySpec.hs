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
    , putTxWitnessTag
    , runGet
    , runPut
    , txWitnessSize
    , withHeader
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , Depth (AddressK)
    , Passphrase (..)
    , XPrv
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, generateKeyFromSeed )
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

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteArray as BA
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
                        $ putSignedTx mempty inps outs wits
                let decode =
                        unMessage . runGet getMessage
                tx' <- try' (decode $ encode signedTx)
                if tx' == Right signedTx
                then return ()
                else expectationFailure $
                    "tx /= decode (encode tx) == " ++ show tx'

--         it "decode (encode tx) === tx stake delegation transaction" $
--             property $ \(StakeDelegationTx args) -> monadicIO $ liftIO $ do
--                 let (poolId, (accountXPrv, pass), rawTx@(Tx _ inps outs), wits) = args
--
--                 let accountPublicKey = BS.take 32 $ CC.unXPub . getRawKey . publicKey $ accountXPrv
--                 let accId = ChimericAccount accountPublicKey
--                 let encode = runPut
--                         $ putStakeDelegationTx
--                             poolId
--                             accId
--                             inps
--                             outs
--                             wits
--                             (sign (accountXPrv, pass))
--                 let decode =
--                         getStakeDelegationTxMessage . runGet getMessage
--                 let tx = (poolId, accId, rawTx, wits)
--                 tx' <- try' (decode encode)
--                 if tx' == Right tx
--                 then return ()
--                 else expectationFailure $
--                     "tx /= decode (encode tx)" ++
--                     show tx' ++ "\n /= \n" ++
--                     show tx
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

newtype StakeDelegationTx =
    StakeDelegationTx
        ( PoolId
        , (ShelleyKey 'AddressK XPrv, Passphrase "encryption")
        , Tx
        , [TxWitness]
        )
    deriving (Eq, Show, Generic)

instance Eq XPrv where
   a == b = f a == f b
     where
       f x = CC.sign pass x msg
       msg = "some message" :: ByteString
       pass = mempty :: ByteString


instance Show XPrv where
    show _ = "<XPrv>"

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
        keyAndPass <- arbitrary
        poolId <- arbitrary
        accId <- ChimericAccount . B8.pack <$> replicateM 32 arbitrary
        accSig <- Hash . B8.pack <$> replicateM 64 arbitrary
        let tid = delegationFragmentId poolId accId accSig inps outs wits
        pure $ StakeDelegationTx (poolId, keyAndPass, Tx tid inps outs, wits)

instance {-# OVERLAPS #-} Arbitrary (ShelleyKey 'AddressK XPrv, Passphrase "encryption")
  where
    shrink _ = []
    arbitrary = do
        seed <- Passphrase . BA.convert . BS.pack <$> replicateM 32 arbitrary
        pwd <- arbitrary
        let root = generateKeyFromSeed (seed, mempty) pwd
        let acc = deriveAccountPrivateKey pwd root minBound
        let key = deriveAddressPrivateKey pwd acc MutableAccount minBound

        return (key, pwd)

instance Arbitrary (Passphrase purpose) where
    shrink _ = []
    arbitrary =
        Passphrase . BA.convert . BS.pack <$> replicateM 16 arbitrary

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
