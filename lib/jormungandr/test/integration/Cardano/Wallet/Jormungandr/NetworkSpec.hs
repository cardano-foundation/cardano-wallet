{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Faucet
    ( block0H )
import Cardano.Wallet.Jormungandr.Api
    ( GetTipId, api )
import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..), fragmentId, putSignedTx, runPut, withHeader )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..), block0 )
import Cardano.Wallet.Jormungandr.Launch
    ( launchJormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetDescendants (..)
    , ErrUnexpectedNetworkFailure (..)
    , Scheme (..)
    )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , NetworkLayer (..)
    , defaultRetryPolicy
    , waitForConnection
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (..)
    , Coin (..)
    , EpochLength (..)
    , EpochSlotId (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    , epochSlotIdToSlotId
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress, unsafeFromHex, unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, cancel )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( bracket )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Retry
    ( limitRetries, retrying )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Functor
    ( ($>) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word64, Word8 )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Servant.Links
    ( safeLink )
import System.Process
    ( StdStream (..) )
import Test.Hspec
    ( Spec
    , afterAll
    , anyException
    , beforeAll
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , generate
    , oneof
    , property
    , shrinkList
    , vectorOf
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Ports
    ( randomUnusedTCPPorts )

import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    let startNode' = startNode (`waitForConnection` defaultRetryPolicy)
    let once = limitRetries 1
    describe "Happy Paths" $ beforeAll startNode' $ afterAll killNode $ do
        it "get network tip" $ \(_, nw, _) -> do
            resp <- runExceptT $ networkTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId <$> resp
            let (Right height) = blockHeight <$> resp
            slot `shouldSatisfy` (>= minBound)
            height `shouldSatisfy` (>= Quantity 0)

        it "get some blocks from the genesis" $ \(_, nw, _) -> do
            threadDelay (10 * second)
            resp <- runExceptT $ nextBlocks nw block0
            resp `shouldSatisfy` isRight
            resp `shouldSatisfy` (not . null)

        it "no blocks after the tip" $ \(_, nw, _) -> do
            let try = do
                    tip <- unsafeRunExceptT $ networkTip nw
                    runExceptT $ nextBlocks nw tip
            -- NOTE Retrying twice since between the moment we fetch the
            -- tip and the moment we get the next blocks, one block may be
            -- inserted.
            -- Nevertheless, this can't happen twice within a slot time.
            resp <- retrying once
                (\_ x -> return $ fmap length x /= Right 0)
                (const try)
            resp `shouldBe` Right []

        it "returns an error when the block header is unknown" $ \(_, nw, _) -> do
            -- NOTE There's a very little chance of hash clash here. But,
            -- for what it's worth, I didn't bother retrying.
            bytes <- BS.pack <$> generate (vectorOf 32 arbitrary)
            let block = BlockHeader
                    { slotId = testSlotId 42 14 -- Anything
                    , blockHeight = Quantity 0 -- Anything
                    , prevBlockHash = Hash bytes
                    }
            resp <- runExceptT $ nextBlocks nw block
            resp `shouldBe` Left (ErrGetBlockNotFound (Hash bytes))

    describe "Error paths" $ do
        let makeUnreachableNetworkLayer = do
                port <- head <$> randomUnusedTCPPorts 1
                let dummyUrl = BaseUrl Http "localhost" port "/api"
                Jormungandr.newNetworkLayer dummyUrl block0H

        it "networkTip: ErrNetworkUnreachable" $ do
            nw <- makeUnreachableNetworkLayer
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ networkTip nw
                    res `shouldSatisfy` \case
                        Left (ErrNetworkTipNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

        it "nextBlocks: ErrNetworkUnreachable" $ do
            nw <- makeUnreachableNetworkLayer
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ nextBlocks nw block0
                    res `shouldSatisfy` \case
                        Left (ErrGetBlockNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

        it "networkTip: throws on invalid url" $ do
            let test (_, _nw, url) = do
                    let wrongUrl = url { baseUrlPath = "/not-valid-prefix" }
                    wrongNw <- Jormungandr.newNetworkLayer wrongUrl block0H
                    let io = void $ runExceptT $ networkTip wrongNw
                    shouldThrow io $ \(ErrUnexpectedNetworkFailure link _) ->
                        show link == show (safeLink api (Proxy @GetTipId))
            bracket startNode' killNode test

    describe "White-box error path tests" $
        beforeAll startNode' $ afterAll killNode $ do

        it "can't fetch a block that doesn't exist" $ \(_, _, url) -> do
            mgr <- newManager defaultManagerSettings
            let jml = Jormungandr.mkJormungandrLayer mgr url
            let nonexistent = Hash "kitten"
            res <- runExceptT (Jormungandr.getBlock jml nonexistent)
            res `shouldBe` Left (ErrGetBlockNotFound nonexistent)

        it "can't fetch a blocks from a parent that doesn't exist" $ \(_, _, url) -> do
            mgr <- newManager defaultManagerSettings
            let jml = Jormungandr.mkJormungandrLayer mgr url
            let nonexistent = Hash "cat"
            res <- runExceptT (Jormungandr.getDescendantIds jml nonexistent 42)
            res `shouldBe` Left (ErrGetDescendantsParentNotFound nonexistent)

        it "returns correct error when backend is not started" $ \(_, _, url) -> do
            mgr <- newManager defaultManagerSettings
            -- connect with a base URL for which the backend is not started on
            let url' = url { baseUrlPort = baseUrlPort url + 5 }
            let jml = Jormungandr.mkJormungandrLayer mgr url'
            res <- runExceptT (Jormungandr.getBlock jml (Hash "xyzzy"))
            res `shouldSatisfy` \case
                Left (ErrGetBlockNetworkUnreachable _) -> True
                _ -> False

    -- NOTE: 'Right ()' just means that the format wasn't obviously wrong.
    -- The tx may still be rejected.
    describe "Submitting signed transactions (that are not obviously wrong)"
        $ beforeAll startNode' $ afterAll killNode $ do

        it "empty tx succeeds" $ \(_, nw, _) -> do
            -- Would be rejected eventually.
            let signedEmpty = (Tx (fragmentId [] [] []) [] [], [])
            runExceptT (postTx nw signedEmpty) `shouldReturn` Right ()

        it "some tx succeeds" $ \(_, nw, _) -> do
            let signed = (txNonEmpty, [pkWitness])
            runExceptT (postTx nw signed) `shouldReturn` Right ()

        it "unbalanced tx (surplus) succeeds" $ \(_, nw, _) -> do
            -- Jormungandr will eventually reject txs that are not perfectly
            -- balanced though.
            let signed = (unbalancedTx, [pkWitness])
            runExceptT (postTx nw signed) `shouldReturn` Right ()

        it "more inputs than witnesses - encoder throws" $ \(_, nw, _) -> do
            let signed = (txNonEmpty, [])
            runExceptT (postTx nw signed) `shouldThrow` anyException

        it "more witnesses than inputs - fine apparently" $ \(_, nw, _) -> do
            -- Because of how signed txs are encoded:
            -- n                      :: Word8
            -- m                      :: Word8
            -- in_0 .. in_n           :: [TxIn]
            -- out_0 .. out_m         :: [TxOut]
            -- witness_0 .. witness_n :: [TxWitness]
            --
            -- this should in practice be like appending bytes to the end of
            -- the message.
            let signed = (txNonEmpty, [pkWitness, pkWitness, pkWitness])
            runExceptT (postTx nw signed) `shouldThrow` anyException

        it "no input, one output" $ \(_, nw, _) -> do
            -- Would be rejected eventually.
            let outs =
                    [ (TxOut $ unsafeDecodeAddress proxy
                        "ca1qwunuat6snw60g99ul6qvte98fja\
                        \le2k0uu5mrymylqz2ntgzs6vs386wxd")
                      (Coin 1227362560)
                    ]
            let tx = (Tx (fragmentId [] outs []) [] outs, [])
            runExceptT (postTx nw tx) `shouldReturn` Right ()

        it "throws when addresses and hashes have wrong length" $ \(_, nw, _) -> do
            let out = TxOut (Address "<not an address>") (Coin 1227362560)
            let tx = (Tx (fragmentId [] [out] []) [] [out], [])
            runExceptT (postTx nw tx) `shouldThrow` anyException

        it "encoder throws an exception if tx is invalid (eg too many inputs)" $
            \(_, nw, _) -> do
            let inps = replicate 300 (head $ inputs txNonEmpty)
            let outs = replicate 3 (head $ outputs txNonEmpty)
            let tx = (Tx (fragmentId inps outs []) inps outs, [])
            runExceptT (postTx nw tx) `shouldThrow` anyException

    describe "Decode External Tx" $ do
        let tl = newTransactionLayer @'Testnet (Hash "genesis")

        it "decodeExternalTx works ok with properly constructed binary blob" $ do
            property $ \(SignedTx signedTx) -> monadicIO $ liftIO $ do
                let encode ((Tx _ inps outs), wits) = runPut
                        $ withHeader MsgTypeTransaction
                        $ putSignedTx inps outs wits
                let encodedSignedTx = BL.toStrict $ encode signedTx
                decodeSignedTx tl encodedSignedTx `shouldBe` Right signedTx

        it "decodeExternalTx throws an exception when binary blob has non-\
            \transaction-type header or is wrongly constructed binary blob" $ do
            property $ \(SignedTx signedTx) -> monadicIO $ liftIO $ do
                let encodeWrongly ((Tx _ inps outs), wits) = runPut
                        $ withHeader MsgTypeInitial
                        $ putSignedTx inps outs wits
                let encodedSignedTx = BL.toStrict $ encodeWrongly signedTx
                decodeSignedTx tl encodedSignedTx `shouldBe`
                    (Left $ ErrDecodeSignedTxWrongPayload
                        "wrongly constructed binary blob")
  where
    second :: Int
    second = 1000000

    startNode
        :: (forall n. NetworkLayer n IO -> IO ())
        -> IO (Async (), NetworkLayer (Jormungandr 'Testnet) IO, BaseUrl)
    startNode wait = do
        (handle, baseUrl, _) <- launchJormungandr Inherit
        nw <- Jormungandr.newNetworkLayer baseUrl block0H
        wait nw $> (handle, nw, baseUrl)

    killNode :: (Async (), a, BaseUrl) -> IO ()
    killNode (h, _, _) = do
        cancel h
        threadDelay (1 * second)
    pkWitness :: TxWitness
    pkWitness = TxWitness $ BS.pack $ replicate 64 3

    proxy :: Proxy (Jormungandr 'Mainnet)
    proxy = Proxy

    txNonEmpty :: Tx
    txNonEmpty = Tx
        { txid = Hash "unused"
        , inputs =
            [ (TxIn
                { inputId = Hash $ unsafeFromHex
                    "666984dec4bc0ff1888be97bfe0694a9\
                    \6b35c58d025405ead51d5cc72a3019f4"
                , inputIx = 0
                }, Coin 934864225351)
            ]
        , outputs =
            [ TxOut
                { address = unsafeDecodeAddress proxy
                    "ca1q0u7k6ltp3e52pch47rhdkld2gdv\
                    \gu26rwyqh02csu3ah3384f2nvhlk7a6"
                , coin = Coin 933636862791
                }
            , TxOut
                { address = unsafeDecodeAddress proxy
                    "ca1qwunuat6snw60g99ul6qvte98fja\
                    \le2k0uu5mrymylqz2ntgzs6vs386wxd"
                , coin = Coin 1227362560
                }
            ]
        }

    unbalancedTx :: Tx
    unbalancedTx = Tx
        { txid = Hash "unused"
        , inputs =
            [ (TxIn
                { inputId = Hash $ unsafeFromHex
                    "666984dec4bc0ff1888be97bfe0694a9\
                    \6b35c58d025405ead51d5cc72a3019f4"
                , inputIx = 0
                }, Coin 100)
            ]
        , outputs =
            [ TxOut
                { address = unsafeDecodeAddress proxy
                    "ca1q0u7k6ltp3e52pch47rhdkld2gdv\
                    \gu26rwyqh02csu3ah3384f2nvhlk7a6"
                , coin = Coin 5
                }
            , TxOut
                { address = unsafeDecodeAddress proxy
                    "ca1qwunuat6snw60g99ul6qvte98fja\
                    \le2k0uu5mrymylqz2ntgzs6vs386wxd"
                , coin = Coin 5
                }
            ]
        }

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
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary
    shrink = genericShrink

-- Only generating single addresses!
instance Arbitrary Address where
    arbitrary = do
        let singleAddressHeader = 3
        let singleAddressLenWithoutHeader = 32
        Address . prependTag singleAddressHeader <$>
            genFixed singleAddressLenWithoutHeader
    shrink (Address addr) = Address . prependTag 3
        <$> shrinkFixedBS (BS.tail addr)

genFixed :: Int -> Gen ByteString
genFixed n = BS.pack <$> (vectorOf n arbitrary)

shrinkFixedBS :: ByteString -> [ByteString]
shrinkFixedBS bs = [zeros | bs /= zeros]
      where
        len = BS.length bs
        zeros = BS.pack (replicate len 0)

prependTag :: Int -> ByteString -> ByteString
prependTag tag bs = BS.pack [fromIntegral tag] <> bs

testEpochLength :: EpochLength
testEpochLength = EpochLength 21600

testSlotId :: Word64 -> Word16 -> SlotId
testSlotId en sn = epochSlotIdToSlotId testEpochLength $ EpochSlotId en sn
