{-# LANGUAGE DataKinds #-}
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

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet.Jormungandr.Api
    ( GetTipId, api )
import Cardano.Wallet.Jormungandr.Api.Client
    ( ErrGetAccountState (..), JormungandrClient )
import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountState (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( MkFragment (..)
    , TxWitnessTag (..)
    , finalizeFragment
    , maxNumberOfInputs
    , putFragment
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , ErrUnexpectedNetworkFailure (..)
    , JormungandrBackend (..)
    , JormungandrConnParams (..)
    , Scheme (..)
    , mkRawNetworkLayer
    , withJormungandr
    , withNetworkLayer
    )
import Cardano.Wallet.Network
    ( Cursor
    , ErrCurrentNodeTip (..)
    , ErrGetBlock (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    )
import Cardano.Wallet.Network.BlockHeaders
    ( emptyBlockHeaders )
import Cardano.Wallet.Network.Ports
    ( randomUnusedTCPPorts )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), Passphrase (..) )
import Cardano.Wallet.Primitive.Slotting
    ( slotMinBound )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SealedTx (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress
    , unsafeFromHex
    , unsafeFromText
    , unsafeRunExceptT
    , unsafeXPrv
    )
import Control.Arrow
    ( left )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar )
import Control.Concurrent.STM.TChan
    ( newBroadcastTChanIO )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( throwIO, try )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( limitRetries, retrying )
import Data.Either
    ( isRight )
import Data.Generics.Internal.VL.Prism
    ( (^?) )
import Data.Generics.Sum.Constructors
    ( _Ctor )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Servant.Links
    ( safeLink )
import Test.Hspec
    ( Spec
    , SpecWith
    , anyException
    , around
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import Test.QuickCheck
    ( Arbitrary (..), generate, vector )

import qualified Cardano.Wallet.Jormungandr.Api.Client as Jormungandr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    let once = limitRetries 1
    describe "Happy Paths" $ around startNode $ do

        it "getAccountState: successfully gets account state" $
            \(_, url) -> do
                manager <- newManager defaultManagerSettings
                let client = Jormungandr.mkJormungandrClient manager url
                res <- runExceptT $
                    Jormungandr.getAccountState client testAccountId
                res `shouldBe` Right AccountState
                    { currentBalance = Quantity 1000
                    , totalTransactionCount = Quantity 0
                    , stakePools = [(testPoolId, Quantity 1)]
                    }

        it "get network tip" $ \(nw, _) -> do
            resp <- runExceptT $ currentNodeTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId <$> resp
            let (Right height) = blockHeight <$> resp
            slot `shouldSatisfy` (>= slotMinBound)
            height `shouldSatisfy` (>= Quantity 0)

        it "get some blocks from the genesis" $ \(nw, _) -> do
            threadDelay (10 * second)
            resp <- (runExceptT . nextBlocks nw) =<< initCursor nw []
            resp `shouldSatisfy` isRight
            resp `shouldSatisfy` (not . null)

        it "no blocks after the tip" $ \(nw, _) -> do
            let attempt = do
                    tip <- unsafeRunExceptT $ currentNodeTip nw
                    (runExceptT . nextBlocks nw) =<< initCursor nw [tip]
            -- NOTE Retrying twice since between the moment we fetch the
            -- tip and the moment we get the next blocks, one block may be
            -- inserted.
            -- Nevertheless, this can't happen twice within a slot time.
            resp <- retrying once
                (\_ x -> return $ fmap isRollForward x == Right True)
                (const attempt)
            -- fmap getRollForward resp `shouldBe` Right Nothing
            resp `shouldBe` Right AwaitReply

        it "initiates recovery when the intersection is unknown" $
            \(nw, _) -> do
                -- NOTE There's a very little chance of hash clash here. But,
                -- for what it's worth, I didn't bother retrying.
                bytes <- BS.pack <$> generate (vector 32)
                let block = BlockHeader
                        { slotId = SlotId 42 14 -- Anything
                        , blockHeight = Quantity 0 -- Anything
                        , headerHash = Hash bytes
                        , parentHeaderHash = Hash bytes
                        }
                resp <- (runExceptT . nextBlocks nw) =<< initCursor nw [block]
                fmap (isRollBackwardTo nw (SlotId 0 0)) resp
                    `shouldBe` Right True

    describe "Error paths" $ do

        let newBrokenNetworkLayer
                :: BaseUrl
                -> IO (NetworkLayer IO Jormungandr ())
            newBrokenNetworkLayer baseUrl = do
                mgr <- newManager defaultManagerSettings
                st <- newMVar emptyBlockHeaders
                chan <- newBroadcastTChanIO
                let jor = Jormungandr.mkJormungandrClient mgr baseUrl
                let g0 = error "GenesisParameters"
                return (void $ mkRawNetworkLayer g0 1000 st chan jor)

        let makeUnreachableNetworkLayer = do
                port <- head <$> randomUnusedTCPPorts 1
                let dummyUrl = BaseUrl Http "localhost" port "/api"
                newBrokenNetworkLayer dummyUrl

        it "currentNodeTip: ErrNetworkUnreachable" $ do
            nw <- makeUnreachableNetworkLayer
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ currentNodeTip nw
                    res `shouldSatisfy` \case
                        Left (ErrCurrentNodeTipNetworkUnreachable e) ->
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
                    res <- (runExceptT . nextBlocks nw) =<< initCursor nw []
                    res `shouldSatisfy` \case
                        Left (ErrGetBlockNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

        it "currentNodeTip: throws on invalid url" $
            startNode $ \(_nw, url) -> do
                let wrongUrl = url { baseUrlPath = "/not-valid-prefix" }
                wrongNw <- newBrokenNetworkLayer wrongUrl
                let io = void $ runExceptT $ currentNodeTip wrongNw
                shouldThrow io $ \(ErrUnexpectedNetworkFailure link _) ->
                    show link == show (safeLink api (Proxy @GetTipId))

    describe "White-box error path tests" $
        around startNode $ do

        describe "getAccountState" $ do
            let ep = Jormungandr.getAccountState
            testNotFound ep ErrGetAccountStateAccountNotFound
            testInvalidUrlPath ep ErrGetAccountStateAccountNotFound
            testGetInvalidResourceId ep (Hash "patate")
            testGetNetworkUnreachable ep
                (^? (_Ctor @"ErrGetAccountStateNetworkUnreachable"))

        describe "getBlock" $ do
            let ep = Jormungandr.getBlock
            testNotFound ep ErrGetBlockNotFound
            testInvalidUrlPath ep ErrGetBlockNotFound
            testGetInvalidResourceId ep (Hash "patate")
            testGetNetworkUnreachable ep
                (^? (_Ctor @"ErrGetBlockNetworkUnreachable"))

        describe "getDescendantIds" $ do
            let ep client rid = Jormungandr.getDescendantIds client rid 10
            testNotFound ep ErrGetDescendantsParentNotFound
            testInvalidUrlPath ep ErrGetDescendantsParentNotFound
            testGetInvalidResourceId ep (Hash "patate")
            testGetNetworkUnreachable ep
                (^? (_Ctor @"ErrGetDescendantsNetworkUnreachable"))

        describe "getStakeDistribution" $ do
            let ep client () = Jormungandr.getStakeDistribution client 0
            testInvalidUrlPath ep (error "should throw")
            testGetNetworkUnreachable ep pure

        describe "getTipId" $ do
            let ep client () = Jormungandr.getTipId client
            testInvalidUrlPath ep (error "should throw")
            testGetNetworkUnreachable ep pure

        describe "getInitialBlockchainParameters" $ do
            let ep = Jormungandr.getInitialBlockchainParameters
            testInvalidUrlPath ep ErrGetBlockchainParamsGenesisNotFound
            testGetNetworkUnreachable ep
                (^? (_Ctor @"ErrGetBlockchainParamsNetworkUnreachable"))

        describe "postMessage" $ do
            it "network unreachable" $ \(_, url) -> do
                let url' = url { baseUrlPort = baseUrlPort url + 5 }
                res <- requestEndpoint
                        url'
                        Jormungandr.postMessage
                        (SealedTx mempty)
                res `shouldSatisfy` \case
                    Left (ErrPostTxNetworkUnreachable _) -> True
                    _ -> False

            it "invalid url path" $ \(_, url) -> do
                let url' = url { baseUrlPath = "/not-valid-prefix" }
                let io = requestEndpoint
                        url'
                        Jormungandr.postMessage
                        (SealedTx mempty)
                io `shouldThrow` isUnexpectedNetworkFailure

    -- NOTE: 'Right ()' just means that the format wasn't obviously wrong.
    -- The tx may still be rejected.
    describe "Submitting signed transactions (that are not obviously wrong)"
        $ around startNode $ do

        let block0H :: Hash "Genesis"
            block0H = Hash (B8.replicate 32 '0')

        let input0 :: TxIn
            input0 = TxIn
                { inputId = Hash $ unsafeFromHex
                    "666984dec4bc0ff1888be97bfe0694a9\
                    \6b35c58d025405ead51d5cc72a3019f4"
                , inputIx = 0
                }

        let inputValue0 :: Coin
            inputValue0 = Coin 934864225351

        let credentials0 :: (XPrv, Passphrase "encryption")
            credentials0 = (unsafeXPrv (B8.replicate 128 '0'), mempty)

        let output0 :: TxOut
            output0 = TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1q0u7k6ltp3e52pch47rhdkld2gdv\
                    \gu26rwyqh02csu3ah3384f2nvhlk7a6"
                , coin = Coin 933636862791
                }

        let output1 :: TxOut
            output1 = TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1qwunuat6snw60g99ul6qvte98fja\
                    \le2k0uu5mrymylqz2ntgzs6vs386wxd"
                , coin = Coin 1227362560
                }

        it "empty tx succeeds" $ \(nw, _) -> do
            -- Would be rejected eventually.
            let sealed = finalizeFragment $ putFragment
                    block0H
                    mempty
                    mempty
                    (MkFragmentSimpleTransaction TxWitnessUTxO)
            runExceptT (postTx nw sealed) `shouldReturn` Right ()

        it "some tx succeeds" $ \(nw, _) -> do
            let sealed = finalizeFragment $ putFragment
                    block0H
                    [((input0, inputValue0), credentials0)]
                    [output0, output1]
                    (MkFragmentSimpleTransaction TxWitnessUTxO)
            runExceptT (postTx nw sealed) `shouldReturn` Right ()

        it "unbalanced tx (surplus) succeeds" $ \(nw, _) -> do
            -- Jormungandr will eventually reject txs that are not perfectly
            -- balanced though.
            let Coin c = inputValue0
            let sealed = finalizeFragment $ putFragment
                    block0H
                    [((input0, Coin (c `div` 2)), credentials0)]
                    [output0, output1]
                    (MkFragmentSimpleTransaction TxWitnessUTxO)
            runExceptT (postTx nw sealed) `shouldReturn` Right ()

        it "no input, one output" $ \(nw, _) -> do
            -- Would be rejected eventually.
            let sealed = finalizeFragment $ putFragment
                    block0H
                    []
                    [output0]
                    (MkFragmentSimpleTransaction TxWitnessUTxO)
            runExceptT (postTx nw sealed) `shouldReturn` Right ()

        it "throws on too many inputs" $ \(nw, _) -> do
            let input = ((input0, inputValue0), credentials0)
            let sealed = finalizeFragment $ putFragment
                    block0H
                    (replicate (maxNumberOfInputs + 1) input)
                    [output0, output1]
                    (MkFragmentSimpleTransaction TxWitnessUTxO)
            runExceptT (postTx nw sealed) `shouldThrow` anyException
  where
    second :: Int
    second = 1000000

    startNode cb = withConfig $ \cfg -> do
        let tr = nullTracer
        e <- withJormungandr tr cfg $
            \cp -> withNetworkLayer tr (UseRunning cp) $ \case
                Right (_, _, nw) -> cb (nw, _restApi cp)
                Left e -> throwIO e
        either throwIO (\_ -> return ()) e

-- | Exercise a particular Jörmungandr API getter and expect a 404 Not Found.
testNotFound
    :: (Arbitrary resourceId, Show err, Eq err, Show result, Eq result)
    => (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> (resourceId -> err)
    -> SpecWith (whatever, BaseUrl)
testNotFound endpoint err =
    it "resource not found" $ \(_, url) -> do
        resourceId <- generate arbitrary
        res <- requestEndpoint url endpoint resourceId
        res `shouldBe` Left (err resourceId)

testInvalidUrlPath
    :: (Arbitrary resourceId, Eq err, Show err, Eq result, Show result)
    => (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> (resourceId -> err)
    -> SpecWith (whatever, BaseUrl)
testInvalidUrlPath endpoint err = do
    it "invalid url path" $ \(_, url) -> do
        let url' = url { baseUrlPath = "/not-valid-prefix" }
        resourceId <- generate arbitrary
        try (requestEndpoint url' endpoint resourceId) >>= \case
            Left (_ :: ErrUnexpectedNetworkFailure) ->
                pure ()
            Right res ->
                res `shouldBe` Left (err resourceId)

testGetInvalidResourceId
    :: (Show err, Eq err, Show result, Eq result)
    => (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> resourceId
    -> SpecWith (whatever, BaseUrl)
testGetInvalidResourceId endpoint resourceId =
    it "invalid resource id" $ \(_, url) -> do
        let io = requestEndpoint url endpoint resourceId
        io `shouldThrow` isUnexpectedNetworkFailure

testGetNetworkUnreachable
    :: (Arbitrary resourceId, Eq err, Show err, Eq result, Show result)
    => (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> (err -> Maybe ErrNetworkUnavailable)
    -> SpecWith (whatever, BaseUrl)
testGetNetworkUnreachable endpoint getError = do
    it "network unreachable" $ \(_, url) -> do
        let url' = url { baseUrlPort = baseUrlPort url + 5 }
        resourceId <- generate arbitrary
        res <- requestEndpoint url' endpoint resourceId
        case left getError res of
            Right _ ->
                expectationFailure "expected an error!"
            Left Nothing ->
                expectationFailure $ unwords
                    [ "got something else than a ErrNetworkUnavailable:"
                    , show res
                    ]
            Left Just{} ->
                return ()

isUnexpectedNetworkFailure :: ErrUnexpectedNetworkFailure -> Bool
isUnexpectedNetworkFailure ErrUnexpectedNetworkFailure{} = True

-- | Generic method to request endpoint against url and its resource
requestEndpoint
    :: (Show err, Eq err, Show result, Eq result)
    => BaseUrl
    -> (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> resourceId
    -> IO (Either err result)
requestEndpoint url endpoint resourceId = do
    manager <- newManager defaultManagerSettings
    let client = Jormungandr.mkJormungandrClient manager url
    runExceptT $ endpoint client resourceId

instance Show (NextBlocksResult (Cursor t) b) where
    show AwaitReply = "AwaitReply"
    show (RollForward _ _ bs) = "RollForward " ++ show (length bs) ++ " blocks"
    show (RollBackward _) = "RollBackward"

instance Eq (NextBlocksResult (Cursor t) b) where
    a == b = show a == show b

instance Arbitrary (Hash any) where
    arbitrary = Hash . BS.pack <$> vector 32

getRollForward :: NextBlocksResult (Cursor t) block -> Maybe [block]
getRollForward AwaitReply = Nothing
getRollForward (RollForward _ _ bs) = Just bs
getRollForward (RollBackward _) = Nothing

isRollForward :: NextBlocksResult (Cursor t) block -> Bool
isRollForward = maybe False (not . null) . getRollForward

isRollBackwardTo
    :: NetworkLayer m t block
    -> SlotId
    -> NextBlocksResult (Cursor t) block
    -> Bool
isRollBackwardTo nl sl = \case
    RollBackward cursor -> cursorSlotId nl cursor == sl
    _ -> False

{-------------------------------------------------------------------------------
                                   Test Data
-------------------------------------------------------------------------------}

testAccountId :: Hash "Account"
testAccountId = Hash $ unsafeFromHex
    "addf8bd48558b72b257408a0164c8722058b4d5337134ab9a02bc4e64194933a"

testPoolId :: PoolId
testPoolId = unsafeFromText
    "712dd028687560506e3b0b3874adbd929ab892591bfdee1221b5ee3796b79b70"
