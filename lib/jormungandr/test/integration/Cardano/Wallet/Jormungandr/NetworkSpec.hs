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

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet.Jormungandr.Api
    ( GetTipId, api )
import Cardano.Wallet.Jormungandr.Api.Client
    ( ErrGetAccountState (..), JormungandrClient )
import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountState (..) )
import Cardano.Wallet.Jormungandr.Binary
    ( TxWitnessTag (..)
    , constructTransactionFragment
    , putTxWitnessTag
    , runPut
    , txWitnessSize
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetDescendants (..)
    , ErrUnexpectedNetworkFailure (..)
    , JormungandrBackend (..)
    , JormungandrConnParams (..)
    , Scheme (..)
    , mkRawNetworkLayer
    , withJormungandr
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    )
import Cardano.Wallet.Network.BlockHeaders
    ( emptyBlockHeaders )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SealedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    , slotMinBound
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress, unsafeFromHex, unsafeFromText, unsafeRunExceptT )
import Control.Arrow
    ( second )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( limitRetries, retrying )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word8 )
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
    , runIO
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
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Utils.Ports
    ( randomUnusedTCPPorts )

import qualified Cardano.Wallet.Jormungandr.Api.Client as Jormungandr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    let once = limitRetries 1
    describe "Happy Paths" $ around startNode $ do

        it "get account state" $
            \(_, url) -> do
                manager <- newManager defaultManagerSettings
                let client = Jormungandr.mkJormungandrClient manager url
                res <- runExceptT $
                    Jormungandr.getAccountState client testAccountId
                res `shouldBe` Right AccountState
                    { currentBalance = Quantity 1
                    , totalTransactionCount = Quantity 0
                    , stakePools = [(testPoolId, Quantity 1)]
                    }

        it "get network tip" $ \(nw, _) -> do
            resp <- runExceptT $ networkTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId <$> resp
            let (Right height) = blockHeight <$> resp
            slot `shouldSatisfy` (>= slotMinBound)
            height `shouldSatisfy` (>= Quantity 0)

        it "get some blocks from the genesis" $ \(nw, _) -> do
            threadDelay (10 * sec)
            resp <- runExceptT $ nextBlocks nw (initCursor nw [])
            resp `shouldSatisfy` isRight
            resp `shouldSatisfy` (not . null)

        it "no blocks after the tip" $ \(nw, _) -> do
            let try = do
                    tip <- unsafeRunExceptT $ networkTip nw
                    runExceptT $ nextBlocks nw (initCursor nw [tip])
            -- NOTE Retrying twice since between the moment we fetch the
            -- tip and the moment we get the next blocks, one block may be
            -- inserted.
            -- Nevertheless, this can't happen twice within a slot time.
            resp <- retrying once
                (\_ x -> return $ fmap isRollForward x == Right True)
                (const try)
            -- fmap getRollForward resp `shouldBe` Right Nothing
            resp `shouldBe` Right AwaitReply

        it "initiates recovery when the intersection is unknown" $
            \(nw, _) -> do
                -- NOTE There's a very little chance of hash clash here. But,
                -- for what it's worth, I didn't bother retrying.
                bytes <- BS.pack <$> generate (vectorOf 32 arbitrary)
                let block = BlockHeader
                        { slotId = SlotId 42 14 -- Anything
                        , blockHeight = Quantity 0 -- Anything
                        , headerHash = Hash bytes
                        , parentHeaderHash = Hash bytes
                        }
                resp <- runExceptT $ nextBlocks nw (initCursor nw [block])
                fmap (isRollBackwardTo nw (SlotId 0 0)) resp
                    `shouldBe` Right True

    describe "Error paths" $ do

        let newBrokenNetworkLayer
                :: BaseUrl
                -> IO (NetworkLayer IO Jormungandr ())
            newBrokenNetworkLayer baseUrl = do
                mgr <- newManager defaultManagerSettings
                st <- newMVar emptyBlockHeaders
                let jor = Jormungandr.mkJormungandrClient mgr baseUrl
                let g0 = (error "block0", error "BlockchainParameters")
                return (void $ mkRawNetworkLayer g0 1000 st jor)

        let makeUnreachableNetworkLayer = do
                port <- head <$> randomUnusedTCPPorts 1
                let dummyUrl = BaseUrl Http "localhost" port "/api"
                newBrokenNetworkLayer dummyUrl

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
                    res <- runExceptT $ nextBlocks nw (initCursor nw [])
                    res `shouldSatisfy` \case
                        Left (ErrGetBlockNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

        it "networkTip: throws on invalid url" $
            startNode $ \(_nw, url) -> do
                let wrongUrl = url { baseUrlPath = "/not-valid-prefix" }
                wrongNw <- newBrokenNetworkLayer wrongUrl
                let io = void $ runExceptT $ networkTip wrongNw
                shouldThrow io $ \(ErrUnexpectedNetworkFailure link _) ->
                    show link == show (safeLink api (Proxy @GetTipId))

    describe "White-box error path tests" $
        around startNode $ do

        testNotFound
            "getAccountState"
            Jormungandr.getAccountState
            (Hash $ B8.replicate 32 '0')
            ErrGetAccountStateAccountNotFound

        testGetInvalid
            "getAccountState"
            Jormungandr.getAccountState
            (Hash "patate")

        testNotFound
            "getBlock"
            Jormungandr.getBlock
            (Hash $ B8.replicate 32 '0')
            ErrGetBlockNotFound

        testGetInvalid
            "getBlock"
            Jormungandr.getBlock
            (Hash "patate")

        testNotFound
            "getDescendantIds"
            (\client rid -> Jormungandr.getDescendantIds client rid 10)
            (Hash $ B8.replicate 32 '0')
            ErrGetDescendantsParentNotFound

        testGetInvalid
            "getBlockDescendantIds"
            (\client rid -> Jormungandr.getDescendantIds client rid 10)
            (Hash "patate")

        it "returns correct error when backend is not started" $
            \(_, url) -> do
                mgr <- newManager defaultManagerSettings
                -- connect with a base URL on which the backend is not started:
                let url' = url { baseUrlPort = baseUrlPort url + 5 }
                let jml = Jormungandr.mkJormungandrClient mgr url'
                res <- runExceptT (Jormungandr.getBlock jml (Hash "xyzzy"))
                res `shouldSatisfy` \case
                    Left (ErrGetBlockNetworkUnreachable _) -> True
                    _ -> False


    utxoWitness <- runIO $ generate arbitrary
    let mkDummyWit _addr = Identity $ const utxoWitness
    let mkTx' ins outs = runIdentity $
            constructTransactionFragment ins outs mkDummyWit
    let mkTx ins outs = snd $ mkTx' ins outs

    -- NOTE: 'Right ()' just means that the format wasn't obviously wrong.
    -- The tx may still be rejected.
    describe "Submitting signed transactions"
        $ around startNode $ do

        it "empty tx succeeds" $ \(nw, _) -> do
            -- Would be rejected eventually.
            let signedEmpty = mkTx [] []
            runExceptT (postTx nw signedEmpty) `shouldReturn` Right ()

        it "some tx succeeds" $ \(nw, _) -> do
            let signed = uncurry mkTx txNonEmpty
            runExceptT (postTx nw signed) `shouldReturn` Right ()

        it "unbalanced tx (surplus) succeeds" $ \(nw, _) -> do
            -- Jormungandr will eventually reject txs that are not perfectly
            -- balanced though.
            let signed = uncurry mkTx unbalancedTx
            runExceptT (postTx nw signed) `shouldReturn` Right ()

        it "no input, one output succeeds (for now)" $ \(nw, _) -> do
            -- Would be rejected eventually.
            let outs =
                    [ (TxOut $ unsafeDecodeAddress @'Mainnet
                        "ca1qwunuat6snw60g99ul6qvte98fja\
                        \le2k0uu5mrymylqz2ntgzs6vs386wxd")
                      (Coin 1227362560)
                    ]
            let signed = mkTx [] outs
            runExceptT (postTx nw signed) `shouldReturn` Right ()

        it "invalid binary should fail early" $ \(nw, _) -> do
            let signed = SealedTx $ unsafeFromHex "0000"
            r <- runExceptT (postTx nw signed)
            case r of
                Right _ -> expectationFailure "invalid tx should fail"
                Left _ -> return ()

        it "encoder throws an exception if tx is invalid (eg too many inputs)" $
            \(nw, _) -> do
                let inps = replicate 300 (head $ fst txNonEmpty)
                let outs = replicate 3 (head $ snd txNonEmpty)
                -- Previously it was postTx that could throw. Now it is mkTx.
                -- But because of laziness this test still works!
                let signed = mkTx inps outs
                runExceptT (postTx nw signed) `shouldThrow` anyException

    describe "Decode External Tx" $ do
        let tl = newTransactionLayer @'Testnet @ShelleyKey (Hash "genesis")

        it "decodeExternalTx works ok with properly constructed binary blob" $ do
            property $ \(InputsOutputs (ins, outs)) -> monadicIO $ liftIO $ do
                let (hash, sealed) = mkTx' ins outs
                let tx = Tx hash (map (second coin) ins) outs
                decodeSignedTx tl (getSealedTx sealed)
                    `shouldBe` Right (tx, sealed)

        it "decodeExternalTx throws an exception when binary blob has non-\
            \transaction-type header or is wrongly constructed binary blob" $
            monadicIO $ liftIO $ do
                let malformedTx = "\NUL\NUL\NUL\NUL"
                decodeSignedTx tl malformedTx `shouldBe` Left
                    (ErrDecodeSignedTxWrongPayload
                        "wrongly constructed binary blob")
  where
    sec :: Int
    sec = 1000000

    startNode cb = withConfig $ \cfg -> do
        let tr = nullTracer
        e <- withJormungandr tr cfg $
            \cp -> withNetworkLayer tr (UseRunning cp) $ \case
                Right (_, nw) -> cb (nw, _restApi cp)
                Left e -> throwIO e
        either throwIO (\_ -> return ()) e

    txNonEmpty =
        (
            [ (TxIn
                { inputId = Hash $ unsafeFromHex
                    "666984dec4bc0ff1888be97bfe0694a9\
                    \6b35c58d025405ead51d5cc72a3019f4"
                , inputIx = 0
                }, TxOut (Address "") (Coin 934864225351))
            ]
        ,
            [ TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1q0u7k6ltp3e52pch47rhdkld2gdv\
                    \gu26rwyqh02csu3ah3384f2nvhlk7a6"
                , coin = Coin 933636862791
                }
            , TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1qwunuat6snw60g99ul6qvte98fja\
                    \le2k0uu5mrymylqz2ntgzs6vs386wxd"
                , coin = Coin 1227362560
                }
            ]
        )

    unbalancedTx =
        (
            [ (TxIn
                { inputId = Hash $ unsafeFromHex
                    "666984dec4bc0ff1888be97bfe0694a9\
                    \6b35c58d025405ead51d5cc72a3019f4"
                , inputIx = 0
                }, TxOut (Address "owner") (Coin 100))
            ]
        ,
            [ TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1q0u7k6ltp3e52pch47rhdkld2gdv\
                    \gu26rwyqh02csu3ah3384f2nvhlk7a6"
                , coin = Coin 5
                }
            , TxOut
                { address = unsafeDecodeAddress @'Mainnet
                    "ca1qwunuat6snw60g99ul6qvte98fja\
                    \le2k0uu5mrymylqz2ntgzs6vs386wxd"
                , coin = Coin 5
                }
            ]
        )

-- | Exercise a particular Jörmungandr API getter and expect a 404 Not Found.
testNotFound
    :: (Show err, Eq err, Show result, Eq result)
    => String
    -> (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> resourceId
    -> (resourceId -> err)
    -> SpecWith (whatever, BaseUrl)
testNotFound fn endpoint resourceId err =
    it (fn <> ": not found") $ \(_, url) -> do
        manager <- newManager defaultManagerSettings
        let client = Jormungandr.mkJormungandrClient manager url
        res <- runExceptT $ endpoint client resourceId
        res `shouldBe` Left (err resourceId)

testGetInvalid
    :: String
    -> (forall m. JormungandrClient m -> resourceId -> ExceptT err m result)
    -> resourceId
    -> SpecWith (whatever, BaseUrl)
testGetInvalid fn endpoint resourceId =
    it (fn <> ": invalid") $ \(_, url) -> do
        manager <- newManager defaultManagerSettings
        let client = Jormungandr.mkJormungandrClient manager url
        let io = runExceptT $ endpoint client resourceId
        io `shouldThrow` isUnexpectedNetworkFailure
  where
    isUnexpectedNetworkFailure :: ErrUnexpectedNetworkFailure -> Bool
    isUnexpectedNetworkFailure ErrUnexpectedNetworkFailure{} = True

instance Show (NextBlocksResult t b) where
    show AwaitReply = "AwaitReply"
    show (RollForward _ _ bs) = "RollForward " ++ show (length bs) ++ " blocks"
    show (RollBackward _) = "RollBackward"

instance Eq (NextBlocksResult t b) where
    a == b = show a == show b

newtype InputsOutputs = InputsOutputs ([(TxIn, TxOut)], [TxOut])
    deriving (Eq, Show)

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

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash <$> genFixed 32
    shrink (Hash bytes) = Hash <$> shrinkFixedBS bytes

-- | Only generates single address witnesses
instance Arbitrary TxWitness where
    arbitrary = taggedWitness TxWitnessUTxO . TxWitness
        <$> genFixed (txWitnessSize TxWitnessUTxO)

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
    shrink (TxIn h ix) = TxIn h <$> shrink ix

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary
    shrink (TxOut a c) = [TxOut a' c' | (a',c') <- shrink (a,c)]

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

taggedWitness :: TxWitnessTag -> TxWitness -> TxWitness
taggedWitness tag (TxWitness bytes) = TxWitness (prefix <> bytes)
  where
    prefix = BL.toStrict $ runPut $ putTxWitnessTag tag

getRollForward :: NextBlocksResult target block -> Maybe [block]
getRollForward AwaitReply = Nothing
getRollForward (RollForward _ _ bs) = Just bs
getRollForward (RollBackward _) = Nothing

isRollForward :: NextBlocksResult target block -> Bool
isRollForward = maybe False (not . null) . getRollForward

isRollBackwardTo
    :: NetworkLayer m target block
    -> SlotId
    -> NextBlocksResult target block
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
    "4f8d686a02c6e625b5a59cc9e234f32e5d72987012f9c25c9a6b60ddade197d1"
