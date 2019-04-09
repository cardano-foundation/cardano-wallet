{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network.HttpBridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Binary
    ( TxWitness (..), encodeSignedTx )
import Cardano.Wallet.Network
    ( ErrNetworkUnreachable (..), ErrPostTx (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , SignedTx (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Test.Hspec
    ( Spec, afterAll, beforeAll, describe, it, shouldReturn, shouldSatisfy )

import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as BL

port :: Int
port = 1337

spec :: Spec
spec = do
    describe "Happy paths" $ beforeAll startBridge $ afterAll closeBridge $ do
        it "get from packed epochs" $ \(_, network) -> do
            let blocks = runExceptT $ nextBlocks network (SlotId 14 0)
            (fmap length <$> blocks)
                `shouldReturn` pure 21600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "7Z\213\204\SUB\134l\149\&8\191ZO\\0q]\ESCB\CAN\254\f[\RS\
                    \\142\SOH\192K\250^\168\188m")

        it "get from packet epochs and filter by start slot"
                $ \(_, network) -> do
            let blocks = runExceptT $ nextBlocks network (SlotId 14 14000)
            (fmap length <$> blocks)
                `shouldReturn` pure 7600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "\186\173\135)\129\248 \214\222\159\161x\EM\214\187\&8\158\
                    \\220\237\245\bd\207\DC4\RS\168\212\143\240g\EOTQ")

        it "get unstable blocks for the unstable epoch" $ \(_, network) -> do
            let action = runExceptT $ do
                    (SlotId ep sl) <- (slotId . snd) <$> networkTip network
                    let sl' = if sl > 2 then sl - 2 else 0
                    blocks <- nextBlocks network (SlotId ep sl')
                    lift $ blocks `shouldSatisfy` (\bs
                        -> length bs >= fromIntegral (sl - sl')
                        && length bs <= fromIntegral (sl - sl' + 1)
                        )
            action `shouldReturn` pure ()

        it "produce no blocks if start is after tip" $ \(_, network) -> do
            let action = runExceptT $ do
                    SlotId ep sl <- (slotId . snd) <$> networkTip network
                    length <$> nextBlocks network (SlotId (ep + 1) sl)
            action `shouldReturn` pure 0

    describe "Error paths" $ beforeAll newNetworkLayer $ do
        it "gets a 'ErrNetworkUnreachable' if bridge isn't up (1)" $ \network -> do
            let msg x = "Expected a ErrNetworkUnreachable' failure but got " <> show x
            let action = do
                    res <- runExceptT $ networkTip network
                    res `shouldSatisfy` \case
                        Left (ErrNetworkUnreachable _) -> True
                        _ -> error (msg res)
            action `shouldReturn` ()

        it "gets a 'ErrNetworkUnreachable' if bridge isn't up (2)" $ \network -> do
            let msg x = "Expected a ErrNetworkUnreachable' failure but got " <> show x
            let tx = sign txEmpty []
            let action = do
                    res <- runExceptT $ postTx network tx
                    res `shouldSatisfy` \case
                        Left (ErrPostTxNetworkUnreachable (ErrNetworkUnreachable _)) ->
                            True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

    describe "Submitting signed transactions"
        $ beforeAll startBridge $ afterAll closeBridge $ do
        it "empty tx fails (1)" $ \(_, network) -> do
            let signed = sign txEmpty []
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has no inputs"
            runExceptT (postTx network signed) `shouldReturn` err

        it "empty tx fails (2)" $ \(_, network) -> do
            let signed = sign txEmpty [pkWitness]
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has no inputs"
            runExceptT (postTx network signed) `shouldReturn` err

        it "old tx fails" $ \(_, network) -> do
            let signed = sign txNonEmpty [pkWitness]
            let err = Left $ ErrPostTxProtocolFailure
                    "Failed to send to peers: Blockchain protocol error"
            runExceptT (postTx network signed) `shouldReturn` err

        it "tx fails - more inputs than witnesses" $ \(_, network) -> do
            let signed = sign txNonEmpty []
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has more \
                    \inputs than witnesses"
            runExceptT (postTx network signed) `shouldReturn` err

        it "tx fails - more witnesses than inputs" $ \(_, network) -> do
            let signed = sign txNonEmpty [pkWitness, pkWitness]
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has more \
                    \witnesses than inputs"
            runExceptT (postTx network signed) `shouldReturn` err
  where
    sign :: Tx -> [TxWitness] -> SignedTx
    sign tx witnesses = SignedTx . toBS $ encodeSignedTx (tx, witnesses)

    toBS = BL.toStrict . CBOR.toLazyByteString

    pkWitness :: TxWitness
    pkWitness = PublicKeyWitness "\130X@O\a\142a\166\180\SO\205\&3I8\160)\224F?\157\252\ACK\DC2\EOT\ESC\184\201\170\218\217\ETX\201\ESCn\SYN\206\179O\n\236\185\235T\163\190o\SI'r\228\241\150yL\218\NAK R2\162\211\144\209\129lr\225X@Go%&7\248\149\194\202\231\210\143-\212f.\135\174\254\186\193^\212?\136\SO;\ACK\a\211\DC1\b\223\159\161\179&\189[\231\217\179\143JOW\194iv5\EMr\197\ETX\158p\DC4=\145\128\n/\255\NUL"

    txNonEmpty :: Tx
    txNonEmpty = Tx { inputs = [ TxIn {inputId = Hash {getHash = "!s\CAN\255]e\252\184F\160\178\250:\190\DC2\250u\SOf\203\179\225\238+\244\162WvR\159\183\b"}, inputIx = 0}]
                    , outputs = [TxOut {address = Address {getAddress = "\130\216\CANXI\131X\FS`\218\181D\EOT{p\196\242\251\253\DC2\221\225\DC1\171\172\161;\172A\\\232\166\ETB\RSp\208\162\SOHX\RSX\FS%\245\158\195@\ENQ\225\212*\215\RSA\241;\245/\243\174s\143O\183\229V\175!\131\226\STXE\SUBAp\203\ETB\NUL\SUBm\US\131\DC4"}, coin = Coin {getCoin = 933636862791}},TxOut {address = Address {getAddress = "\130\216\CANXI\131X\FS\227\213\&146.\160)\226\207\218\134@\\\149\166oq\CAN\DC1c\245\237\153]\235yO\162\SOHX\RSX\FS%\245\158\195@\ENQ\225\140\133\182\189A\194\206\228UZ\ENQ\168,\134\207\183\253\235\236t\171\STXE\SUBAp\203\ETB\NUL\SUB8\203F)"}, coin = Coin {getCoin = 1227362560}}]
                    }
    txEmpty :: Tx
    txEmpty = Tx [] []

    newNetworkLayer =
        HttpBridge.newNetworkLayer "testnet" port
    closeBridge (handle, _) = do
        cancel handle
        threadDelay 500000
    startBridge = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", "testnet"
                ]
                (return ())
                Inherit
            ]
        network <- newNetworkLayer
        threadDelay 1000000
        return (handle, network)
