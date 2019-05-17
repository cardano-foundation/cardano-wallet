{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network.HttpBridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment.HttpBridge
    ( Network (..), network )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Network
    ( ErrNetworkTip (..)
    , ErrNetworkUnreachable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT, withExceptT )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( Spec, afterAll, beforeAll, describe, it, shouldReturn, shouldSatisfy )

import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge
import qualified Data.Text as T

port :: Int
port = 1337

spec :: Spec
spec = do
    describe "Happy paths"
        $ requireTestnet
        $ beforeAll startBridge $ afterAll closeBridge $ do
        it "get from packed epochs" $ \(_, bridge) -> do
            let blocks = runExceptT $ nextBlocks bridge (SlotId 13 21599)
            (fmap length <$> blocks)
                `shouldReturn` pure 21600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "7Z\213\204\SUB\134l\149\&8\191ZO\\0q]\ESCB\CAN\254\f[\RS\
                    \\142\SOH\192K\250^\168\188m")

        it "get from packet epochs and filter by start slot"
                $ \(_, bridge) -> do
            let blocks = runExceptT $ nextBlocks bridge (SlotId 14 13999)
            (fmap length <$> blocks)
                `shouldReturn` pure 7600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "\186\173\135)\129\248 \214\222\159\161x\EM\214\187\&8\158\
                    \\220\237\245\bd\207\DC4\RS\168\212\143\240g\EOTQ")

        it "get unstable blocks for the unstable epoch" $ \(_, bridge) -> do
            let action = runExceptT $ do
                    (SlotId ep sl) <- (slotId . snd) <$> networkTip' bridge
                    let sl' = if sl > 2 then sl - 2 else 0
                    blocks <- nextBlocks bridge (SlotId ep sl')
                    lift $ blocks `shouldSatisfy` (\bs
                        -> length bs >= fromIntegral (sl - sl')
                        && length bs <= fromIntegral (sl - sl' + 1)
                        )
            action `shouldReturn` pure ()

        it "produce no blocks if start is after tip" $ \(_, bridge) -> do
            let action = runExceptT $ do
                    SlotId ep sl <- (slotId . snd) <$> networkTip' bridge
                    length <$> nextBlocks bridge (SlotId (ep + 1) sl)
            action `shouldReturn` pure 0

    describe "Error paths" $ beforeAll newNetworkLayer $ do
        it "gets a 'ErrNetworkUnreachable' if bridge isn't up (1)" $ \bridge -> do
            let msg x = "Expected a ErrNetworkUnreachable' failure but got " <> show x
            let action = do
                    res <- runExceptT $ networkTip bridge
                    res `shouldSatisfy` \case
                        Left (ErrNetworkTipNetworkUnreachable _) -> True
                        _ -> error (msg res)
            action `shouldReturn` ()

        it "gets a 'ErrNetworkUnreachable' if bridge isn't up (2)" $ \bridge -> do
            let msg x = "Expected a ErrNetworkUnreachable' failure but got " <> show x
            let action = do
                    res <- runExceptT $ postTx bridge (txEmpty, [])
                    res `shouldSatisfy` \case
                        Left (ErrPostTxNetworkUnreachable (ErrNetworkUnreachable _)) ->
                            True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

    describe "Submitting signed transactions"
        $ requireTestnet
        $ beforeAll startBridge $ afterAll closeBridge $ do
        it "empty tx fails (1)" $ \(_, bridge) -> do
            let signed = (txEmpty, [])
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has no inputs"
            runExceptT (postTx bridge signed) `shouldReturn` err

        it "empty tx fails (2)" $ \(_, bridge) -> do
            let signed = (txEmpty, [pkWitness])
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has no inputs"
            runExceptT (postTx bridge signed) `shouldReturn` err

        it "old tx fails" $ \(_, bridge) -> do
            let signed = (txNonEmpty, [pkWitness])
            let err = Left $ ErrPostTxProtocolFailure
                    "Failed to send to peers: Blockchain protocol error"
            runExceptT (postTx bridge signed) `shouldReturn` err

        it "tx fails - more inputs than witnesses" $ \(_, bridge) -> do
            let signed = (txNonEmpty, [])
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has more \
                    \inputs than witnesses"
            runExceptT (postTx bridge signed) `shouldReturn` err

        it "tx fails - more witnesses than inputs" $ \(_, bridge) -> do
            let signed = (txNonEmpty, [pkWitness, pkWitness])
            let err = Left $ ErrPostTxBadRequest
                    "Transaction failed verification: transaction has more \
                    \witnesses than inputs"
            runExceptT (postTx bridge signed) `shouldReturn` err
  where
    pkWitness :: TxWitness
    pkWitness = PublicKeyWitness "O\a\142a\166\180\SO\205\&3I8\160)\224F?\157\252\ACK\DC2\EOT\ESC\184\201\170\218\217\ETX\201\ESCn\SYN\206\179O\n\236\185\235T\163\190o\SI'r\228\241\150yL\218\NAK R2\162\211\144\209\129lr\225" $ Hash "Go%&7\248\149\194\202\231\210\143-\212f.\135\174\254\186\193^\212?\136\SO;\ACK\a\211\DC1\b\223\159\161\179&\189[\231\217\179\143JOW\194iv5\EMr\197\ETX\158p\DC4=\145\128\n/\255\NUL"

    txNonEmpty :: Tx
    txNonEmpty = Tx
        { inputs =
            [ TxIn
                { inputId = Hash "!s\CAN\255]e\252\184F\160\178\250:\190\DC2\250u\SOf\203\179\225\238+\244\162WvR\159\183\b"
                , inputIx = 0
                }
            ]
        , outputs =
            [ TxOut
                { address = Address "\130\216\CANXI\131X\FS`\218\181D\EOT{p\196\242\251\253\DC2\221\225\DC1\171\172\161;\172A\\\232\166\ETB\RSp\208\162\SOHX\RSX\FS%\245\158\195@\ENQ\225\212*\215\RSA\241;\245/\243\174s\143O\183\229V\175!\131\226\STXE\SUBAp\203\ETB\NUL\SUBm\US\131\DC4"
                , coin = Coin 933636862791
                }
            , TxOut
                { address = Address "\130\216\CANXI\131X\FS\227\213\&146.\160)\226\207\218\134@\\\149\166oq\CAN\DC1c\245\237\153]\235yO\162\SOHX\RSX\FS%\245\158\195@\ENQ\225\140\133\182\189A\194\206\228UZ\ENQ\168,\134\207\183\253\235\236t\171\STXE\SUBAp\203\ETB\NUL\SUB8\203F)"
                , coin = Coin 1227362560
                }
            ]
        }

    txEmpty :: Tx
    txEmpty = Tx [] []

    networkTip' = withExceptT unwrap . networkTip
      where
        unwrap (ErrNetworkTipNetworkUnreachable e) = e
        unwrap ErrNetworkTipNotFound = ErrNetworkUnreachable "no tip"
    newNetworkLayer =
        HttpBridge.newNetworkLayer port
    closeBridge (handle, _) = do
        cancel handle
        threadDelay 1000000
    startBridge = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", T.unpack (toText network)
                ]
                (return ())
                Inherit
            ]
        bridge <- newNetworkLayer
        threadDelay 1000000
        return (handle, bridge)

requireTestnet :: Spec -> Spec
requireTestnet prop = case network of
    Testnet -> prop
    Mainnet -> notDefinedFor network
    Staging -> notDefinedFor network
  where
    notDefinedFor n =
        it ("defined for NETWORK=testnet, not NETWORK=" ++ toStr n) False
    toStr
        = T.unpack . toText
