{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Byron.NetworkSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet.Byron.Compatibility
    ( NodeVersionData )
import Cardano.Wallet.Byron.Launch
    ( withCardanoNode )
import Cardano.Wallet.Byron.Network
    ( NetworkLayerLog (..), withNetworkLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , TxParameters (..)
    )
import Control.Retry
    ( constantDelay, limitRetries, recoverAll )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( set )
import Data.Generics.Labels
    ()
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( withLogging )

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "getTxParameters" $ do
    it "Correct values are queried" $ do
        withTestNode $ \np sock vData -> withLogging $ \(tr, getLogs) -> do
            -- Initial TxParameters for NetworkLayer are all zero
            let np' = np &
                    (#protocolParameters . #txParameters) `set` zeroTxParameters
            withNetworkLayer tr np' sock vData $ \nl -> do
                -- After a short while, the network layer should have gotten
                -- protocol parameters from the node, and they should reflect
                -- the genesis block configuration.
                let retryPolicy = constantDelay 1_000_000 <> limitRetries 10
                recoverAll retryPolicy $ const $
                    getProtocolParameters nl `shouldReturn`
                        protocolParameters np
            -- Parameters update should be logged exactly once.
            msg <- mapMaybe isMsgProtocolParams <$> getLogs
            msg `shouldBe` [protocolParameters np]

withTestNode
    :: (NetworkParameters -> FilePath -> NodeVersionData -> IO a)
    -> IO a
withTestNode action = withCardanoNode nullTracer $(getTestData) Error $
    \sock _block0 (np, vData) -> action np sock vData

isMsgProtocolParams :: NetworkLayerLog -> Maybe ProtocolParameters
isMsgProtocolParams (MsgProtocolParameters pp) = Just pp
isMsgProtocolParams _ = Nothing

zeroTxParameters :: TxParameters
zeroTxParameters = TxParameters
    (LinearFee (Quantity 0) (Quantity 0) (Quantity 0))
    (Quantity 0)
