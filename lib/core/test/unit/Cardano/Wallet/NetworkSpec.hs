{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Wallet.DummyTarget.Primitive.Types
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrGetBlock (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , FollowAction (..)
    , FollowLog (..)
    , NetworkLayer (..)
    , follow
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..), Hash (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.STM.TVar
    ( newTVarIO, readTVarIO )
import Control.Monad
    ( void )
import Data.Maybe
    ( mapMaybe )
import Test.Hspec
    ( Spec, describe, expectationFailure, it )

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrNetworkUnreachable mempty
        testShow $ ErrNetworkInvalid mempty
        testShow $ ErrCurrentNodeTipNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrCurrentNodeTipNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow ErrCurrentNodeTipNotFound
        testShow $ ErrGetBlockNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrGetBlockNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow $ ErrGetBlockNotFound $ Hash mempty
        testShow $ ErrPostTxNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrPostTxNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow $ ErrPostTxBadRequest mempty
        testShow $ ErrPostTxProtocolFailure mempty

    followSpec

followSpec :: Spec
followSpec =
    describe "follow" $ do
        it "retries on exceptions, but not too often" $ do
            tvar <- newTVarIO []
            let tr = traceInTVarIO tvar
            let getHeader = header
            let advance _blocks _h = return $ Continue @()
            void $ race
                (threadDelay $ 10 * second)
                (follow mockNetworkLayer tr [] advance getHeader)
            errors <- mapMaybe matchRecoverableErrors <$> readTVarIO tvar
            case length errors of
                x | x == 5 -> return ()
                  | otherwise -> expectationFailure
                        $ "we expected 4 errors to be logged, not " ++ show x
  where
    second = 1000*1000
    matchRecoverableErrors = \case
        e@MsgUnhandledException{} -> Just e
        _ -> Nothing

mockNetworkLayer :: NetworkLayer IO DummyTarget Block
mockNetworkLayer = NetworkLayer
    { nextBlocks = \_ -> error "no next blocks"
    , findIntersection = \_ -> error "no find intersection"
    , initCursor = \_ -> error "no init cursor"
    , cursorSlotId = \_ -> error "no cursor slot id"
    , currentNodeTip = error "there is no current node tip"
    , postTx = \_ -> error "the tx is not a thing that can be posted"
    , staticBlockchainParameters = error "static blockchain params don't exist"
    , stakeDistribution = error "stake? no."
    , getAccountBalance = error "it is empty"
    }

testShow :: Show a => a -> Spec
testShow a = it (show a) True
