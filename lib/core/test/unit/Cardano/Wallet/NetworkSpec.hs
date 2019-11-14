{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LOContent (..), LogObject (..) )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.Wallet.DummyTarget.Primitive.Types
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , FollowAction (..)
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

import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrNetworkUnreachable mempty
        testShow $ ErrNetworkInvalid mempty
        testShow $ ErrNetworkTipNetworkUnreachable
            $ ErrNetworkUnreachable mempty
        testShow $ ErrNetworkTipNetworkUnreachable
            $ ErrNetworkInvalid mempty
        testShow ErrNetworkTipNotFound
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
            let rollback _slot = return $ Continue @()
            void $ race (threadDelay $ 10 * second)
                (follow mockNetworkLayer tr [] advance rollback getHeader)
            errors <- mapMaybe (unMsg . loContent) <$> readTVarIO tvar
            case length errors of
                x | x == 5 -> return ()
                  | otherwise -> expectationFailure
                        $ "we expected 4 errors to be logged, not " ++ show x
  where
    second = 1000*1000
    unMsg (LogMessage txt)
        | err `T.isPrefixOf` txt = Just txt
        | otherwise = Nothing
    unMsg _ = Nothing

    err = "Recoverable error following the chain:"

mockNetworkLayer :: NetworkLayer IO DummyTarget Block
mockNetworkLayer = NetworkLayer
    { nextBlocks = \_ -> error "no next blocks"
    , findIntersection = \_ -> error "no find intersection"
    , initCursor = \_ -> error "no init cursor"
    , cursorSlotId = \_ -> error "no cursor slot id"
    , networkTip = error "there is no network tip"
    , postTx = \_ -> error "the tx is not a thing that can be posted"
    , staticBlockchainParameters = error "static blockchain params don't exist"
    , stakeDistribution = error "stake? no."
    }

testShow :: Show a => a -> Spec
testShow a = it (show a) True
